{-# language GADTs #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language RecursiveDo #-}
{-# language StandaloneDeriving #-}
{-# language ExistentialQuantification, FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language DeriveLift #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# language TemplateHaskell #-}

{-# OPTIONS_GHC -fplugin=LiftPlugin #-}

module LMS (module LMS, Pure(..)) where
-- This module implements a function which can be specialised to a
-- interpreter or a compiler.
import Unsafe.Coerce

import qualified Control.Applicative as A
import Control.Lens.Cons (Cons, uncons)
import Data.List (intersect, union)
import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.Semigroup ((<>))

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax
import Prelude hiding (Applicative(..))
import Data.Functor.Identity
import GHC.Exts (Any)

import LiftPlugin

newtype Code a = Code (Q (TExp a))

runCode (Code a) = a

-- LMS start

class Pure r => Ops r where
  type DynVal r
  -- Structural overloading
  _if :: r Bool -> r a -> r a -> r a
  _uncons :: Cons c c a a => r c -> (r a -> r c -> r res) -> r res -> r res
  _fix :: (r a -> r a) -> r a
  _lam :: (r a -> r b) -> r (a -> b)
  _let :: r a -> (r a -> r b) -> r b
  _elim_prod :: r (a, b) -> (r a -> r b -> r x) -> r x

  -- Pointless lifting
  {-
  -}
  -- Plugin only handles functions so far
  _nothing :: r (Maybe a)

  _cast :: DynVal r -> r a
  _forget :: r a -> DynVal r

  (<*>) :: r (a -> b) -> r a -> r b

_eq :: (Ops r, Eq a) => r a -> r a -> r Bool
_comp :: Ops r => r (b -> c) -> r (a -> b) -> r (a -> c)
_just :: Ops r => r a -> r (Maybe a)
-- Otherwise we need any annoying `Lift` constraint on `a`.
_tup :: Ops r => r a -> r b -> r (a, b)
_fst :: Ops r => r (a, b) -> r a
_snd :: Ops r => r (a, b) -> r b
_fmap :: (Ops r, Functor f) => r (a -> b) -> r (f a) -> r (f b)
_const_l :: Ops r => r a -> r b -> r a
_const_r :: Ops r => r a -> r b -> r b
_cons :: Ops r => r a -> r ([a] -> [a])
_bind :: (Ops r, Monad m) => r (m a) -> r (a -> m b) -> r (m b)



_eq a b = pure (==) <*> a <*> b
_comp a b = pure (.) <*> a <*> b
_just a = pure Just <*> a
--_nothing = pure Nothing
_tup a b = pure (,) <*> a <*> b
_fst r = pure fst <*> r
_snd r = pure snd <*> r
_fmap f b = pure fmap <*> f <*> b

const_l a b = a
const_r a b = b

_const_l a b = pure const_l <*> a <*> b
_const_r a b = pure const_r <*> a <*> b
_cons ra = pure (:) <*> ra
_bind ra rf = pure (>>=) <*> ra <*> rf

infixl 4 <*>

fix :: (a -> a) -> a
fix f = let x = f x in x


instance Ops Code where
  type DynVal Code = ExpQ
  _if (Code a) (Code b) (Code c) = Code [|| if $$a then $$b else $$c ||]
  _uncons (Code a) f (Code r) =
    Code [|| case uncons $$a of
              Just (c, r) -> $$(runCode $ f (Code [||c ||]) (Code [|| r ||]))
              Nothing -> $$r ||]

  _let (Code ra) body =
    Code [|| let x = $$ra in $$(runCode $ body (Code [|| x ||])) ||]

  _fix f = Code [|| fix (\a -> $$(runCode $ f (Code [||a||]))) ||]

  _lam f = Code $ [|| \a ->  $$(runCode $ f (Code [|| a ||]))  ||]

  _elim_prod (Code p) f = Code [|| case $$p of
                                      (a, b) -> $$(runCode $ f (Code ([|| a ||])) (Code [|| b ||])) ||]

  _nothing = Code [|| Nothing ||]

  _cast = Code . unsafeTExpCoerce
  _forget (Code a) = unTypeQ a

  (Code f) <*> (Code a) = Code [|| $$f $$a ||]

instance Pure Code where
  pure = Code . unsafeTExpCoerce . lift


instance Ops Identity where
  type DynVal Identity = Any
  _if (Identity b) (Identity c1) (Identity c2) = Identity (if b then c1 else c2)

  _uncons (Identity a) f fallBack =
    case uncons a of
      Just (c, r) -> f (Identity c) (Identity r)
      Nothing -> fallBack

  _let (Identity a) body =
    let x = a
    in body (Identity x)




  _fix = fix
  _lam f = Identity (\a -> runIdentity (f (Identity a)))

  _elim_prod (Identity (a,b)) f = f (Identity a) (Identity b)

  _nothing = Identity Nothing

  _cast = unsafeCoerce
  _forget = unsafeCoerce

  (<*>) (Identity a1) (Identity a2) = Identity (a1 a2)

instance Pure Identity where
  pure = Identity



{-
curry_c :: Code (a -> b) -> Code a -> Code b
curry_c f a = Code [|| $$(f) $$(a) ||]

uncurry_c :: (Code a -> Code b) -> Code (a -> b)
uncurry_c f = [|| \a -> $$(f [|| a ||]) ||]
-}

data CodeOps a = CodeOps (forall r . Ops r => r a)

pureCode :: CodeOps a -> a
pureCode (CodeOps a) = runIdentity a

genCode :: CodeOps a -> Code a
genCode (CodeOps a) = a

lmsCode :: Ops r => CodeOps a -> r a
lmsCode (CodeOps a) = a

data CFG ann var c a where
  Pure :: ann -> CodeOps a -> CFG ann var c a
  Bot :: ann -> CFG ann var c a
  Or :: ann -> CFG ann var c a -> CFG ann var c a -> CFG ann var c a
  Empty :: ann -> CFG ann var c ()
  Char :: ann -> c -> CFG ann var c c
  Seq :: ann -> CFG ann var c (a -> b) -> CFG ann var c a -> CFG ann var c b
  NotNull :: ann -> CFG ann var c a -> CFG ann var c a
  Var :: ann -> var c a -> CFG ann var c a
  Mu :: ann -> (var c a -> CFG ann var c a) -> CFG ann var c a
  Map :: ann -> CodeOps (a -> b) -> CFG ann var c a -> CFG ann var c b

data CFG' ann c a where
  Pure' :: ann -> CodeOps a -> CFG' ann c a
  Bot' :: ann -> CFG' ann c a
  Or' :: ann -> CFG' ann c a -> CFG' ann c a -> CFG' ann c a
  Empty' :: ann -> CFG' ann c ()
  Char' :: ann -> c -> CFG' ann c c
  Seq' :: ann -> CFG' ann c (a -> b) -> CFG' ann c a -> CFG' ann c b
  NotNull' :: ann -> CFG' ann c a -> CFG' ann c a
  Var' :: ann -> Int -> CFG' ann c a
  Mu' :: ann -> Int -> CFG' ann c a -> CFG' ann c a
  Map' :: ann -> CodeOps (a -> b) -> CFG' ann c a -> CFG' ann c b

lower :: CFG a Var c d -> CFG' a c d
lower = go [0..]
  where
    go :: [Int] -> CFG a Var c d -> CFG' a c d
    go _ (Pure a b) = Pure' a b
    go _ (Bot a) = Bot' a
    go supply (Or a b c) = Or' a (go supply b) (go supply c)
    go _ (Empty a) = Empty' a
    go _ (Char a b) = Char' a b
    go supply (Seq a b c) = Seq' a (go supply b) (go supply c)
    go supply (NotNull a b) = NotNull' a (go supply b)
    go _ (Var a (MkVar n)) = Var' a n
    go (s:supply) (Mu a f) = Mu' a s (go supply $ f (MkVar s))
    go [] _ = error "impossible"
    go supply (Map a b c) = Map' a b (go supply c)

cfgAnn :: CFG' a c d -> a
cfgAnn e =
  case e of
    Pure' a _ -> a
    Bot' a -> a
    Or' a _ _ -> a
    Empty' a -> a
    Char' a _ -> a
    Seq' a _ _ -> a
    NotNull' a _ -> a
    Var' a _ -> a
    Mu' a _ _ -> a
    Map' a _ _ -> a

setCfgAnn :: a -> CFG' a c d -> CFG' a c d
setCfgAnn a e =
  case e of
    Pure' _ b -> Pure' a b
    Bot' _ -> Bot' a
    Or' _ b c -> Or' a b c
    Empty' _ -> Empty' a
    Char' _ b -> Char' a b
    Seq' _ b c -> Seq' a b c
    NotNull' _ b -> NotNull' a b
    Var' _ b -> Var' a b
    Mu' _ b c -> Mu' a b c
    Map' _ b c -> Map' a b c

data Ty c
  = Ty
  { _null :: Bool
  , _first :: [c]
  , _followLast :: [c]
  , _guarded :: Bool
  } deriving (Eq, Show, Functor, Foldable, Traversable, Lift)

(#) :: Eq c => Ty c -> Ty c -> Bool
(#) t t' = not (_null t && _null t') && null (_first t `intersect` _first t')

(.*.) :: Eq c => Ty c -> Ty c -> Bool
(.*.) t t' = null (_followLast t `intersect` _first t') && not (_null t)

data TyError c where
  NotDisjoint :: ShowCFG c a -> ShowCFG c a -> TyError c
  Ambiguous :: ShowCFG c a -> ShowCFG c b -> TyError c
  Null :: ShowCFG c a -> TyError c
  Guarded :: (Ty c) -> ShowCFG c a -> TyError c
  NotGuarded :: ShowCFG c a -> TyError c
deriving instance Show c => Show (TyError c)

newtype Var c a = MkVar Int
newtype THVar c a = MkTHVar ExpQ
data ShowCFG c a = forall ann. ShowCFG (CFG' ann c a)

instance Show c => Show (ShowCFG c a) where
  show (ShowCFG a) = "(" ++ showCFG a ++ ")"

typeOf :: (Show c, Eq c) => CFG' () c a -> Either (TyError c) (CFG' (Ty c) c a)
typeOf = go []
  where
    go
      :: (Show c, Eq c)
      => [Ty c]
      -> CFG' () c a
      -> Either (TyError c) (CFG' (Ty c) c a)
    go _ (Pure' () a) =
      let
        t =
          Ty
          { _null = True
          , _first = []
          , _followLast = []
          , _guarded = True
          }
      in A.pure $ Pure' t a
    go ctxt (NotNull' () g) = do
      g' <- go ctxt g
      let t = cfgAnn g'
      if _null t
        then Left $ Null (ShowCFG g)
        else
        let
          t' =
            Ty
            { _null = False
            , _first = _first t
            , _followLast = _followLast t
            , _guarded = _guarded t
            }
        in A.pure $ NotNull' t' g'
    go _ (Bot' ()) =
      let
        t =
          Ty
          { _null = False
          , _first = []
          , _followLast = []
          , _guarded = False
          }
      in A.pure (Bot' t)
    go ctxt (Or' () f g) = do
      f' <- go ctxt f
      let t = cfgAnn f'
      g' <- go ctxt g
      let t' = cfgAnn g'
      if not (t # t')
        then Left $ NotDisjoint (ShowCFG f) (ShowCFG g)
        else
        let
          t'' =
            Ty
            { _null = _null t || _null t'
            , _first = _first t `union` _first t'
            , _followLast = _followLast t `union` _followLast t'
            , _guarded = _guarded t && _guarded t'
            }
        in A.pure (Or' t'' f' g')
    go _ (Empty' ()) =
      let
        t =
          Ty
          { _null = True
          , _first = []
          , _followLast = []
          , _guarded = True
          }
      in A.pure (Empty' t)
    go ctxt (Seq' () _ (Bot' ())) = go ctxt (Bot' ())
    go ctxt (Seq' () (Bot' ()) _) = go ctxt (Bot' ())
    go ctxt (Seq' () a b) = do
      a' <- go ctxt a
      let t = cfgAnn a'
      b' <- go ctxt b
      let t' = cfgAnn b'
      if not (t .*. t')
        then Left $ Ambiguous (ShowCFG a) (ShowCFG b)
        else
        let
          t'' =
            -- removed some redundancy implied by t .*. t'
            Ty
            { _null = False
            , _first = _first t
            , _followLast =
                _followLast t' `union`
                (if _null t' then _first t' `union` _followLast t else [])
            , _guarded = _guarded t
            }
        in A.pure (Seq' t'' a' b')
    go _ (Char' () c) =
      let
        t =
          Ty
          { _null = False
          , _first = [c]
          , _followLast = []
          , _guarded = True
          }
      in A.pure (Char' t c)
    go ctxt (Map' () f a) = do
      a' <- go ctxt a
      A.pure (Map' (cfgAnn a') f a')
    go ctxt (Mu' () s f) = do
      -- Binding order mistake was repeated
      res <- fix (\ty -> go (ctxt ++ [cfgAnn ty]) f)
      let t = cfgAnn res
      if _guarded t
        then A.pure (Mu' t s res)
        else Left $ NotGuarded (ShowCFG $ Mu' () s f)
      where
        fix :: (Show b, Eq b) => (CFG' (Ty b) b c -> Either (TyError b) (CFG' (Ty b) b c)) -> Either (TyError b) (CFG' (Ty b) b c)
        fix f = inner =<< typeOf (Bot' ())
          where
            inner input = do
              case f input of
                Left{} -> A.pure input
                Right output ->
                  if cfgAnn input == cfgAnn output
                    then A.pure output
                    else inner output
            {-
            inner input = do
              output <- f input
              if cfgAnn input == cfgAnn output
                then A.pure output
                else inner output
-}
    -- My interpretation of section 3 is that guardedness doesn't matter
    -- for variable lookup
    go ctxt (Var' () n) = A.pure $ Var' (ctxt !! n) n

showCFG :: Show c => CFG' x c a -> String
showCFG Pure'{} = "value"
showCFG Bot'{} = "_|_"
showCFG (Or' _ a b) = "(" ++ showCFG a ++ ") \\/ (" ++ showCFG b ++ ")"
showCFG Empty'{} = "e"
showCFG (Char' _ c) = show c
showCFG (Seq' _ a b) = "(" ++ showCFG a ++ ") . ("  ++ showCFG b ++ ")"
showCFG (NotNull' _ a) = "[" ++ showCFG a ++ "]"
showCFG (Var' _ n) = "var" ++ show n
showCFG (Mu' _ s f) = "mu var" ++ show s ++ ". " ++ showCFG f
showCFG (Map' _ _ a) = showCFG a

data ParseError c
  = Unexpected c [c]
  | UnexpectedEof [c]
  | Bottom
  deriving Show

parse :: Eq c => CFG' (Ty c) c a -> [c] -> Either (ParseError c) ([c], a)
parse = go []
  where
    go :: Eq c => [[c] -> Maybe ([c], ())] -> CFG' (Ty c) c a -> [c] -> Either (ParseError c) ([c], a)
    go ctxt cfg str =
      case cfg of
        Pure' _ a -> Right (str, pureCode a)
        Bot'{} -> Left Bottom
        Or' ty a b ->
          let
            ta = cfgAnn a
            tb = cfgAnn b
          in
            case str of
              c:_
                | c `elem` _first ta -> go ctxt a str
                | c `elem` _first tb -> go ctxt b str
              _
                | _null ta -> go ctxt a str
                | _null tb -> go ctxt b str
                | otherwise ->
                    Left $ case str of
                      c : _ -> Unexpected c $ _first ta ++ _first tb
                      [] -> UnexpectedEof $ _first ta ++ _first tb
        Empty'{} -> A.pure (str, ())
        Char' ty c'
          | c:cs <- str, c == c' -> A.pure (cs, c')
          | otherwise ->
              Left $ case str of
                c : _ -> Unexpected c [c']
                [] -> UnexpectedEof [c']
        Seq' ty a b -> do
          (str', a') <- go ctxt a str
          (str'', b') <- go ctxt b str'
          A.pure (str'', a' b')
        NotNull' _ a -> go ctxt a str
        Var' ty n -> unsafeCoerce (ctxt !! n) str
        Mu' ty s f ->
          let
            f' = go (ctxt ++ [unsafeCoerce f']) f
          in
            f' str
        Map' ty f a
          | c:_ <- str, c `elem` _first ty -> fmap (pureCode f) <$> go ctxt a str
          | _null ty -> fmap (pureCode f) <$> go ctxt a str
          | otherwise ->
              let ta = cfgAnn a in
              Left $ case str of
                c : _ -> Unexpected c $ _first ta
                [] -> UnexpectedEof $ _first ta

data IR c a where
  IR_Pure :: Ty c -> CodeOps a -> IR c a
  IR_Bot :: Ty c -> IR c a
  IR_Or :: Ty c -> NonEmpty (IR c a) -> IR c a
  IR_Empty :: Ty c -> IR c ()
  IR_Char :: Ty c -> c -> IR c c
  IR_Seq :: Ty c -> IR c (a -> b) -> IR c a -> IR c b
  IR_NotNull :: Ty c -> IR c a -> IR c a
  IR_Var :: Ty c -> Int -> IR c a
  IR_Mu :: Ty c -> IR c a -> IR c a
  IR_Map :: Ty c -> (CodeOps (a -> b)) -> IR c a -> IR c b

ir_str :: IR c a -> String
ir_str e =
  case e of
    IR_Pure a _ -> "pure"
    IR_Bot a -> "bot"
    IR_Or a _ -> "or"
    IR_Empty a -> "empty"
    IR_Char a _ -> "char"
    IR_Seq a _ _ -> "seq"
    IR_NotNull a _ -> "notnull"
    IR_Var a _ -> "var"
    IR_Mu a _ -> "MU"
    IR_Map a _ _ -> "MAP"

irAnn :: IR c d -> Ty c
irAnn e =
  case e of
    IR_Pure a _ -> a
    IR_Bot a -> a
    IR_Or a _ -> a
    IR_Empty a -> a
    IR_Char a _ -> a
    IR_Seq a _ _ -> a
    IR_NotNull a _ -> a
    IR_Var a _ -> a
    IR_Mu a _ -> a
    IR_Map a _ _ -> a

toIR :: CFG' (Ty c) c a -> IR c a
toIR e = case e of
  Pure' t a -> IR_Pure t a
  Bot' t ->  IR_Bot t
  Or' t a b -> IR_Or t (fmap toIR (ors a <> ors b))
  Empty' t -> IR_Empty t
  Char' t c -> IR_Char t c
  Seq' t a b -> IR_Seq t (toIR a) (toIR b)
  NotNull' t a -> IR_NotNull t (toIR a)
  Var' t v -> IR_Var t v
  Mu' t s f -> IR_Mu t (toIR f)
  Map' t (CodeOps f) (Map' _ (CodeOps g) a) -> toIR (Map' t (CodeOps (_comp f g)) a)
  Map' t f a -> IR_Map t f (toIR a)
  where
    ors (Or' _ a b) = ors a <> ors b
    ors a = A.pure a

type Context r c = [DynVal r]

compile :: (Lift a, Lift c, Eq c, Show c, Cons s s c c)
        => CFG () Var c a -> Q (TExp (s -> Maybe (s, a)))
compile = runCode . makeParser_lms

interpret :: (Lift a, Lift c, Eq c, Show c, Cons s s c c)
              => CFG () Var c a -> s -> Maybe (s, a)
interpret = runIdentity . makeParser_lms

makeParser_lms
  :: ( Lift a
     , Lift c, Eq c, Show c
     , Cons s s c c
     , Ops r
     )
  => CFG () Var c a
  -> r (s -> Maybe (s, a))
makeParser_lms =
  go_lms [] . either (error . show) (toIR) . typeOf . lower

elem_lms
  :: (Lift a, Eq a, Ops r)
  => r a -- ^ Needle
  -> [a] -- ^ Haystack
  -> r (a -> res) -- ^ If needle is found
  -> r res -- ^ If needle isn't found
  -> r res
elem_lms c r found notFound =
  foldr
    (\a b -> _if (_eq (pure a) c) (found <*> c) b)
    notFound
    r

go_lms
  :: forall s c a r
   . (Lift c, Eq c, Cons s s c c, Ops r)
  => Context r c
  -> IR c a
  -> r (s -> Maybe (s, a))
go_lms context e =
  case e of
    IR_Pure _ (lmsCode -> a) -> _lam $ \cs -> _just (_tup cs a)
    IR_Bot _ -> _lam $ \_ -> _nothing
    IR_Char _ c' ->
      _lam $
         \x -> _uncons x
                  (\c cs -> _if (_eq (pure c') c) (_just (_tup cs c)) (_nothing))
                  (_nothing)
    IR_Or _ bs -> ir_ors_lms context bs
    IR_Empty _ -> _lam $ \x -> _just (_tup x (pure ()))
    IR_Seq _ a b ->
      _lam $ \x ->
         _bind (go_lms context a <*> x)
               (_lam $ \r -> _elim_prod r (\x' a' ->
                              _bind (go_lms context b <*> x')
                                    (_lam $ \r' -> _elim_prod r' (\x'' b' ->
                                                      _just (_tup x'' (a' <*> b'))))))
    IR_NotNull _ a -> go_lms context a
    IR_Map ty (lmsCode -> f) ta ->
        let r = _first ty
        in
          _lam $ \str ->
            _let (_lam (\mr -> _fmap (_lam $ \ab -> _fmap f ab) mr) `_comp` go_lms context ta) $ \success ->
            _let (_lam $ \str' -> _if (pure $ _null ty) (success <*> str') (_nothing)) $ \fallThrough ->
                _uncons str (\c _ -> elem_lms c r (_lam $ \_ -> success <*> str) (fallThrough <*> str)) (fallThrough <*> str)

    IR_Var _ n ->
      _cast (context !! n)

    IR_Mu _ f ->
      _fix (\x -> go_lms (context ++ [ _forget x ] ) f)

_const :: Ops r => r a -> r (b -> a)
_const r = _lam $ (\_ -> r)

ir_ors_lms
  :: forall s c a r
  . (Lift c, Eq c, Cons s s c c, Ops r)
  => Context r c
  -> NonEmpty (IR c a)
  -> r (s -> Maybe (s, a))
ir_ors_lms context as =
  let
    fallThrough =
      case filter (_null . irAnn) (toList as) of
        [] -> _lam $ \_ -> _nothing
        a : _ -> go_lms context a
  in
    _let fallThrough $ \ff -> _lam $
      \str -> _uncons str (\c _ -> foldr (comb c) ff as <*> str) (ff <*> str)
  where
    comb
      :: (Lift c, Eq c, Cons s s c c, Ops r)
      => r c
      -> IR c a
      -> r (s -> Maybe (s, a))
      -> r (s -> Maybe (s, a))
    comb c ta f2 =
      let
        r = _first (irAnn ta)
      in
        elem_lms c r (_const (go_lms context ta))f2
