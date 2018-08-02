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

module LMS where
-- This module implements a function which can be specialised to a
-- interpreter or a compiler.
import Unsafe.Coerce

import Control.Applicative ((<|>), liftA2, liftA)
import qualified Control.Applicative as A
import Control.Lens.Cons (Cons, uncons)
import Control.Monad ((<=<), unless, when)
import Data.Either (fromRight)
import Data.List (intersect, union, foldl')
import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.Semigroup ((<>))
import Data.Traversable (for)

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax
import Prelude hiding (Applicative(..))
import Data.Functor.Identity
import GHC.Exts (Any)

newtype Code a = Code (Q (TExp a))

runCode (Code a) = a

-- LMS start

class Ops r where
  type DynVal r
  _if :: r Bool -> r a -> r a -> r a
  --_caseString :: r ByteString -> r a -> r a -> r a
  _uncons :: Cons c c a a => r c -> (r a -> r c -> r res) -> r res -> r res
  _fix :: (r a -> r a) -> r a
  _lam :: (r a -> r b) -> r (a -> b)
  _let :: r a -> (r a -> r b) -> r b

  -- Pointless lifting
  _eq :: Eq a => r a -> r a -> r Bool
  _comp :: r (b -> c) -> r (a -> b) -> r (a -> c)
  _just :: r a -> r (Maybe a)
  -- Otherwise we need any annoying `Lift` constraint on `a`.
  _nothing :: r (Maybe a)
  _tup :: r a -> r b -> r (a, b)
  _fst :: r (a, b) -> r a
  _snd :: r (a, b) -> r b
  _fmap :: Functor f => r (a -> b) -> r (f a) -> r (f b)
  _const_l :: r a -> r b -> r a
  _const_r :: r a -> r b -> r b
  _cons :: r a -> r ([a] -> [a])

  _bind :: Monad m => r (m a) -> r (a -> m b) -> r (m b)

  _cast :: DynVal r -> r a
  _forget :: r a -> DynVal r


  pure :: Lift a => a -> r a
  (<*>) :: r (a -> b) -> r a -> r b

infixl 4 <*>

fix :: (a -> a) -> a
fix f = let x = f x in x


instance Ops Code where
  type DynVal Code = ExpQ
  _if (Code a) (Code b) (Code c) = Code [|| if $$a then $$b else $$c ||]
                                                   {-
  _caseString (Code a) (Code b) (Code c) =
    Code [|| case $$a of
                "" -> $$b
                _  -> $$c ||]
                -}

  _uncons (Code a) f (Code r) =
    Code [|| case uncons $$a of
              Just (c, r) -> $$(runCode $ f (Code [||c ||]) (Code [|| r ||]))
              Nothing -> $$r ||]

  _let (Code ra) body =
    Code [|| let x = $$ra in $$(runCode $ body (Code [|| x ||])) ||]

  _fix f = Code [|| fix (\a -> $$(runCode $ f (Code [||a||]))) ||]

  _lam f = Code $ [|| \a ->  $$(runCode $ f (Code [|| a ||]))  ||]


  -- Simple liftings
  _eq (Code e1) (Code e2) = Code [|| $$e1 == $$e2 ||]
  _comp (Code e1) (Code e2) = Code [|| $$e1 . $$e2 ||]
  _bind (Code e1) (Code e2) = Code [|| $$e1 >>= $$e2 ||]
  _just (Code v) = Code [|| Just $$v ||]
  _nothing = Code [|| Nothing ||]
  _tup (Code a) (Code b) = Code [|| ($$a, $$b) ||]
  _fst (Code a) = Code [|| fst $$a ||]
  _snd (Code a) = Code [|| snd $$a ||]
  _fmap (Code f) (Code a) = Code [|| fmap $$f $$a ||]
  _const_l f1 _ = f1
  _const_r _ f2 = f2
  _cons (Code r) = Code [|| \rs -> $$r : rs ||]

  _cast = Code . unsafeTExpCoerce
  _forget (Code a) = unTypeQ a

  pure = Code . unsafeTExpCoerce . lift
  (Code f) <*> (Code a) = Code [|| $$f $$a ||]


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

  _eq = liftA2 (==)
  _comp = liftA2 (.)
  _just = liftA Just
  _nothing = A.pure Nothing
  _tup = liftA2 (,)
  _fst = liftA fst
  _snd = liftA snd
  _fmap = liftA2 fmap
  _bind = liftA2 (>>=)
  _const_l = liftA2 (\a b -> a)
  _const_r = liftA2 (\a b -> b)
  _cons (Identity r) = Identity (\rs -> r:rs)

  _cast = unsafeCoerce
  _forget = unsafeCoerce

  pure = Identity
  (<*>) (Identity a1) (Identity a2) = Identity (a1 a2)



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

cfgAnn :: CFG a b c d -> a
cfgAnn e =
  case e of
    Pure a _ -> a
    Bot a -> a
    Or a _ _ -> a
    Empty a -> a
    Char a _ -> a
    Seq a _ _ -> a
    NotNull a _ -> a
    Var a _ -> a
    Mu a _ -> a
    Map a _ _ -> a

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
  Guarded :: ShowCFG c a -> TyError c
  NotGuarded :: ShowCFG c a -> TyError c
deriving instance Show c => Show (TyError c)

newtype Var c a = MkVar Int
newtype THVar c a = MkTHVar ExpQ
newtype ShowCFG c a = ShowCFG (CFG () Var c a)

instance Show c => Show (ShowCFG c a) where
  show (ShowCFG a) = "(" ++ showCFG a ++ ")"

{-# inline typeOf #-}
typeOf :: (Show c, Eq c) => CFG () Var c a -> Either (TyError c) (CFG (Ty c) Var c a)
typeOf = go [0..] [] False
  where
    go :: (Show c, Eq c) => [Int] -> [Ty c] -> Bool -> CFG () Var c a -> Either (TyError c) (CFG (Ty c) Var c a)
    go supply ctxt allowGuarded (Pure () a) =
      let
        t =
          Ty
          { _null = True
          , _first = []
          , _followLast = []
          , _guarded = True
          }
      in A.pure $ Pure t a
    go supply ctxt allowGuarded (NotNull () g) = do
      g' <- go supply ctxt allowGuarded g
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
        in A.pure $ NotNull t' g'
    go supply ctxt allowGuarded (Bot ()) =
      let
        t = Ty { _null = False, _first = [], _followLast = [], _guarded = True }
      in A.pure (Bot t)
    go supply ctxt allowGuarded (Or () f g) = do
      f' <- go supply ctxt allowGuarded f
      let t = cfgAnn f'
      g' <- go supply ctxt allowGuarded g
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
        in A.pure (Or t'' f' g')
    go supply ctxt allowGuarded (Empty ()) =
      let
        t = Ty { _null = True, _first = [], _followLast = [], _guarded = True }
      in A.pure (Empty t)
    go supply ctxt allowGuarded (Seq () a (Bot ())) = go supply ctxt allowGuarded (Bot ())
    go supply ctxt allowGuarded (Seq () a b) = do
      a' <- go supply ctxt allowGuarded a
      let t = cfgAnn a'
      b' <- go supply ctxt (not $ _null t) b
      let t' = cfgAnn b'
      if not (t .*. t')
        then Left $ Ambiguous (ShowCFG a) (ShowCFG b)
        else
        let
          t'' =
            Ty
            { _null = _null t && _null t'
            , _first = _first t `union` (if _null t then _first t' else [])
            , _followLast = _followLast t' `union` (if _null t' then _first t' `union` _followLast t else [])
            , _guarded = _guarded t
            }
        in A.pure (Seq t'' a' b')
    go supply ctxt allowGuarded (Char () c) =
      let
        t = Ty { _null = False, _first = [c], _followLast = [], _guarded = True }
      in A.pure (Char t c)
    go supply ctxt allowGuarded (Map () f a) = do
      a' <- go supply ctxt allowGuarded a
      let t = cfgAnn a'
      A.pure (Map t f a')
    go (s:supply) ctxt allowGuarded (Mu () f) = do
      res <- fix (\ty -> go supply (cfgAnn ty:ctxt) allowGuarded (f $ MkVar s))
      let t = cfgAnn res
      if _guarded t
        then A.pure (Mu t $ fromRight (error "impossible") . go supply (t:ctxt) allowGuarded . f)
        else Left $ NotGuarded (ShowCFG $ Mu () f)
      where
        fix f = inner =<< typeOf (Bot ())
          where
            inner input = do
              output <- f input
              if cfgAnn input == cfgAnn output
                then A.pure output
                else inner output
    go [] _ _ (Mu _ _) = error "impossible"
    go supply ctxt allowGuarded a@(Var () (MkVar n)) =
      let t = ctxt !! n in
      if not allowGuarded && _guarded t
      then Left $ Guarded (ShowCFG a)
      else A.pure $ Var t (MkVar n)

showCFG :: Show c => CFG x Var c a -> String
showCFG = go [0..]
  where
    go :: Show c => [Int] -> CFG x Var c a -> String
    go supply Pure{} = "value"
    go supply Bot{} = "_|_"
    go supply (Or _ a b) = "(" ++ go supply a ++ ") \\/ (" ++ go supply b ++ ")"
    go supply Empty{} = "e"
    go supply (Char _ c) = show c
    go supply (Seq _ a b) = "(" ++ go supply a ++ ") . ("  ++ go supply b ++ ")"
    go supply (NotNull _ a) = "[" ++ go supply a ++ "]"
    go supply (Var _ (MkVar n)) = "var" ++ show n
    go (s:supply) (Mu _ f) =
        "mu var" ++ show s ++ ". " ++ go supply (f $ MkVar s)
    go [] (Mu _ f) = error "impossible"
    go supply (Map _ _ a) = go supply a

data ParseError c
  = Unexpected c [c]
  | UnexpectedEof [c]
  | Bottom
  deriving Show

parse :: Eq c => CFG (Ty c) Var c a -> [c] -> Either (ParseError c) ([c], a)
parse = go [0..] []
  where
    go :: Eq c => [Int] -> [[c] -> Maybe ([c], ())] -> CFG (Ty c) Var c a -> [c] -> Either (ParseError c) ([c], a)
    go supply ctxt cfg str =
      case cfg of
        Pure _ a -> Right (str, pureCode a)
        Bot{} -> Left Bottom
        Or ty a b ->
          let
            ta = cfgAnn a
            tb = cfgAnn b
          in
            case str of
              c:_
                | c `elem` _first ta -> go supply ctxt a str
                | c `elem` _first tb -> go supply ctxt b str
              _
                | _null ta -> go supply ctxt a str
                | _null tb -> go supply ctxt b str
                | otherwise ->
                    Left $ case str of
                      c : _ -> Unexpected c $ _first ta ++ _first tb
                      [] -> UnexpectedEof $ _first ta ++ _first tb
        Empty{} -> A.pure (str, ())
        Char ty c'
          | c:cs <- str, c == c' -> A.pure (cs, c')
          | otherwise ->
              Left $ case str of
                c : _ -> Unexpected c [c']
                [] -> UnexpectedEof [c']
        Seq ty a b -> do
          (str', a') <- go supply ctxt a str
          (str'', b') <- go supply ctxt b str'
          A.pure (str'', a' b')
        NotNull _ a -> go supply ctxt a str
        Var ty (MkVar n) -> unsafeCoerce (ctxt !! n) str
        Mu ty f
          | s:supply' <- supply ->
              let
                f' = go supply' (unsafeCoerce f' : ctxt) (f $ MkVar s)
              in
                f' str
          | otherwise -> error "impossible"
        Map ty f a
          | c:_ <- str, c `elem` _first ty -> fmap (pureCode f) <$> go supply ctxt a str
          | _null ty -> fmap (pureCode f) <$> go supply ctxt a str
          | otherwise ->
              let ta = cfgAnn a in
              Left $ case str of
                c : _ -> Unexpected c $ _first ta
                [] -> UnexpectedEof $ _first ta

data IR var c a where
  IR_Pure :: Ty c -> CodeOps a -> IR var c a
  IR_Bot :: Ty c -> IR var c a
  IR_Or :: Ty c -> NonEmpty (IR var c a) -> IR var c a
  IR_Empty :: Ty c -> IR var c ()
  IR_Char :: Ty c -> c -> IR var c c
  IR_Seq :: Ty c -> IR var c (a -> b) -> IR var c a -> IR var c b
  IR_NotNull :: Ty c -> IR var c a -> IR var c a
  IR_Var :: Ty c -> var c a -> IR var c a
  IR_Mu :: Ty c -> (var c a -> IR var c a) -> IR var c a
  IR_Map :: Ty c -> (CodeOps (a -> b)) -> IR var c a -> IR var c b

ir_str :: IR var c a -> String
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

irAnn :: IR b c d -> Ty c
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

toIR :: CFG (Ty c) v c a -> IR v c a
toIR e = case e of
  Pure t a -> IR_Pure t a
  Bot t ->  IR_Bot t
  Or t a b -> IR_Or t (fmap toIR (ors a <> ors b))
  Empty t -> IR_Empty t
  Char t c -> IR_Char t c
  Seq t a b -> IR_Seq t (toIR a) (toIR b)
  NotNull t a -> IR_NotNull t (toIR a)
  Var t v -> IR_Var t v
  Mu t f -> IR_Mu t (toIR . f)
  Map t (CodeOps f) (Map _ (CodeOps g) a) -> toIR (Map t (CodeOps (_comp f g)) a)
  Map t f a -> IR_Map t f (toIR a)
  where
    ors (Or _ a b) = ors a <> ors b
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
  go_lms [0..] [] . either (error . show) (toIR) . typeOf

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
  => [Int]
  -> Context r c
  -> IR Var c a
  -> r (s -> Maybe (s, a))
go_lms supply context e =
  case e of
    IR_Pure _ (lmsCode -> a) -> _lam $ \cs -> _just (_tup cs a)
    IR_Bot _ -> _lam $ \_ -> _nothing
    IR_Char _ c' ->
      _lam $
         \x -> _uncons x
                  (\c cs -> _if (_eq (pure c') c) (_just (_tup cs c)) (_nothing))
                  (_nothing)
    IR_Or _ bs -> ir_ors_lms supply context bs
    IR_Empty _ -> _lam $ \x -> _just (_tup x (pure ()))
    IR_Seq _ a b ->
      _lam $ \x ->
         _bind (go_lms supply context a <*> x)
               (_lam $ \r -> let x' = _fst r
                                 a' = _snd r
                             in
                              _bind (go_lms supply context b <*> x')
                                    (_lam $ \r' -> let x'' = _fst r'
                                                       b'  = _snd r'
                                                   in _just (_tup x'' (a' <*> b'))))
    IR_NotNull _ a -> go_lms supply context a
    IR_Map ty (lmsCode -> f) ta ->
        let r = _first ty
        in
          _lam $ \str ->
            _let (_lam (\mr -> _fmap (_lam $ \ab -> _fmap f ab) mr) `_comp` go_lms supply context ta) $ \success ->
            _let (_lam $ \str -> _if (pure $ _null ty) (success <*> str) (_nothing)) $ \fallThrough ->
                _uncons str (\c _ -> elem_lms c r (_lam $ \_ -> success <*> str) (fallThrough <*> str)) (fallThrough <*> str)

    IR_Var ty (MkVar n) ->
      _cast (context !! n)

    IR_Mu ty f
      | s:supply' <- supply -> do
          _fix (\x -> go_lms supply' (context ++ [ _forget x ] ) (f $ MkVar s))
      | otherwise -> error "impossible"

_const :: Ops r => r a -> r (b -> a)
_const r = _lam $ (\_ -> r)

ir_ors_lms
  :: forall s c a r
  . (Lift c, Eq c, Cons s s c c, Ops r)
  => [Int]
  -> Context r c
  -> NonEmpty (IR Var c a)
  -> r (s -> Maybe (s, a))
ir_ors_lms supply context as =
  let
    fallThrough =
      case filter (_null . irAnn) (toList as) of
        [] -> _lam $ \_ -> _nothing
        a : _ -> go_lms supply context a
  in
    _let fallThrough $ \ff -> _lam $
      \str -> _uncons str (\c _ -> foldr (comb c) ff as <*> str) (ff <*> str)
  where
    comb
      :: (Lift c, Eq c, Cons s s c c, Ops r)
      => r c
      -> IR Var c a
      -> r (s -> Maybe (s, a))
      -> r (s -> Maybe (s, a))
    comb c ta f2 =
      let
        r = _first (irAnn ta)
      in
        elem_lms c r (_const (go_lms supply context ta))f2
