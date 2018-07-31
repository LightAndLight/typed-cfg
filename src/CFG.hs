{-# language GADTs #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language RecursiveDo #-}
{-# language StandaloneDeriving #-}
{-# language ExistentialQuantification, FlexibleInstances #-}
{-# language ScopedTypeVariables #-}

{-# language TemplateHaskell #-}

module CFG where

import Unsafe.Coerce

import Control.Monad ((<=<), unless, when)
import Data.Either (fromRight)
import Data.List (intersect, union, foldl')
import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.Traversable (for)

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Jailbreak

eval' :: forall a. Q (TExp a) -> Q a
eval' q = eval (unTypeQ q) :: Q a

data CFG ann var c a where
  Pure :: Lift a => ann -> a -> CFG ann var c a
  Bot :: ann -> CFG ann var c a
  Or :: ann -> CFG ann var c a -> CFG ann var c a -> CFG ann var c a
  Empty :: ann -> CFG ann var c ()
  Char :: ann -> c -> CFG ann var c c
  Seq :: ann -> CFG ann var c (a -> b) -> CFG ann var c a -> CFG ann var c b
  NotNull :: ann -> CFG ann var c a -> CFG ann var c a
  Var :: ann -> var c a -> CFG ann var c a
  Mu :: ann -> (var c a -> CFG ann var c a) -> CFG ann var c a
  Map :: Lift a => ann -> (a -> b) -> CFG ann var c a -> CFG ann var c b

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

map' :: Lift a => (a -> b) -> CFG () var c a -> CFG () var c b
map' = Map ()

(<.>) :: CFG () var c (a -> b) -> CFG () var c a -> CFG () var c b
(<.>) = Seq ()
infixl 4 <.>

(<.) :: Lift a => CFG () var c a -> CFG () var c b -> CFG () var c a
(<.) a b = map' (\a b -> a) a <.> b
infixl 4 <.

(.>) :: Lift a => CFG () var c a -> CFG () var c b -> CFG () var c b
(.>) a b = map' (\a b -> b) a <.> b
infixl 4 .>

data Ty c
  = Ty
  { _null :: Bool
  , _first :: [c]
  , _followLast :: [c]
  , _guarded :: Bool
  } deriving (Eq, Show, Functor, Foldable, Traversable)

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
      in pure $ Pure t a
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
        in pure $ NotNull t' g'
    go supply ctxt allowGuarded (Bot ()) =
      let
        t = Ty { _null = False, _first = [], _followLast = [], _guarded = True }
      in pure (Bot t)
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
        in pure (Or t'' f' g')
    go supply ctxt allowGuarded (Empty ()) =
      let
        t = Ty { _null = True, _first = [], _followLast = [], _guarded = True }
      in pure (Empty t)
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
        in pure (Seq t'' a' b')
    go supply ctxt allowGuarded (Char () c) =
      let
        t = Ty { _null = False, _first = [c], _followLast = [], _guarded = True }
      in pure (Char t c)
    go supply ctxt allowGuarded (Map () f a) = do
      a' <- go supply ctxt allowGuarded a
      let t = cfgAnn a'
      pure (Map t f a')
    go (s:supply) ctxt allowGuarded (Mu () f) = do
      res <- fix (\ty -> go supply (cfgAnn ty:ctxt) allowGuarded (f $ MkVar s))
      let t = cfgAnn res
      if _guarded t
        then pure (Mu t $ fromRight (error "impossible") . go supply (t:ctxt) allowGuarded . f)
        else Left $ NotGuarded (ShowCFG $ Mu () f)
      where
        fix f = inner =<< typeOf (Bot ())
          where
            inner input = do
              output <- f input
              if cfgAnn input == cfgAnn output
                then pure output
                else inner output
    go [] _ _ (Mu _ _) = error "impossible"
    go supply ctxt allowGuarded a@(Var () (MkVar n)) =
      let t = ctxt !! n in
      if not allowGuarded && _guarded t
      then Left $ Guarded (ShowCFG a)
      else pure $ Var t (MkVar n)

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
        Pure _ a -> Right (str, a)
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
        Empty{} -> pure (str, ())
        Char ty c'
          | c:cs <- str, c == c' -> pure (cs, c')
          | otherwise ->
              Left $ case str of
                c : _ -> Unexpected c [c']
                [] -> UnexpectedEof [c']
        Seq ty a b -> do
          (str', a') <- go supply ctxt a str
          (str'', b') <- go supply ctxt b str'
          pure (str'', a' b')
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
          | c:_ <- str, c `elem` _first ty -> fmap f <$> go supply ctxt a str
          | _null ty -> fmap f <$> go supply ctxt a str
          | otherwise ->
              let ta = cfgAnn a in
              Left $ case str of
                c : _ -> Unexpected c $ _first ta
                [] -> UnexpectedEof $ _first ta

many :: Lift a => CFG () v c a -> CFG () v c [a]
many c = Mu () $ \x -> Or () (map' (const []) $ Empty ()) (map' (:) c <.> Var () x)

some :: Lift a => CFG () v c a -> CFG () v c [a]
some c = map' (:) c <.> many c

-- | T → ε | "(" S ")" T
-- | S → ε | "(" T ")" S
brackets :: CFG () v Char Int
brackets =
  Mu () $ \t ->
  Or ()
    (map' (const 1) $ Empty ())
    (map' (const (*)) (Char () '(') <.>
     (map' (+1) $
      Mu () $ \s ->
      Or ()
        (map' (const 0) $ Empty ())
        (map' (*) (map' (const (+1)) (Char () '(') <.> Var () t <. Char () ')') <.> Var () s)) <.
      Char () ')' <.>
     Var () t)

data IR var c a where
  IR_Pure :: Ty c -> TExp a -> IR var c a
  IR_Bot :: Ty c -> IR var c a
  IR_Or :: Ty c -> NonEmpty (IR var c a) -> IR var c a
  IR_Empty :: Ty c -> IR var c ()
  IR_Char :: Ty c -> c -> IR var c c
  IR_Seq :: Ty c -> IR var c (a -> b) -> IR var c a -> IR var c b
  IR_NotNull :: Ty c -> IR var c a -> IR var c a
  IR_Var :: Ty c -> var c a -> IR var c a
  IR_Mu :: Ty c -> (var c a -> Q (IR var c a)) -> IR var c a
  IR_Map :: Ty c -> (TExp a -> TExp b) -> IR var c a -> IR var c b

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

toIR :: CFG (Ty c) v c a -> Q (IR v c a)
toIR e = case e of
  Pure t a -> IR_Pure t <$> [|| a ||]
  Bot t -> pure $ IR_Bot t
  Or t a b -> IR_Or t <$> traverse toIR (ors a <> ors b)
  Empty t -> pure $ IR_Empty t
  Char t c -> pure $ IR_Char t c
  Seq t a b -> IR_Seq t <$> toIR a <*> toIR b
  NotNull t a -> IR_NotNull t <$> toIR a
  Var t v -> pure $ IR_Var t v
  Mu t f -> pure $ IR_Mu t (toIR . f)
  Map t f (Map _ g a) -> toIR (Map t (f . g) a)
  Map t f a -> IR_Map t (_ f) <$> toIR a
  where
    ors (Or _ a b) = ors a <> ors b
    ors a = pure a

makeParser
  :: (Lift a, Lift c, Eq c, Show c)
  => CFG () Var c a
  -- Q (TExp ([c] -> Maybe ([c], a)))
  -> Q Exp
makeParser = unTypeQ . go <=< toIR <=< either (fail . show) pure . typeOf
  where
    go
      :: (Lift c, Eq c)
      => IR Var c a
      -> Q (TExp ([c] -> Maybe ([c], a)))
    go e =
      case e of
        IR_Pure _ a -> [|| \cs -> Just (cs, $$( pure a )) ||]
        IR_Bot _ -> [|| \_ -> Nothing ||]
        IR_Char _ c' ->
          [||
             \x -> case x of
               c : cs -> if c' == c then Just (cs, c) else Nothing
               [] -> Nothing
          ||]
        IR_Or _ bs -> ir_ors bs
        IR_Empty _ -> [|| \x -> Just (x, ()) ||]
        IR_Seq _ a b -> do
          [|| \x -> $$( go a ) x >>= \(x', a') ->
             $$( go b ) x' >>= \(x'', b') ->
             pure (x'', a' b') ||]
        IR_NotNull _ a -> go a

    ir_ors :: (Lift c, Eq c)
           => NonEmpty (IR Var c a) -> Q (TExp ([c] -> Maybe ([c], a)))
    ir_ors as = foldl' comb [|| \_ -> Nothing ||] as
      where
        comb :: (Lift c, Eq c)
                => Q (TExp ([c] -> Maybe ([c], a)))
                -> IR Var c a
                -> Q (TExp ([c] -> Maybe ([c], a)))
        comb f2 ta =
          let r = _first (irAnn ta)
              n = _null (irAnn ta)
          in
          [|| \str ->
                          case str of
                            c:_ | c `elem` r ->  $$(go ta) str
                            []  | n -> $$(go ta) str
                            _ -> $$(f2) str ||]
