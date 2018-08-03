{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
module LibraryLMS where

import LMS
import Language.Haskell.TH.Syntax
import Prelude hiding (Applicative(..))

map' :: (forall r . Ops r => (r a -> r b)) -> CFG () var c a -> CFG () var c b
map' f = Map () (CodeOps (_lam $ \x -> f x))

(<.>) :: CFG () var c (a -> b) -> CFG () var c a -> CFG () var c b
(<.>) = Seq ()
infixl 4 <.>

(<.) :: CFG () var c a -> CFG () var c b -> CFG () var c a
(<.) a b = map' (\r -> _lam $ \v -> _const_l r v)  a <.> b
infixl 4 <.

(.>) :: CFG () var c a -> CFG () var c b -> CFG () var c b
(.>) a b = map' (\r -> _lam $ \v -> _const_r r v) a <.> b
infixl 4 .>


many :: Lift a => CFG () v c a -> CFG () v c [a]
many c = Mu () $ \x -> Or () (map' (_const_l (pure [])) $ Empty ()) (map' _cons c <.> Var () x)

some :: Lift a => CFG () v c a -> CFG () v c [a]
some c = map' _cons c <.> many c

-- | T → ε | "(" T ")" T
brackets :: (Char -> c) -> CFG () v c ()
brackets cc =
  Mu () $ \t ->
  Or ()
    (Empty ())
    (Char () (cc '(') .> Var () t <. Char () (cc ')') <. Var () t)

alternatingBrackets :: CFG () v Char ()
alternatingBrackets =
  Mu () $ \t ->
  Or ()
    (Or ()
      (Empty ())
      (Char () '(' .> Var () t <. Char () ')' <. Var () t))
      (Char () '[' .> Var () t <. Char () ']' <. Var () t)

-- |
-- T ::= e | '(' U ')' T
-- U ::= e | '{' V '}' U
-- V ::= e | '{' T '}' V
alternate :: CFG () v Char ()
alternate =
  Mu () $ \t ->
  Or ()
    (Empty ())
    (Char () '(' .>
     (Mu () $ \u ->
      Or ()
        (Empty ())
        (Char () '{' .>
         (Mu () $ \v ->
          Or ()
            (Empty ())
            (Char () '[' .>
             Var () t <.
             Char () ']' <.
             Var () v)) <.
         Char () '}' <.
         Var () u)) <.
     Char () ')' <.
     Var () t)
