{-# language TemplateHaskell #-}
module Library where

import CFG
import Language.Haskell.TH.Syntax

map' :: (a -> b) -> (Code (a -> b)) -> CFG () var c a -> CFG () var c b
map' = Map ()

(<.>) :: CFG () var c (a -> b) -> CFG () var c a -> CFG () var c b
(<.>) = Seq ()
infixl 4 <.>

(<.) :: CFG () var c a -> CFG () var c b -> CFG () var c a
(<.) a b = map' (\a b -> a) [|| \a b -> a ||] a <.> b
infixl 4 <.

(.>) :: CFG () var c a -> CFG () var c b -> CFG () var c b
(.>) a b = map' (\a b -> b) [|| (\a b -> b) ||]  a <.> b
infixl 4 .>


many :: Lift a => CFG () v c a -> CFG () v c [a]
many c = Mu () $ \x -> Or () (map' (const []) [|| (const []) ||] $ Empty ()) (map' (:) [|| (:) ||] c <.> Var () x)

some :: Lift a => CFG () v c a -> CFG () v c [a]
some c = map' (:) [|| (:) ||] c <.> many c

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
