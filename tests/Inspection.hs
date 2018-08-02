{-# language TemplateHaskell #-}
{-# options_ghc -O -fplugin Test.Inspection.Plugin #-}
module Inspection where

import CFG (CFG(..), makeParser)
import Library (many, (<.), (.>))
import Test.Inspection

parseAorBsGen, parseAorBsHand :: String -> Maybe (String, [Char])
parseAorBsGen = $$(makeParser (many $ Or () (Char () 'a') (Char () 'b')))

parseAorBsHand [] = Just ("", [])
parseAorBsHand str@(c:cs) =
  case c of
    'a' ->
      case parseAorBsHand cs of
        Nothing -> Nothing
        Just (cs', r) -> Just (cs', c:r)
    'b' ->
      case parseAorBsHand cs of
        Nothing -> Nothing
        Just (cs', r) -> Just (cs', c:r)
    _   -> Just (str, [])

inspect ('parseAorBsGen === 'parseAorBsHand)


parseAlternateGen, parseAlternateHand :: [Char] -> Maybe ([Char], ())
parseAlternateGen =
  $$(makeParser $
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
        Var () t))

parseAlternateHand = go0
  where
    -- the inspection test fails if we put `go0` first, because it doesn't know
    -- how to reason about binder ordering
    go1 [] = Just ([], ())
    go1 l@(x:xs) =
      case x of
        '{' ->
          case go2 xs of
            Just (xs', a) ->
              case xs' of
                x':xs'' ->
                  case x' of
                    '}' ->
                      case go1 xs'' of
                        Nothing -> Nothing
                        Just (xs''', _) -> Just (xs''', a)
                    _ -> Nothing
                [] -> Nothing
            Nothing -> Nothing
        _ -> Just (l, ())

    go2 [] = Just ([], ())
    go2 l@(x:xs) =
      case x of
        '[' ->
          case go0 xs of
            Just (xs', a) ->
              case xs' of
                x':xs'' ->
                  case x' of
                    ']' ->
                      case go2 xs'' of
                        Nothing -> Nothing
                        Just (xs''', _) -> Just (xs''', a)
                    _ -> Nothing
                [] -> Nothing
            Nothing -> Nothing
        _ -> Just (l, ())

    go0 [] = Just ([], ())
    go0 l@(x:xs) =
      case x of
        '(' ->
          case go1 xs of
            Just (xs', a) ->
              case xs' of
                x':xs'' ->
                  case x' of
                    ')' ->
                      case go0 xs'' of
                        Nothing -> Nothing
                        Just (xs''', _) -> Just (xs''', a)
                    _ -> Nothing
                [] -> Nothing
            Nothing -> Nothing
        _ -> Just (l, ())

inspect ('parseAlternateGen ==- 'parseAlternateHand)


parseBracketsGen, parseBracketsHand :: String -> Maybe (String, ())
parseBracketsGen =
  $$(makeParser $
     Mu () $ \t ->
     Or ()
       (Empty ())
       (Char () '(' .>
        Var () t <.
        Char () ')' <.
        Var () t))

parseBracketsHand [] = Just ([], ())
parseBracketsHand l@(x:xs) =
  case x of
    '(' ->
      case parseBracketsHand xs of
        Just (xs', a) ->
          case xs' of
            x':xs'' ->
              case x' of
                ')' ->
                  case parseBracketsHand xs'' of
                    Nothing -> Nothing
                    Just (xs''', _) -> Just (xs''', a)
                _ -> Nothing
            [] -> Nothing
        Nothing -> Nothing
    _ -> Just (l, ())

inspect ('parseBracketsGen === 'parseBracketsHand)
