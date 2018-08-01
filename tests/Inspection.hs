{-# language TemplateHaskell #-}
{-# options_ghc -O -fplugin Test.Inspection.Plugin #-}
module Inspection where

import CFG (CFG(..), makeParser)
import Library (many)
import Test
import Test.Inspection

parseAorBsGen, parseAorBsHand :: String -> Maybe (String, [Char])
parseAorBsGen = $$(makeParser (many $ Or () (Char () 'a') (Char () 'b')))

parseAorBsHand [] = Just ("", [])
parseAorBsHand (c:cs) =
  case c of
    'a' ->
      case parseAorBsHand cs of
        Nothing -> Just (cs, "a")
        Just (cs', r) -> Just (cs', 'a':r)
    'b' ->
      case parseAorBsHand cs of
        Nothing -> Just (cs, "b")
        Just (cs', r) -> Just (cs', 'b':r)
    _   -> Nothing

inspect ('parseAorBsGen === 'parseAorBsHand)
