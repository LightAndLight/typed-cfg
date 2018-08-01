{-# language TemplateHaskell #-}
{-# options_ghc -O -fplugin Test.Inspection.Plugin #-}
module Inspection where

import CFG (CFG(..), makeParser)
import Library (many)
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
