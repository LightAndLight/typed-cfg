module Main where

import Criterion.Main

import Test

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

parseAorBs' :: String -> Maybe [Char]
parseAorBs' =
  parseMaybe p
  where
    {-# inline p #-}
    p :: Parsec Void String String
    p = many (char 'a' <|> char 'b')

parseBrackets' :: String -> Maybe ()
parseBrackets' =
  parseMaybe p
  where
    {-# inline p #-}
    p :: Parsec Void String ()
    p = let x = (char '(' *> x <* char ')' <* x) <|> pure () in x

main :: IO ()
main =
  defaultMain
  [ env (pure $ replicate 50000 'a') $ \str ->
      bgroup "a or bs"
      [ bench "normal" $ whnf parseAorBs str
      , bench "megaparsec" $ whnf parseAorBs' str
      ]
  , env (pure $ replicate 25000 '(' ++ replicate 25000 ')') $ \str ->
      bgroup "brackets"
      [ bench "normal" $ whnf parseBrackets str
      , bench "megaparsec" $ whnf parseBrackets' str
      ]
  ]
