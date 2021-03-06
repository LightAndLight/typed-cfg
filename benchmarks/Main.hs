module Main where

import Criterion.Main

import Test
import qualified TestLMS as LMS

import Data.ByteString (ByteString)
import Data.ByteString.Internal (c2w)
import Data.Text (Text)
import Data.Void (Void)
import Data.Word (Word8)
import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text as Text

parseAorBs' :: String -> Maybe [Char]
parseAorBs' =
  parseMaybe p
  where
    {-# inline p #-}
    p :: Parsec Void String String
    p = many (char 'a' <|> char 'b')

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

parseBrackets' :: String -> Maybe ()
parseBrackets' =
  parseMaybe p
  where
    {-# inline p #-}
    p :: Parsec Void String ()
    p = let x = (char '(' *> x <* char ')' <* x) <|> pure () in x

parseAorBsT' :: Text -> Maybe [Char]
parseAorBsT' =
  parseMaybe p
  where
    {-# inline p #-}
    p :: Parsec Void Text [Char]
    p = many (char 'a' <|> char 'b')

parseBracketsT' :: Text -> Maybe ()
parseBracketsT' =
  parseMaybe p
  where
    {-# inline p #-}
    p :: Parsec Void Text ()
    p = let x = (char '(' *> x <* char ')' <* x) <|> pure () in x

parseAorBsB' :: ByteString -> Maybe [Word8]
parseAorBsB' =
  parseMaybe p
  where
    {-# inline p #-}
    p :: Parsec Void ByteString [Word8]
    p = many (char (c2w 'a') <|> char (c2w 'b'))

parseBracketsB' :: ByteString -> Maybe ()
parseBracketsB' =
  parseMaybe p
  where
    {-# inline p #-}
    p :: Parsec Void ByteString ()
    p = let x = (char (c2w '(') *> x <* char (c2w ')') <* x) <|> pure () in x

main :: IO ()
main =
  defaultMain
  [ env (pure $ replicate 50000 'a') $ \str ->
      bgroup "a or bs"
      [ bench "normal" $ whnf parseAorBs str
      , bench "megaparsec" $ whnf parseAorBs' str
      , bench "hand" $ whnf parseAorBsHand str
      , bench "lms" $ whnf LMS.parseAorBs str
      , bench "interpret" $ whnf LMS.parseAorBs_interpret str
      ]
  , env (pure $ replicate 25000 '(' ++ replicate 25000 ')') $ \str ->
      bgroup "brackets"
      [ bench "normal" $ whnf parseBrackets str
      , bench "megaparsec" $ whnf parseBrackets' str
      , bench "lms" $ whnf LMS.parseBrackets str
      , bench "interpret" $ whnf LMS.parseBrackets_interpret str
      ]
  , env (pure . Text.pack $ replicate 50000 'a') $ \str ->
      bgroup "a or bs text"
      [ bench "normal" $ whnf parseAorBsT str
      , bench "megaparsec" $ whnf parseAorBsT' str
      ]
  , env (pure . Text.pack $ replicate 25000 '(' ++ replicate 25000 ')') $ \str ->
      bgroup "brackets text"
      [ bench "normal" $ whnf parseBracketsT str
      , bench "megaparsec" $ whnf parseBracketsT' str
      ]
  , env (pure . ByteString.pack $ replicate 50000 'a') $ \str ->
      bgroup "a or bs bytestring"
      [ bench "normal" $ whnf parseAorBsB str
      , bench "megaparsec" $ whnf parseAorBsB' str
      ]
  , env (pure . ByteString.pack $ replicate 25000 '(' ++ replicate 25000 ')') $ \str ->
      bgroup "brackets bytestring"
      [ bench "normal" $ whnf parseBracketsB str
      , bench "megaparsec" $ whnf parseBracketsB' str
      ]
  ]
