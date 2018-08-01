{-# language TemplateHaskell #-}

module Test where

import Data.ByteString (ByteString)
import Data.ByteString.Internal (c2w)
import Data.Text (Text)
import Data.Word (Word8)

import CFG
import Library

parseA :: String -> Maybe (String, Char)
parseA = $$(makeParser (Char () 'a'))

parseAs :: String -> Maybe (String, [Char])
parseAs = $$(makeParser (many $ Char () 'a'))

parseAorB :: String -> Maybe (String, Char)
parseAorB = $$(makeParser (Or () (Char () 'a') (Char () 'b')))

parseAorBs :: String -> Maybe (String, [Char])
parseAorBs = $$(makeParser (many $ Or () (Char () 'a') (Char () 'b')))

parseAorBsT :: Text -> Maybe (Text, [Char])
parseAorBsT = $$(makeParser (many $ Or () (Char () 'a') (Char () 'b')))

parseAorBsB :: ByteString -> Maybe (ByteString, [Word8])
parseAorBsB =
  $$(makeParser (many $ Or () (Char () $ c2w 'a') (Char () $ c2w 'b')))

parseBrackets :: String -> Maybe (String, ())
parseBrackets = $$(makeParser (brackets id))

parseBracketsT :: Text -> Maybe (Text, ())
parseBracketsT = $$(makeParser (brackets id))

parseBracketsB :: ByteString -> Maybe (ByteString, ())
parseBracketsB = $$(makeParser (brackets c2w))

parseAlternate :: String -> Maybe (String, ())
parseAlternate = $$(makeParser alternate)

parseAlternatingBrackets :: String -> Maybe (String, ())
parseAlternatingBrackets = $$(makeParser alternatingBrackets)
