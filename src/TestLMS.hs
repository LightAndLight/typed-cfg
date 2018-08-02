{-# language TemplateHaskell #-}

module TestLMS where

import Data.ByteString (ByteString)
import Data.ByteString.Internal (c2w)
import Data.Text (Text)
import Data.Word (Word8)

import LMS
import LibraryLMS

parseA :: String -> Maybe (String, Char)
parseA = $$(compile (Char () 'a'))

parseA_interpret :: String -> Maybe (String, Char)
parseA_interpret = interpret (Char () 'a')

parseAs :: String -> Maybe (String, [Char])
parseAs = $$(compile (many $ Char () 'a'))

parseAs_interpret :: String -> Maybe (String, [Char])
parseAs_interpret = interpret (many $ Char () 'a')

parseAorB :: String -> Maybe (String, Char)
parseAorB = $$(compile (Or () (Char () 'a') (Char () 'b')))

parseAorB_interpret:: String -> Maybe (String, Char)
parseAorB_interpret = interpret (Or () (Char () 'a') (Char () 'b'))

parseAorBs :: String -> Maybe (String, [Char])
parseAorBs = $$(compile (many $ Or () (Char () 'a') (Char () 'b')))

parseAorBs_interpret :: String -> Maybe (String, [Char])
parseAorBs_interpret = interpret (many $ Or () (Char () 'a') (Char () 'b'))

parseBrackets :: String -> Maybe (String, ())
parseBrackets = $$(compile (brackets id))

parseBrackets_interpret :: String -> Maybe (String, ())
parseBrackets_interpret = interpret (brackets id)

parseAlternate :: String -> Maybe (String, ())
parseAlternate = $$(compile alternate)

parseAlternate_interpret :: String -> Maybe (String, ())
parseAlternate_interpret = interpret alternate

parseAlternatingBrackets :: String -> Maybe (String, ())
parseAlternatingBrackets = $$(compile alternatingBrackets)

parseAlternatingBrackets_interpret :: String -> Maybe (String, ())
parseAlternatingBrackets_interpret = interpret alternatingBrackets
