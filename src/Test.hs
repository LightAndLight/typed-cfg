{-# language TemplateHaskell #-}

module Test where

import CFG

parseA :: String -> Maybe (String, Char)
parseA = $$(makeParser (Char () 'a'))

parseAs :: String -> Maybe (String, [Char])
parseAs = $$(makeParser (many $ Char () 'a'))

parseAorB :: String -> Maybe (String, Char)
parseAorB = $$(makeParser (Or () (Char () 'a') (Char () 'b')))

parseAorBs :: String -> Maybe (String, [Char])
parseAorBs = $$(makeParser (many $ Or () (Char () 'a') (Char () 'b')))

parseBrackets :: String -> Maybe (String, ())
parseBrackets = $$(makeParser brackets)

parseAlternate :: String -> Maybe (String, ())
parseAlternate = $$(makeParser alternate)

parseAlternatingBrackets :: String -> Maybe (String, ())
parseAlternatingBrackets = $$(makeParser alternatingBrackets)
