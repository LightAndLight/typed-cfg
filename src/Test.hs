{-# language TemplateHaskell #-}

module Test where

import CFG

a_or_b_or_c_or_d :: String -> Maybe (String, Char)
a_or_b_or_c_or_d =
  $$(makeParser
      (Or ()
        (Char () 'a')
        (Or ()
          (Char () 'b')
          (Or () (Char () 'c') (Char () 'd')))))

parseBrackets :: String -> Maybe (String, Char)
parseBrackets = $$(makeParser brackets)
