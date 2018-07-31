{-# language TemplateHaskell #-}

{- options_ghc
  -fplugin CFG.Plugin
  -fplugin-opt CFG.Plugin:a
-}
module Test where

import CFG

data Boo = A | B

thingo :: String -> Maybe (String, Char)
thingo = $(makeParser brackets)
