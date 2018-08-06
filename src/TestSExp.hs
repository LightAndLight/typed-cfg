{-# LANGUAGE TemplateHaskell #-}
module TestSExp where

import LMS
import LibraryLMS

sexp_parser :: String -> Maybe (String, SExp)
sexp_parser = $$(compile sexp)
