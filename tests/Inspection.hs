{-# language TemplateHaskell #-}
{-# options_ghc -O -fplugin Test.Inspection.Plugin #-}
module Inspection(inspect_tests) where

import CFG (CFG(..), makeParser)
import Library (many, (<.), (.>))
import qualified LMS as LMS
import qualified LibraryLMS as LMS
import Test.Inspection

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=), assertFailure)

mkUnitTest :: String -> Result -> TestTree
mkUnitTest s r = testCase s assertion
  where
    assertion =
      case r of
        Success v -> return ()
        Failure reason -> assertFailure reason

inspect_tests :: TestTree
inspect_tests = testGroup "inspection tests" [parseAorBs_test
                                             , parseAlternate_test
                                             , parseBrackets_test ]

parseAorBsGen, parseAorBsCompile, parseAorBsHand :: String -> Maybe (String, [Char])
parseAorBsGen = $$(makeParser (many $ Or () (Char () 'a') (Char () 'b')))
parseAorBsCompile = $$(LMS.compile (LMS.many $ LMS.Or () (LMS.Char () 'a') (LMS.Char () 'b')))

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


parseAorBs_test :: TestTree
parseAorBs_test =
  testGroup "parseAorBs" $
  [ mkUnitTest "gen" $(inspectTest ('parseAorBsGen === 'parseAorBsHand))
  , mkUnitTest "lms" $(inspectTest ('parseAorBsCompile === 'parseAorBsHand)) ]


parseAlternateGen, parseAlternateCompile, parseAlternateHand :: [Char] -> Maybe ([Char], ())
parseAlternateGen =
  $$(makeParser $
     Mu () $ \t ->
     Or ()
       (Empty ())
       (Char () '(' .>
        (Mu () $ \u ->
         Or ()
           (Empty ())
           (Char () '{' .>
            (Mu () $ \v ->
             Or ()
               (Empty ())
               (Char () '[' .>
                Var () t <.
                Char () ']' <.
                Var () v)) <.
            Char () '}' <.
            Var () u)) <.
        Char () ')' <.
        Var () t))

parseAlternateCompile =
  $$(LMS.compile $
     LMS.Mu () $ \t ->
     LMS.Or ()
       (LMS.Empty ())
       (LMS.Char () '(' LMS..>
        (LMS.Mu () $ \u ->
         LMS.Or ()
           (LMS.Empty ())
           (LMS.Char () '{' LMS..>
            (LMS.Mu () $ \v ->
             LMS.Or ()
               (LMS.Empty ())
               (LMS.Char () '[' LMS..>
                LMS.Var () t LMS.<.
                LMS.Char () ']' LMS.<.
                LMS.Var () v)) LMS.<.
            LMS.Char () '}' LMS.<.
            LMS.Var () u)) LMS.<.
        LMS.Char () ')' LMS.<.
        LMS.Var () t))

parseAlternateHand = go0
  where
    -- the inspection test fails if we put `go0` first, because it doesn't know
    -- how to reason about binder ordering
    go0 [] = Just ([], ())
    go0 l@(x:xs) =
      case x of
        '(' ->
          case go1 xs of
            Just (xs', a) ->
              case xs' of
                x':xs'' ->
                  case x' of
                    ')' ->
                      case go0 xs'' of
                        Nothing -> Nothing
                        Just (xs''', _) -> Just (xs''', a)
                    _ -> Nothing
                [] -> Nothing
            Nothing -> Nothing
        _ -> Just (l, ())

    go1 [] = Just ([], ())
    go1 l@(x:xs) =
      case x of
        '{' ->
          case go2 xs of
            Just (xs', a) ->
              case xs' of
                x':xs'' ->
                  case x' of
                    '}' ->
                      case go1 xs'' of
                        Nothing -> Nothing
                        Just (xs''', _) -> Just (xs''', a)
                    _ -> Nothing
                [] -> Nothing
            Nothing -> Nothing
        _ -> Just (l, ())

    go2 [] = Just ([], ())
    go2 l@(x:xs) =
      case x of
        '[' ->
          case go0 xs of
            Just (xs', a) ->
              case xs' of
                x':xs'' ->
                  case x' of
                    ']' ->
                      case go2 xs'' of
                        Nothing -> Nothing
                        Just (xs''', _) -> Just (xs''', a)
                    _ -> Nothing
                [] -> Nothing
            Nothing -> Nothing
        _ -> Just (l, ())

parseAlternate_test :: TestTree
parseAlternate_test =
  testGroup "parseAlternate"
    [ mkUnitTest "gen" $(inspectTest ('parseAlternateGen ==- 'parseAlternateHand))
    , mkUnitTest "lms" $(inspectTest ('parseAlternateCompile ==- 'parseAlternateHand)) ]


parseBracketsGen, parseBracketsCompile, parseBracketsHand :: String -> Maybe (String, ())
parseBracketsGen =
  $$(makeParser $
     Mu () $ \t ->
     Or ()
       (Empty ())
       (Char () '(' .>
        Var () t <.
        Char () ')' <.
        Var () t))
parseBracketsCompile =
  $$(LMS.compile $
     LMS.Mu () $ \t ->
     LMS.Or ()
       (LMS.Empty ())
       (LMS.Char () '(' LMS..>
        LMS.Var () t LMS.<.
        LMS.Char () ')' LMS.<.
        LMS.Var () t))

parseBracketsHand [] = Just ([], ())
parseBracketsHand l@(x:xs) =
  case x of
    '(' ->
      case parseBracketsHand xs of
        Just (xs', a) ->
          case xs' of
            x':xs'' ->
              case x' of
                ')' ->
                  case parseBracketsHand xs'' of
                    Nothing -> Nothing
                    Just (xs''', _) -> Just (xs''', a)
                _ -> Nothing
            [] -> Nothing
        Nothing -> Nothing
    _ -> Just (l, ())

parseBrackets_test :: TestTree
parseBrackets_test =
  testGroup "parseBrackets" $
    [mkUnitTest "gen" $(inspectTest ('parseBracketsGen === 'parseBracketsHand))
    , mkUnitTest "lms" $(inspectTest ('parseBracketsCompile === 'parseBracketsHand)) ]
