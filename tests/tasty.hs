module Main(main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Test

main :: IO ()
main =
    defaultMain $ testGroup "Tests"
      [ testCase "parseA success" $ parseA "a" @?= Just ("", 'a')
      , testCase "parseA failure" $ parseA "b" @?= Nothing
      , testCase "parseA suffix" $ parseA "ab" @?= Just ("b", 'a')

      , testCase "parseAs success" $ parseAs "aa" @?= Just ("", "aa")
      , testCase "parseAs start b" $ parseAs "ba" @?= Just ("ba", "")
      , testCase "parseAs suffix" $ parseAs "aab" @?= Just ("b", "aa")
      , testCase "parseAs empty" $ parseAs "" @?= Just ("", [])


      , testCase "parseAorB a" $ parseAorB "a" @?= Just ("", 'a')
      , testCase "parseAorB b" $ parseAorB "b" @?= Just ("", 'b')
      , testCase "parseAorB empty" $ parseAorB "" @?= Nothing

      , testCase "parseAorBs as" $ parseAorBs "aaaa" @?= Just ("", "aaaa")
      , testCase "parseAorBs bs" $ parseAorBs "bbbb" @?= Just ("", "bbbb")
      , testCase "parseAorBs bs" $ parseAorBs "abab" @?= Just ("", "abab")
      , testCase "parseAorBs mixture" $ parseAorBs "abacb" @?= Just ("cb", "aba")
      , testCase "parseAorBs empty" $ parseAorBs "" @?= Just ("", [])

      , testCase "parseBrackets empty" $ parseBrackets "" @?= Just ("", ())
      , testCase "parseBrackets ()" $ parseBrackets "()" @?= Just ("", ())
      , testCase "parseBrackets ()(" $ parseBrackets "()(" @?= Nothing
      , testCase "parseBrackets )()" $ parseBrackets ")()" @?= Just (")()", ())
      , testCase "parseBrackets (())" $ parseBrackets "(())" @?= Just ("", ())
      , testCase "parseBrackets (())()" $ parseBrackets "(())()" @?= Just ("", ())

      , testCase "parseAlternate ()" $ parseAlternate "()" @?= Just ("", ())
      , testCase "parseAlternate empty" $ parseAlternate "" @?= Just ("", ())
      , testCase "parseAlternate {}" $ parseAlternate "{}" @?= Just ("{}", ())
      , testCase "parseAlternate ({})" $ parseAlternate "({})" @?= Just ("", ())
      , testCase "parseAlternate ({{}})" $ parseAlternate "({{}})" @?= Nothing
      , testCase "parseAlternate ({[()]})" $ parseAlternate "({[()]})" @?= Just ("", ())
      ]
