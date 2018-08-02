module Main(main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Test as T
import qualified TestLMS as LMS

main :: IO ()
main =
    defaultMain $ testGroup "all"
      [testGroup "normal"
        [ testCase "parseA success" $ T.parseA "a" @?= Just ("", 'a')
        , testCase "parseA failure" $ T.parseA "b" @?= Nothing
        , testCase "parseA suffix" $ T.parseA "ab" @?= Just ("b", 'a')

        , testCase "parseAs success" $ T.parseAs "aa" @?= Just ("", "aa")
        , testCase "parseAs start b" $ T.parseAs "ba" @?= Just ("ba", "")
        , testCase "parseAs suffix" $ T.parseAs "aab" @?= Just ("b", "aa")
        , testCase "parseAs empty" $ T.parseAs "" @?= Just ("", [])


        , testCase "parseAorB a" $ T.parseAorB "a" @?= Just ("", 'a')
        , testCase "parseAorB b" $ T.parseAorB "b" @?= Just ("", 'b')
        , testCase "parseAorB empty" $ T.parseAorB "" @?= Nothing

        , testCase "parseAorBs as" $ T.parseAorBs "aaaa" @?= Just ("", "aaaa")
        , testCase "parseAorBs bs" $ T.parseAorBs "bbbb" @?= Just ("", "bbbb")
        , testCase "parseAorBs bs" $ T.parseAorBs "abab" @?= Just ("", "abab")
        , testCase "parseAorBs mixture" $ T.parseAorBs "abacb" @?= Just ("cb", "aba")
        , testCase "parseAorBs empty" $ T.parseAorBs "" @?= Just ("", [])

        , testCase "parseBrackets empty" $ T.parseBrackets "" @?= Just ("", ())
        , testCase "parseBrackets ()" $ T.parseBrackets "()" @?= Just ("", ())
        , testCase "parseBrackets ()(" $ T.parseBrackets "()(" @?= Nothing
        , testCase "parseBrackets )()" $ T.parseBrackets ")()" @?= Just (")()", ())
        , testCase "parseBrackets (())" $ T.parseBrackets "(())" @?= Just ("", ())
        , testCase "parseBrackets (())()" $ T.parseBrackets "(())()" @?= Just ("", ())

        , testCase "parseAlternate ()" $ T.parseAlternate "()" @?= Just ("", ())
        , testCase "parseAlternate empty" $ T.parseAlternate "" @?= Just ("", ())
        , testCase "parseAlternate {}" $ T.parseAlternate "{}" @?= Just ("{}", ())
        , testCase "parseAlternate ({})" $ T.parseAlternate "({})" @?= Just ("", ())
        , testCase "parseAlternate ({{}})" $ T.parseAlternate "({{}})" @?= Nothing
        , testCase "parseAlternate ({[()]})" $ T.parseAlternate "({[()]})" @?= Just ("", ())
        ]
      , testGroup "LMS"
        [ testCase "parseA success" $ LMS.parseA "a" @?= Just ("", 'a')
        , testCase "parseA failure" $ LMS.parseA "b" @?= Nothing
        , testCase "parseA suffix" $ LMS.parseA "ab" @?= Just ("b", 'a')

        , testCase "parseAs success" $ LMS.parseAs "aa" @?= Just ("", "aa")
        , testCase "parseAs start b" $ LMS.parseAs "ba" @?= Just ("ba", "")
        , testCase "parseAs suffix" $ LMS.parseAs "aab" @?= Just ("b", "aa")
        , testCase "parseAs empty" $ LMS.parseAs "" @?= Just ("", [])


        , testCase "parseAorB a" $ LMS.parseAorB "a" @?= Just ("", 'a')
        , testCase "parseAorB b" $ LMS.parseAorB "b" @?= Just ("", 'b')
        , testCase "parseAorB empty" $ LMS.parseAorB "" @?= Nothing

        , testCase "parseAorBs as" $ LMS.parseAorBs "aaaa" @?= Just ("", "aaaa")
        , testCase "parseAorBs bs" $ LMS.parseAorBs "bbbb" @?= Just ("", "bbbb")
        , testCase "parseAorBs bs" $ LMS.parseAorBs "abab" @?= Just ("", "abab")
        , testCase "parseAorBs mixture" $ LMS.parseAorBs "abacb" @?= Just ("cb", "aba")
        , testCase "parseAorBs empty" $ LMS.parseAorBs "" @?= Just ("", [])

        , testCase "parseBrackets empty" $ LMS.parseBrackets "" @?= Just ("", ())
        , testCase "parseBrackets ()" $ LMS.parseBrackets "()" @?= Just ("", ())
        , testCase "parseBrackets ()(" $ LMS.parseBrackets "()(" @?= Nothing
        , testCase "parseBrackets )()" $ LMS.parseBrackets ")()" @?= Just (")()", ())
        , testCase "parseBrackets (())" $ LMS.parseBrackets "(())" @?= Just ("", ())
        , testCase "parseBrackets (())()" $ LMS.parseBrackets "(())()" @?= Just ("", ())

        , testCase "parseAlternate ()" $ LMS.parseAlternate "()" @?= Just ("", ())
        , testCase "parseAlternate empty" $ LMS.parseAlternate "" @?= Just ("", ())
        , testCase "parseAlternate {}" $ LMS.parseAlternate "{}" @?= Just ("{}", ())
        , testCase "parseAlternate ({})" $ LMS.parseAlternate "({})" @?= Just ("", ())
        , testCase "parseAlternate ({{}})" $ LMS.parseAlternate "({{}})" @?= Nothing
        , testCase "parseAlternate ({[()]})" $ LMS.parseAlternate "({[()]})" @?= Just ("", ())
        ]
      , testGroup "LMS interpreted"
        [ testCase "parseA success" $ LMS.parseA_interpret "a" @?= Just ("", 'a')
        , testCase "parseA failure" $ LMS.parseA_interpret "b" @?= Nothing
        , testCase "parseA suffix" $ LMS.parseA_interpret "ab" @?= Just ("b", 'a')

        , testCase "parseAs success" $ LMS.parseAs_interpret  "aa" @?= Just ("", "aa")
        , testCase "parseAs start b" $ LMS.parseAs_interpret  "ba" @?= Just ("ba", "")
        , testCase "parseAs suffix" $ LMS.parseAs_interpret  "aab" @?= Just ("b", "aa")
        , testCase "parseAs empty" $ LMS.parseAs_interpret  "" @?= Just ("", [])


        , testCase "parseAorB a" $ LMS.parseAorB_interpret  "a" @?= Just ("", 'a')
        , testCase "parseAorB b" $ LMS.parseAorB_interpret  "b" @?= Just ("", 'b')
        , testCase "parseAorB empty" $ LMS.parseAorB_interpret  "" @?= Nothing

        , testCase "parseAorBs as" $ LMS.parseAorBs_interpret  "aaaa" @?= Just ("", "aaaa")
        , testCase "parseAorBs bs" $ LMS.parseAorBs_interpret  "bbbb" @?= Just ("", "bbbb")
        , testCase "parseAorBs bs" $ LMS.parseAorBs_interpret  "abab" @?= Just ("", "abab")
        , testCase "parseAorBs mixture" $ LMS.parseAorBs_interpret  "abacb" @?= Just ("cb", "aba")
        , testCase "parseAorBs empty" $ LMS.parseAorBs_interpret  "" @?= Just ("", [])

        , testCase "parseBrackets empty" $ LMS.parseBrackets_interpret  "" @?= Just ("", ())
        , testCase "parseBrackets ()" $ LMS.parseBrackets_interpret  "()" @?= Just ("", ())
        , testCase "parseBrackets ()(" $ LMS.parseBrackets_interpret  "()(" @?= Nothing
        , testCase "parseBrackets )()" $ LMS.parseBrackets_interpret  ")()" @?= Just (")()", ())
        , testCase "parseBrackets (())" $ LMS.parseBrackets_interpret  "(())" @?= Just ("", ())
        , testCase "parseBrackets (())()" $ LMS.parseBrackets_interpret  "(())()" @?= Just ("", ())

        , testCase "parseAlternate ()" $ LMS.parseAlternate_interpret  "()" @?= Just ("", ())
        , testCase "parseAlternate empty" $ LMS.parseAlternate_interpret  "" @?= Just ("", ())
        , testCase "parseAlternate {}" $ LMS.parseAlternate_interpret  "{}" @?= Just ("{}", ())
        , testCase "parseAlternate ({})" $ LMS.parseAlternate_interpret  "({})" @?= Just ("", ())
        , testCase "parseAlternate ({{}})" $ LMS.parseAlternate_interpret  "({{}})" @?= Nothing
        , testCase "parseAlternate ({[()]})" $ LMS.parseAlternate_interpret  "({[()]})" @?= Just ("", ())
        ]
        ]
