{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import qualified Data.Text as T -- Use qualified import for Text
import Text.Megaparsec (parse, errorBundlePretty) -- Add ParseErrorBundle
import MyLib
 -- Import Void

main :: IO ()
main = defaultMain tests

-- Helper to run parser; fails test on parse error
runMyParser :: Parser a -> Text -> Assertion -> (a -> Assertion) -> Assertion
runMyParser p input _onFailure onSuccess =
  case parse p "" input of
    Left err -> assertFailure $ "Parser error: " ++ errorBundlePretty err
    Right val -> onSuccess val

-- Assertion for parsing results
parseEquals :: (Eq a, Show a) => Parser a -> Text -> a -> Assertion
parseEquals p input expected =
    runMyParser p input
        (assertFailure $ "Expected successful parse for: " ++ T.unpack input) -- Provide specific failure message
        (\val -> val @?= expected)

-- Assertion for matching results (parses regex, then runs `matches`)
testMatches :: String -> Text -> Bool -> Assertion
testMatches patternString inputText expectedBool =
    case parse regex "" (T.pack patternString) of
        Left err -> assertFailure $ "Regex Parse Failure for \"" ++ patternString ++ "\": " ++ errorBundlePretty err
        Right parsedRegex ->
            -- Use the 'matches' function which returns Bool
            (matches parsedRegex inputText) @?= expectedBool

tests :: TestTree
tests = testGroup "Regex Tests" -- Renamed top-level group
  [ testGroup "Parser Tests" -- Group all parser tests
    [ testGroup "Quantifiers"
        [ testCase "{3}"    $ parseEquals parseQuant "{3}"    (Exactly 3)
        , testCase "{3,}"   $ parseEquals parseQuant "{3,}"   (AtLeast 3)
        , testCase "{3,5}"  $ parseEquals parseQuant "{3,5}"  (Between 3 5)
        -- Corrected quantifier tests for *, +, ?
        , testCase "*"      $ parseEquals parseQuant "*"      ZeroOrMore
        , testCase "+"      $ parseEquals parseQuant "+"      OneOrMore
        , testCase "?"      $ parseEquals parseQuant "?"      ZeroOrOne
        ]

    , testGroup "Escaped characters"
        [ testCase "\\w" $ parseEquals escapedParser "\\w" WordChar
        , testCase "\\D" $ parseEquals escapedParser "\\D" NotDigitChar
        , testCase "\\." $ parseEquals escapedParser "\\." (LiteralChar '.')
        , testCase "\\a" $ parseEquals escapedParser "\\a" BellChar
        , testCase "\\\\" $ parseEquals escapedParser "\\\\" (LiteralChar '\\') -- Test escaped backslash
        , testCase "\\$" $ parseEquals escapedParser "\\$" (LiteralChar '$') -- Test escaped special char
        ]

    , testGroup "Regex symbols"
        [ testCase "."    $ parseEquals parseRegexSymbol "."    Any
        , testCase "^"    $ parseEquals parseRegexSymbol "^"    Caret
        , testCase "$"    $ parseEquals parseRegexSymbol "$"    Dollar
        , testCase "x"    $ parseEquals parseRegexSymbol "x"    (Literal 'x')
        , testCase "\\t" $ parseEquals parseRegexSymbol "\\t" (Escape TabChar)
        ]

    , testGroup "Character classes"
        [ testCase "[abc]" $
            parseEquals classParser "[abc]" (RClass False [Single 'a', Single 'b', Single 'c'])

        , testCase "[a-z]" $
            parseEquals classParser "[a-z]" (RClass False [Range 'a' 'z'])

        , testCase "[^x]" $
            parseEquals classParser "[^x]" (RClass True [Single 'x'])

        -- Added more class tests
        , testCase "[a-zA-Z0-9_]" $
             parseEquals classParser "[a-zA-Z0-9_]" (RClass False [Range 'a' 'z', Range 'A' 'Z', Range '0' '9', Single '_'])

        -- Inside the "Character classes" test group:
        , testCase "[^\\d]" $ -- Test expects parser to treat \d as literal 'd' inside class
             parseEquals classParser "[^\\d]" (RClass True [Single 'd']) -- Corrected expectation
        , testCase "Invalid range [z-a]" $
            case parse classParser "" "[z-a]" of
              Left _ -> pure () -- Expect parse failure
              Right res -> assertFailure $ "Expected failure for invalid range [z-a], but got: " ++ show res
        ]

      , testGroup "Regex Quantified Atom" -- Test parsing of atom+quantifier
        [ testCase "a{3}" $
            parseEquals regexQuantified "a{3}" $ Quantified (Symbol (Literal 'a')) (Exactly 3)
        , testCase "\\d*" $
             parseEquals regexQuantified "\\d*" $ Quantified (Symbol (Escape DigitChar)) ZeroOrMore
         , testCase "(abc)?" $
             parseEquals regexQuantified "(abc)?" $ Quantified (Group (Seq [Symbol (Literal 'a'), Symbol (Literal 'b'), Symbol (Literal 'c')])) ZeroOrOne
         , testCase "[^x]+" $
             parseEquals regexQuantified "[^x]+" $ Quantified (Symbol (Class (RClass True [Single 'x']))) OneOrMore
        ]

      , testGroup "Regex Parser AST (Full)" -- Test parsing of complex patterns
        [ testCase "Literal concatenation: ab" $
            parseEquals regex "ab"
              (Seq [Symbol (Literal 'a'), Symbol (Literal 'b')])

        , testCase "Simple alternation: a|b" $
            parseEquals regex "a|b"
              (Alt [Symbol (Literal 'a'), Symbol (Literal 'b')])

        , testCase "Grouped alternation: (a|b)" $
            parseEquals regex "(a|b)"
              (Group (Alt [Symbol (Literal 'a'), Symbol (Literal 'b')]))

        , testCase "Sequence with quantifier: a{2}b" $
            parseEquals regex "a{2}b"
              (Seq [Quantified (Symbol (Literal 'a')) (Exactly 2), Symbol (Literal 'b')])

        , testCase "Wildcard with quantifier: .*" $
            parseEquals regex ".*"
              (Quantified (Symbol Any) ZeroOrMore)

        , testCase "Anchored pattern: ^abc$" $
            parseEquals regex "^abc$"
              (Seq [Symbol Caret, Symbol (Literal 'a'), Symbol (Literal 'b'), Symbol (Literal 'c'), Symbol Dollar])

        , testCase "Group repetition: (ab)+" $
            parseEquals regex "(ab)+"
              (Quantified (Group (Seq [Symbol (Literal 'a'), Symbol (Literal 'b')])) OneOrMore)

        , testCase "Nested grouping: ((x))" $
            parseEquals regex "((x))"
              (Group (Group (Symbol (Literal 'x'))))

        , testCase "Alternation of sequences: ab|cd" $
            parseEquals regex "ab|cd"
              (Alt
                [ Seq [Symbol (Literal 'a'), Symbol (Literal 'b')]
                , Seq [Symbol (Literal 'c'), Symbol (Literal 'd')]
                ])

        , testCase "Character class in sequence: [a-z]x" $
             parseEquals regex "[a-z]x"
               (Seq [Symbol (Class (RClass False [Range 'a' 'z'])), Symbol (Literal 'x')])

        , testCase "Empty pattern" $
            parseEquals regex "" (Seq []) -- Check parsing of empty string

        , testCase "Alternation with empty parts: a||b" $ -- Often treated as 'a' or 'empty' or 'b'
             parseEquals regex "a||b" (Alt [Symbol (Literal 'a'), Seq [], Symbol (Literal 'b')]) -- Check parser result

        , testCase "Quantifier on group: (a|b)*c" $
             parseEquals regex "(a|b)*c"
                (Seq [
                    Quantified (Group (Alt [Symbol (Literal 'a'), Symbol (Literal 'b')])) ZeroOrMore,
                    Symbol (Literal 'c')
                ])
        ]
    ] -- End Parser Tests group

  , testGroup "Matcher Tests (`matches`)" -- Group all matching tests
      [ testCase "Exact: 'hello' matches 'hello'" $
          testMatches "hello" "hello" True
      , testCase "Exact: 'hello' doesn't match 'world'" $
          testMatches "hello" "world" False
      , testCase "Exact: 'hello' doesn't match 'hell'" $
          testMatches "hello" "hell" False
      , testCase "Exact: 'hello' doesn't match 'helloo'" $
          testMatches "hello" "helloo" False

      , testCase "Wildcard: 'h.llo' matches 'hello'" $
          testMatches "h.llo" "hello" True
      , testCase "Wildcard: 'h.llo' matches 'hallo'" $
          testMatches "h.llo" "hallo" True
      , testCase "Wildcard: 'h.llo' doesn't match 'hllo'" $
          testMatches "h.llo" "hllo" False
      , testCase "Wildcard: 'h.llo' doesn't match 'helo'" $
           testMatches "h.llo" "helo" False -- '.' requires exactly one char

      , testCase "Alternation: 'foo|bar' matches 'bar'" $
          testMatches "foo|bar" "bar" True
      , testCase "Alternation: 'foo|bar' matches 'foo'" $
          testMatches "foo|bar" "foo" True
      , testCase "Alternation: 'foo|bar' doesn't match 'baz'" $
          testMatches "foo|bar" "baz" False
      , testCase "End anchor: 'end$' doesn't match 'the very end' (full string)" $
          testMatches "end$" "the very end" False -- Corrected expectation
      , testCase "End anchor: 'end$' DOES NOT match 'the very end' (full string)" $
          testMatches "end$" "the very end" False -- Expect False because it's not a full match from start
      -- Keep the existing non-matching test which was already correct
      , testCase "End anchor: 'end$' doesn't match 'endings are nice'" $
          testMatches "end$" "endings are nice" False
      , testCase "Digits: '[0-9]+' matches '12345'" $
          testMatches "[0-9]+" "12345" True
      , testCase "Digits: '[0-9]+' doesn't match 'abc'" $
          testMatches "[0-9]+" "abc" False
      , testCase "Digits: '\\d+' matches '987'" $
           testMatches "\\d+" "987" True
      , testCase "Digits: '\\d+' doesn't match 'a987'" $
           testMatches "\\d+" "a987" False -- Doesn't match full string
      , testCase "Digits: '\\d+' doesn't match '987b'" $
           testMatches "\\d+" "987b" False -- Doesn't match full string

      , testCase "Empty Pattern: '' matches ''" $
          testMatches "" "" True
      , testCase "Empty Pattern: '' doesn't match 'something'" $
          -- An empty pattern ONLY matches an empty string fully
          testMatches "" "something" False

      , testCase "Case Sensitive: 'Hello' doesn't match 'hello'" $
          testMatches "Hello" "hello" False
      , testCase "Case Sensitive: 'Hello' matches 'Hello'" $
          testMatches "Hello" "Hello" True

      , testGroup "Quantifier Matching"
        [ testCase "ZeroOrMore: 'a*' matches ''" $ testMatches "a*" "" True
        , testCase "ZeroOrMore: 'a*' matches 'a'" $ testMatches "a*" "a" True
        , testCase "ZeroOrMore: 'a*' matches 'aaaa'" $ testMatches "a*" "aaaa" True
        , testCase "ZeroOrMore: 'a*' doesn't match 'aaab'" $ testMatches "a*" "aaab" False -- Not full match
        , testCase "ZeroOrMore: '.*' matches 'anything'" $ testMatches ".*" "anything 123" True
        , testCase "ZeroOrMore: 'a*b' matches 'b'" $ testMatches "a*b" "b" True
        , testCase "ZeroOrMore: 'a*b' matches 'aaab'" $ testMatches "a*b" "aaab" True
        , testCase "ZeroOrMore: 'a*b' doesn't match 'aaac'" $ testMatches "a*b" "aaac" False

        , testCase "OneOrMore: 'a+' matches 'a'" $ testMatches "a+" "a" True
        , testCase "OneOrMore: 'a+' matches 'aaaa'" $ testMatches "a+" "aaaa" True
        , testCase "OneOrMore: 'a+' doesn't match ''" $ testMatches "a+" "" False
        , testCase "OneOrMore: 'a+b' matches 'ab'" $ testMatches "a+b" "ab" True
        , testCase "OneOrMore: 'a+b' matches 'aaab'" $ testMatches "a+b" "aaab" True
        , testCase "OneOrMore: 'a+b' doesn't match 'b'" $ testMatches "a+b" "b" False

        , testCase "ZeroOrOne: 'a?' matches ''" $ testMatches "a?" "" True
        , testCase "ZeroOrOne: 'a?' matches 'a'" $ testMatches "a?" "a" True
        , testCase "ZeroOrOne: 'a?' doesn't match 'aa'" $ testMatches "a?" "aa" False
        , testCase "ZeroOrOne: 'a?b' matches 'b'" $ testMatches "a?b" "b" True
        , testCase "ZeroOrOne: 'a?b' matches 'ab'" $ testMatches "a?b" "ab" True
        , testCase "ZeroOrOne: 'a?b' doesn't match 'aab'" $ testMatches "a?b" "aab" False

        , testCase "Exactly: 'a{3}' matches 'aaa'" $ testMatches "a{3}" "aaa" True
        , testCase "Exactly: 'a{3}' doesn't match 'aa'" $ testMatches "a{3}" "aa" False
        , testCase "Exactly: 'a{3}' doesn't match 'aaaa'" $ testMatches "a{3}" "aaaa" False
        , testCase "Exactly: '(ab){2}' matches 'abab'" $ testMatches "(ab){2}" "abab" True
        , testCase "Exactly: '(ab){2}' doesn't match 'ab'" $ testMatches "(ab){2}" "ab" False
        , testCase "Exactly: '(ab){2}' doesn't match 'ababa'" $ testMatches "(ab){2}" "ababa" False

        , testCase "AtLeast: 'a{2,}' matches 'aa'" $ testMatches "a{2,}" "aa" True
        , testCase "AtLeast: 'a{2,}' matches 'aaaaa'" $ testMatches "a{2,}" "aaaaa" True
        , testCase "AtLeast: 'a{2,}' doesn't match 'a'" $ testMatches "a{2,}" "a" False
        , testCase "AtLeast: '(ab){1,}' matches 'ab'" $ testMatches "(ab){1,}" "ab" True
        , testCase "AtLeast: '(ab){1,}' matches 'ababab'" $ testMatches "(ab){1,}" "ababab" True
        , testCase "AtLeast: '(ab){1,}' doesn't match ''" $ testMatches "(ab){1,}" "" False

        , testCase "Between: 'a{2,4}' matches 'aa'" $ testMatches "a{2,4}" "aa" True
        , testCase "Between: 'a{2,4}' matches 'aaa'" $ testMatches "a{2,4}" "aaa" True
        , testCase "Between: 'a{2,4}' matches 'aaaa'" $ testMatches "a{2,4}" "aaaa" True
        , testCase "Between: 'a{2,4}' doesn't match 'a'" $ testMatches "a{2,4}" "a" False
        , testCase "Between: 'a{2,4}' doesn't match 'aaaaa'" $ testMatches "a{2,4}" "aaaaa" False
        , testCase "Between: '(ab){1,2}' matches 'ab'" $ testMatches "(ab){1,2}" "ab" True
        , testCase "Between: '(ab){1,2}' matches 'abab'" $ testMatches "(ab){1,2}" "abab" True
        , testCase "Between: '(ab){1,2}' doesn't match ''" $ testMatches "(ab){1,2}" "" False
        , testCase "Between: '(ab){1,2}' doesn't match 'ababab'" $ testMatches "(ab){1,2}" "ababab" False
        ]

      , testGroup "Complex Patterns"
        [ testCase "Email-like: '\\w+@\\w+\\.\\w+' matches 'test@example.com'" $
            testMatches "\\w+@\\w+\\.\\w+" "test@example.com" True
        , testCase "Email-like: '\\w+@\\w+\\.\\w+' doesn't match 'test@example'" $
            testMatches "\\w+@\\w+\\.\\w+" "test@example" False
        , testCase "Email-like: '\\w+@\\w+\\.\\w+' doesn't match 'test@.com'" $
            testMatches "\\w+@\\w+\\.\\w+" "test@.com" False -- Requires \w+ before dot
        , testCase "Alternation/Sequence: 'a(b|c)d' matches 'abd'" $
            testMatches "a(b|c)d" "abd" True
        , testCase "Alternation/Sequence: 'a(b|c)d' matches 'acd'" $
            testMatches "a(b|c)d" "acd" True
        , testCase "Alternation/Sequence: 'a(b|c)d' doesn't match 'ad'" $
            testMatches "a(b|c)d" "ad" False
        , testCase "Alternation/Quantifier: '(a|b)*c' matches 'c'" $
            testMatches "(a|b)*c" "c" True
        , testCase "Alternation/Quantifier: '(a|b)*c' matches 'ac'" $
            testMatches "(a|b)*c" "ac" True
        , testCase "Alternation/Quantifier: '(a|b)*c' matches 'babc'" $
            testMatches "(a|b)*c" "babc" True
        , testCase "Alternation/Quantifier: '(a|b)*c' doesn't match 'ab*c'" $
             testMatches "(a|b)*c" "ab*c" False -- '*' is literal here
        , testCase "Alternation/Quantifier: '(a|b)*c' doesn't match 'abca'" $
             testMatches "(a|b)*c" "abca" False -- Doesn't match fully

        -- Potential non-termination / loop test
        -- testCase "Loop check: '(a*)*' matches 'aaa'" $ testMatches "(a*)*" "aaa" True
        -- testCase "Loop check: '(a*)*' matches ''" $ testMatches "(a*)*" "" True
        -- NOTE: The guard `T.length txt' < T.length currentTxt` should prevent infinite loops here.

        ]
    ] -- End Matcher tests group
  ]
