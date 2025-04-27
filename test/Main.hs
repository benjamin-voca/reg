-- test/RegexTest.hs
module Main (main) where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Data.Text as T

import MyLib (regex, match)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Regex Tests"
  [ testCase "Simple exact match" $
      match (regex "hello") (T.pack "hello") @?= True

  , testCase "Simple mismatch" $
      match (regex "hello") (T.pack "world") @?= False

  , testCase "Wildcard match" $
      match (regex "h.llo") (T.pack "hello") @?= True

  , testCase "Multiple matches" $
      match (regex "foo|bar") (T.pack "bar") @?= True

  , testCase "Start anchor match" $
      match (regex "^start") (T.pack "start of line") @?= True

  , testCase "Start anchor mismatch" $
      match (regex "^start") (T.pack "in the start") @?= False

  , testCase "End anchor match" $
      match (regex "end$") (T.pack "the very end") @?= True

  , testCase "End anchor mismatch" $
      match (regex "end$") (T.pack "endings are nice") @?= False

  , testCase "Digit matching" $
      match (regex "[0-9]+") (T.pack "12345") @?= True

  , testCase "No match for digits in text" $
      match (regex "[0-9]+") (T.pack "abc") @?= False

  , testCase "Empty pattern matches empty text" $
      match (regex "") (T.pack "") @?= True

  , testCase "Empty pattern matches non-empty text" $
      match (regex "") (T.pack "something") @?= True

  , testCase "Case sensitive match (fail)" $
      match (regex "Hello") (T.pack "hello") @?= False
  ]
