{-# LANGUAGE OverloadedStrings #-}
module MyLib where

import Text.Megaparsec hiding (single, match)
import Text.Megaparsec.Char
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Data.Functor.Identity (Identity)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.List (nub) -- Import nub for removing duplicates
import Control.Monad (guard) -- Import guard for filtering in list monad
import Data.Maybe (isJust)

type Parser a = ParsecT Void Text Identity a

-- DATA TYPES (Unchanged from original) -----------------------

-- The main regex tree
data Regex
  = Seq [Regex]         -- Sequence of regex elements
  | Alt [Regex]         -- Alternation (|)
  | Quantified Regex Quant -- Quantified expression (*, +, ?, {n,m})
  | Group Regex         -- Grouping (e.g., (abc))
  | Symbol RegexSymbol    -- Basic symbols like literal, dot, anchors
  deriving (Show, Eq)

-- Quantifiers for repetitions
data Quant
  = ZeroOrMore        -- *
  | OneOrMore         -- +
  | ZeroOrOne         -- ?
  | Exactly Int       -- {n}
  | AtLeast Int       -- {n,}
  | Between Int Int   -- {n,m}
  deriving (Show, Eq)

data Escaped
  = WordChar      -- \w
  | NotWordChar   -- \W
  | DigitChar     -- \d
  | NotDigitChar  -- \D
  | SpaceChar     -- \s
  | NotSpaceChar  -- \S
  | TabChar       -- \t
  | NewlineChar   -- \n
  | ReturnChar    -- \r
  | FormFeedChar  -- \f
  | BellChar      -- \a
  | EscapeChar    -- \e
  | BackspaceChar -- \b
  | LiteralChar Char -- any escaped literal like \* or \. or \\
  deriving (Eq, Show)

-- Basic building blocks
data RegexSymbol
  = Caret             -- ^ (Start of string anchor) - Needs context!
  | Dollar            -- $ (End of string anchor)
  | Any               -- . (dot)
  | Literal Char      -- A literal character
  | Escape Escaped    -- An escaped character (\d, \w, etc.)
  | Class RClass      -- Character class like [a-z]
  deriving (Show, Eq)

-- Character class definition
data RClass = RClass
  { negated :: Bool         -- True if [^...]
  , elements :: [RClassElem]   -- List of elements
  }
  deriving (Show, Eq)

-- Elements inside a character class
data RClassElem
  = Single Char           -- Single character like 'a'
  | Range Char Char         -- Range like 'a'-'z'
  deriving (Show, Eq)


-- PARSERS (Mostly unchanged, slight adjustments if needed) --------


-- Top-level: alternation using '|'
regex :: Parser Regex
regex = do
  -- Use sepBy to allow potentially zero parts (e.g., empty input or just '|')
  parts <- sepBy regexSeq (char '|')
  pure $ case parts of
    []       -> Seq []     -- Empty pattern parses to empty sequence
    [single] -> single     -- Single part doesn't need Alt
    xs       -> Alt xs       -- Multiple parts become an Alt

-- Sequence of quantified expressions
regexSeq :: Parser Regex
regexSeq = do
  -- Use 'many' instead of 'some' to allow an empty sequence of expressions
  exprs <- many regexQuantified
  pure $ case exprs of
    []       -> Seq []     -- Explicitly handle empty sequence result
    [single] -> single     -- Single expression doesn't need Seq
    xs       -> Seq xs       -- Multiple expressions become a Seq

-- Parse a quantified atom
regexQuantified :: Parser Regex
regexQuantified = do
  atom <- regexAtom
  mq <- optional parseQuant -- Try to parse a quantifier
  pure $ case mq of
    Nothing -> atom          -- No quantifier, just the atom
    Just q  -> Quantified atom q -- Atom with quantifier

-- Atomic unit of regex (group, symbol, character class, escaped)
regexAtom :: Parser Regex
regexAtom = choice
  [ Group <$> between (char '(') (char ')') regex -- Group needs to parse full regex inside
  , Symbol <$> parseRegexSymbol
  -- No fallback needed if `some` is used correctly in regexSeq
  ]

specialRegexChars :: String
specialRegexChars = "^$.[\\|()*+?{}" -- Chars with special meaning in regex patterns

parseRegexSymbol :: Parser RegexSymbol
parseRegexSymbol = choice $ [
    Caret <$ char '^'
  , Dollar <$ char '$'
  , Any <$ char '.'
  , Escape <$> escapedParser
  , Class <$> classParser
  , Literal <$> noneOf specialRegexChars -- Match non-special chars literally
  -- Handling escaped special chars happens within escapedParser now
  ]

escapedParser :: Parser Escaped
escapedParser = char '\\' *> choice
  [ WordChar      <$ char 'w'
  , NotWordChar   <$ char 'W'
  , DigitChar     <$ char 'd'
  , NotDigitChar  <$ char 'D'
  , SpaceChar     <$ char 's'
  , NotSpaceChar  <$ char 'S'
  , TabChar       <$ char 't'
  , NewlineChar   <$ char 'n'
  , ReturnChar    <$ char 'r'
  , FormFeedChar  <$ char 'f'
  , BellChar      <$ char 'a'
  , EscapeChar    <$ char 'e'
  , BackspaceChar <$ char 'b'
  -- Fallback: handle escaped special characters like \. \* \\ etc.
  , LiteralChar   <$> oneOf ("^$.[\\|()*+?{}" :: String) -- Match escaped special chars
  , LiteralChar   <$> char '\\' -- Match escaped backslash explicitly
  -- Consider adding more specific escapes if needed (e.g., \xHH, \uHHHH)
  -- If none of the above match, it's an invalid escape sequence
  , fail "Invalid escape sequence"
  ]

classParser :: Parser RClass
classParser = do
  _ <- char '['
  neg <- isJust <$> optional (char '^') -- Check for negation
  elems <- manyTill classElem (char ']') -- Use manyTill for elements until ']'
  return RClass { negated = neg, elements = elems }

-- Parses elements within a character class [ ]
classElem :: Parser RClassElem
classElem = try rangeParser <|> singleParser
  where
    -- Try to parse a range like 'a-z'
    rangeParser :: Parser RClassElem
    rangeParser = do
      start <- classChar -- Character that can start a range
      _ <- char '-'      -- The dash
      end <- classChar   -- Character that can end a range
      if start <= end
        then return $ Range start end
        else fail $ "Invalid range in character class: " ++ [start] ++ "-" ++ [end]

    -- Parse a single character element
    singleParser :: Parser RClassElem
    singleParser = Single <$> classChar

    -- Characters allowed inside classes (can be literal or escaped)
    -- Avoids consuming ']' or '-' in the wrong context
    classChar :: Parser Char
    classChar = noneOf ("\\]-" :: String) -- Basic literal char
            <|> (escapedClassChar >>= checkEscape) -- Or an escaped char

    -- Special handling for escapes within char classes if needed (e.g., \d, \s)
    -- Basic version: just treat escaped chars as literals for now
    escapedClassChar :: Parser Char
    escapedClassChar = char '\\' *> anySingle -- Allow any escaped character literally

    -- Placeholder: Check if the escaped char itself is valid in a class
    -- Currently just returns the char, might need more logic for \d etc.
    checkEscape :: Char -> Parser Char
    checkEscape c = pure c -- For now, accept any escaped char literally


parseQuant :: Parser Quant
parseQuant = choice $ [
    ZeroOrMore <$ char '*',
    OneOrMore  <$ char '+',
    ZeroOrOne  <$ char '?',
    parseRangeQuant
  ]
  where
  parseRangeQuant :: Parser Quant
  parseRangeQuant = do
    _ <- char '{'
    n <- decimal
    m_opt <- optional (char ',' *> optional decimal)
    _ <- char '}'
    case m_opt of
      Nothing        -> return $ Exactly n      -- {n}
      Just Nothing   -> return $ AtLeast n      -- {n,}
      Just (Just m') -> if n <= m'
                         then return $ Between n m' -- {n,m}
                         else fail $ "Invalid quantifier range: {" ++ show n ++ "," ++ show m' ++ "}"


-- MATCHING LOGIC (Refactored) ------------------------------

-- Helper: Match a single Char against RClassElem
matchCharClassElem :: Char -> RClassElem -> Bool
matchCharClassElem c (Single x) = c == x
matchCharClassElem c (Range start end) = c >= start && c <= end

-- Helper: Match a single Char against RClass
matchCharClass :: Char -> RClass -> Bool
matchCharClass c cls =
  let doesMatch = or $ map (matchCharClassElem c) (elements cls)
  in if negated cls then not doesMatch else doesMatch

-- Helper: Match a single Char against Escaped
matchEscaped :: Char -> Escaped -> Bool
matchEscaped c WordChar      = isAlphaNum c || c == '_' -- \w
matchEscaped c NotWordChar   = not (matchEscaped c WordChar) -- \W
matchEscaped c DigitChar     = isDigit c        -- \d
matchEscaped c NotDigitChar  = not (isDigit c)  -- \D
matchEscaped c SpaceChar     = isSpace c        -- \s
matchEscaped c NotSpaceChar  = not (isSpace c)  -- \S
matchEscaped c TabChar       = c == '\t'        -- \t
matchEscaped c NewlineChar   = c == '\n'        -- \n
matchEscaped c ReturnChar    = c == '\r'        -- \r
matchEscaped c FormFeedChar  = c == '\f'        -- \f
matchEscaped c BellChar      = c == '\a'        -- \a
matchEscaped c EscapeChar    = c == '\ESC'      -- \e (ASCII escape)
matchEscaped c BackspaceChar = c == '\b'        -- \b
matchEscaped c (LiteralChar lc) = c == lc       -- \. \* \\ etc.


-- Core matching function: returns a list of remaining texts
-- An empty list indicates no match.
match :: Regex -> Text -> [Text]
match (Alt ps) txt = nub $ concatMap (\p -> match p txt) ps -- Combine results from all alternatives
match (Seq ps) txt = matchSeq ps txt                     -- Delegate to sequence helper
match (Group p) txt = match p txt                        -- Groups don't change matching logic (just precedence/capture)
match (Symbol s) txt = matchSymbol s txt                 -- Delegate to symbol helper
match (Quantified p q) txt = nub $ matchQuant p q txt    -- Delegate to quantifier helper, remove duplicates


-- Helper for matching sequences
matchSeq :: [Regex] -> Text -> [Text]
matchSeq [] txt = [txt] -- Base case: empty sequence matches, consumes nothing, returns current text
matchSeq (p:ps) txt = do
    -- Monadic bind for lists:
    -- For each way 'p' can match 'txt' producing a remainder 'txt''...
    txt' <- match p txt
    -- ...recursively match the rest of the sequence 'ps' against 'txt''
    matchSeq ps txt'
    -- The list monad automatically collects all successful results


-- Helper for matching basic symbols
matchSymbol :: RegexSymbol -> Text -> [Text]
matchSymbol Caret _txt =
    -- Error: Cannot implement '^' correctly without knowing if we are at the
    -- *absolute beginning* of the original input string.
    -- Returning [] as it doesn't successfully yield a next state based on current text alone.
    -- A full implementation would need context (e.g., `match :: Bool -> Regex -> Text -> [Text]`)
    []
matchSymbol Dollar txt
    | T.null txt = [txt] -- Matches end of string: Success, consumes nothing
    | otherwise  = []    -- Not at end of string: Fail
matchSymbol Any txt
    | T.null txt = []             -- Cannot match '.' on empty string
    | otherwise  = [T.tail txt]   -- Matches any single char, consumes it
matchSymbol (Literal c) txt
    | not (T.null txt) && T.head txt == c = [T.tail txt] -- Match literal, consume
    | otherwise                           = []             -- No match
matchSymbol (Escape e) txt
    | not (T.null txt) && matchEscaped (T.head txt) e = [T.tail txt] -- Match escape, consume
    | otherwise                                       = []             -- No match
matchSymbol (Class cls) txt
    | not (T.null txt) && matchCharClass (T.head txt) cls = [T.tail txt] -- Match class, consume
    | otherwise                                           = []             -- No match

-- Helper for matching quantified expressions
matchQuant :: Regex -> Quant -> Text -> [Text]
matchQuant p ZeroOrOne txt =
    -- Try matching p once. Include results. Also include the original text (matching p zero times).
    match p txt ++ [txt] -- Nub handled by the caller 'match'

matchQuant p ZeroOrMore txt = matchStar p txt
  where
    -- Helper for *: matches p zero or more times (greedy implied by structure)
    matchStar :: Regex -> Text -> [Text]
    matchStar _p currentTxt =
        -- Result includes matching 0 times from current position
        currentTxt :
        -- Results from matching p 1 or more times from current position
        (do txt' <- match p currentTxt
            -- IMPORTANT: Guard against infinite loops if 'p' matches an empty string.
            -- If p matched but consumed no text, don't recurse further down this path.
            guard (T.length txt' < T.length currentTxt)
            -- If p consumed text, recursively match p* starting from the new position txt'
            matchStar p txt'
        )
        -- Nub handled by the caller 'match'

matchQuant p OneOrMore txt = matchPlus p txt
  where
    -- Helper for +: matches p one or more times
    matchPlus :: Regex -> Text -> [Text]
    matchPlus _p currentTxt = do
        -- Must match p at least once
        txt' <- match p currentTxt
        -- Now match p zero or more times (like *) starting from txt'
        -- Result includes the state after the first match (txt') AND recursive matches
        let results_after_one =
                txt' : -- Include the result of matching just once
                (do txt'' <- match p txt'
                    guard (T.length txt'' < T.length txt') -- Prevent loop
                    matchPlus' txt'' -- Recursively match 1+ times from txt''
                )
        results_after_one -- Return combined results

    -- Inner helper to avoid re-matching the first 'p' in recursion for '+'
    matchPlus' :: Text -> [Text]
    matchPlus' currentTxt =
      currentTxt :
      (do txt' <- match p currentTxt
          guard (T.length txt' < T.length currentTxt)
          matchPlus' txt'
      )
      -- Nub handled by the caller 'match'


matchQuant p (Exactly n) txt
    | n < 0     = error "Negative quantifier value"
    | n == 0    = [txt] -- Match exactly 0 times: success, consume nothing
    | otherwise = do
        txt' <- match p txt -- Match p once
        -- Recursively match p exactly (n-1) times from the remainder
        matchQuant p (Exactly (n - 1)) txt'
        -- Nub handled by the caller 'match'

matchQuant p (AtLeast n) txt = do
    -- First, match exactly n times
    txt_n <- matchQuant p (Exactly n) txt
    -- Then, match zero or more times from each resulting text
    matchQuant p ZeroOrMore txt_n
    -- Nub handled by the caller 'match'

matchQuant p (Between n m) txt
    | n < 0 || m < 0 || n > m = error "Invalid quantifier range"
    | otherwise = do
        -- First, match exactly n times
        txt_n <- matchQuant p (Exactly n) txt
        -- Then, match optionally up to (m-n) additional times
        matchOptional (m - n) txt_n
  where
    -- Helper to match p between 0 and 'k' times
    matchOptional :: Int -> Text -> [Text]
    matchOptional k currentTxt
        | k <= 0 = [currentTxt] -- Cannot or don't need to match more
        | otherwise =
            -- Include the current text (matching 0 more times)
            currentTxt :
            -- Try matching p one more time
            (do txt' <- match p currentTxt
                -- Guard against non-consuming loops AND exceeding max count
                guard (T.length txt' < T.length currentTxt)
                -- Recursively match up to k-1 more times
                matchOptional (k - 1) txt'
            )
            -- Nub handled by the caller 'match'


-- USER-FACING FUNCTION (Optional but common) -------------

-- Check if the *entire* text matches the regex.
-- This uses the internal 'match' function and checks if any
-- of the resulting remaining texts are empty.
matches :: Regex -> Text -> Bool
matches r text =
  let remainingTexts = match r text
  in any T.null remainingTexts

-- Example Usage (in GHCi or a main function):
-- > :load MyLib.hs
-- > let Right r = parse regex "" "a(b|c)*d"
-- > matches r "abbbcd"
-- True
-- > matches r "ad"
-- True
-- > matches r "abd"
-- True
-- > matches r "acd"
-- True
-- > matches r "ac"
-- False
-- > matches r "abbbcde"
-- False
-- > let Right r2 = parse regex "" "a{2,3}"
-- > matches r2 "aa"
-- True
-- > matches r2 "aaa"
-- True
-- > matches r2 "a"
-- False
-- > matches r2 "aaaa"
-- False
-- > let Right r3 = parse regex "" "[^a-c]+"
-- > matches r3 "xyz"
-- True
-- > matches r3 "ax"
-- False
