module MyLib (Regex(..), RegexSymbol(..), RClass(..), RClassElem(..), Quant(..),regex, match) where
regex :: String -> Regex
regex str = undefined
match :: Regex -> String -> Bool
match reg str = undefined

-- The main regex tree
data Regex
  = Seq [Regex]            -- Sequence of regex elements
  | Alt [Regex]            -- Alternation (|)
  | Quantified Regex Quant -- Quantified expression (*, +, ?, {n,m})
  | Group Regex            -- Grouping (e.g., (abc))
  | Symbol RegexSymbol     -- Basic symbols like literal, dot, anchors
  deriving (Show, Eq)

-- Quantifiers for repetitions
data Quant
  = ZeroOrMore            -- *
  | OneOrMore             -- +
  | ZeroOrOne             -- ?
  | RangeQuant Int (Maybe Int) -- {n} or {n,} or {n,m}
  deriving (Show, Eq)

-- Basic building blocks
data RegexSymbol
  = Caret                 -- ^
  | Dollar                -- $
  | Any                   -- . (dot)
  | Literal Char          -- A literal character
  | Escape Char           -- An escaped character (\d, \w, etc.)
  | Class RClass          -- Character class like [a-z]
  deriving (Show, Eq)

-- Character class definition
data RClass = RClass
  { negated :: Bool              -- True if [^...]
  , elements :: [RClassElem]      -- List of elements
  }
  deriving (Show, Eq)

-- Elements inside a character class
data RClassElem
  = Single Char                  -- Single character like 'a'
  | Range Char Char              -- Range like 'a'-'z'
  | PosixClass String            -- [:digit:], [:alpha:], etc.
  deriving (Show, Eq)
