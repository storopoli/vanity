{- |
Module: Crypto.Bitcoin.Vanity.Pattern
Copyright: (c) 2025 Jose Storopoli
License: MIT
Maintainer: Jose Storopoli <jose@storopoli.com>

Regex pattern matching for vanity addresses.

Uses POSIX extended regular expressions via regex-tdfa for efficient matching.
-}
module Crypto.Bitcoin.Vanity.Pattern (
  -- * Pattern Compilation
  compilePattern,
  CompiledPattern,

  -- * Pattern Matching
  matchPattern,

  -- * Validation
  validatePattern,
  PatternError (..),
) where

import Crypto.Bitcoin.Vanity.Types (Address (..), Pattern (..))
import Data.Text (Text)
import Data.Text qualified as T
import Text.Regex.TDFA (Regex, caseSensitive, defaultCompOpt, defaultExecOpt, makeRegexOptsM, matchTest)
import Text.Regex.TDFA.Text ()

-- | A compiled regex pattern for efficient repeated matching.
newtype CompiledPattern = CompiledPattern Regex

-- | Pattern compilation/validation errors.
data PatternError
  = -- | The pattern string is empty
    EmptyPattern
  | -- | The pattern contains invalid regex syntax
    InvalidRegex Text
  deriving (Show, Eq)

{- | Compile a pattern into a compiled pattern for efficient repeated matching.

The second parameter controls case sensitivity:
- 'True': Case-sensitive matching (default)
- 'False': Case-insensitive matching
-}
compilePattern :: Pattern -> Bool -> Either PatternError CompiledPattern
compilePattern (Pattern pat) isCaseSensitive
  | T.null pat = Left EmptyPattern
  | otherwise =
      let compOpts = defaultCompOpt{caseSensitive = isCaseSensitive}
       in case makeRegexOptsM compOpts defaultExecOpt pat of
            Nothing -> Left (InvalidRegex pat)
            Just regex -> Right (CompiledPattern regex)

{- | Check if an address matches the compiled pattern.

Returns 'True' if the address text matches the regex pattern.
-}
matchPattern :: CompiledPattern -> Address -> Bool
matchPattern (CompiledPattern regex) (Address addr) = matchTest regex addr

{- | Validate a pattern without full compilation.

Returns 'Right ()' if the pattern is valid, 'Left PatternError' otherwise.
-}
validatePattern :: Pattern -> Either PatternError ()
validatePattern pat = compilePattern pat True >> Right ()
