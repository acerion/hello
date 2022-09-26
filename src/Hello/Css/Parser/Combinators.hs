{-
Copyright (C) 2021-2022 Kamil Ignacak acerion@wp.pl

This file is part of "hello" web browser.

"hello" is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

"hello" is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with "hello".  If not, see <https://www.gnu.org/licenses/>.
-}




module Hello.Css.Parser.Combinators
  (
    MyParser (..)
  , combinatorExactlyOne
  , combinatorAllInOrder
  , combinatorOneOrMoreUnordered
  , multiplierZeroOrOnce
  , multiplierOnce
  )
where




--import Debug.Trace

import Data.List as L




{-
Helpers for new approach to parsing of shortcut properties.

The helpers will allow me to rewrite specification of CSS property values
(e.g. "[ [ <'font-style'> || <'font-variant'> || <'font-weight'> ]? ]") in
terms of Haskell.

Poor man's implementation of:
https://www.w3.org/TR/css-values-3/#component-combinators
https://www.w3.org/TR/css-values-3/#component-multipliers

Also a busy man's implementation, because I don't have time to read through
the CSS spec end-to-end and recognize all usage cases, corner cases, rules
and exceptions.
-}




-- I think that due to [], this is not a canonical form of parser function.
-- But it works for me now.
type MyParser p a = p -> (p, Maybe [a])




-- Run all parsers in given order
--
-- Return success (Just accumulator) if exactly one of them succeeds.
-- Return failure (Nothing) if zero, two or more of them succeed.
--
-- https://www.w3.org/TR/css-values-3/#component-combinators
-- "A bar (|) separates two or more alternatives: exactly one of them must occur."
--
-- Unit tested? Yes.
combinatorExactlyOne :: [MyParser p a] -> p -> (p, Maybe [a])
combinatorExactlyOne fs pat = if countOfNonEmpty == 1
                              then (outPat, Just $ L.concat accumulators)
                              else (pat, Nothing)

  where
    countOfNonEmpty = L.length $ L.filter (\l -> not . L.null $ l) accumulators
    (outPat, accumulators) = runFunctions fs (pat, [])




-- Run all parsers in given order
--
-- Return success (Just accumulator) if all of the parsers succeeded (returned some accumulator).
-- Return failure (Nothing) if one or more of parsers failed (returned Nothing).
--
-- https://www.w3.org/TR/css-values-3/#component-combinators
-- "Juxtaposing components means that all of them must occur, in the given order."
--
-- Unit tested? Yes.
combinatorAllInOrder :: [MyParser p a] -> p -> (p, Maybe [a])
combinatorAllInOrder functions pat = if countOfNonEmpty == L.length functions
                                     then (outPat, Just $ L.concat accumulators)
                                     else (pat, Nothing)

  where
    countOfNonEmpty        = (L.length accumulators)
    (outPat, accumulators) = runFunctions functions (pat, [])




-- Run all parsers, not necessarily in specified order
--
-- Return success (Just accumulator of all successes) if at least one of the parsers succeeded (returned some accumulator).
-- Return failure (Nothing) if all parsers failed (returned Nothing).
--
-- TODO: the "unordered" part is not being implemented. The values that
-- aren't in specific order aren't parsed correctly. Currently the only
-- accepted order is the one dictated by order of parser functions.
--
-- https://www.w3.org/TR/css-values-3/#component-combinators
-- "A double bar (||) separates two or more options: one or more of them must occur, in any order."
combinatorOneOrMoreUnordered :: [MyParser p a] -> p -> (p, Maybe [a])
combinatorOneOrMoreUnordered functions pat = if successes >= 1
                                             then (outPat, Just $ L.concat accumulators)
                                             else (pat, Nothing)

  where
    successes = L.length accumulators
    (outPat, accumulators) = runFunctions functions (pat, [])






-- Multiplier: run given parser (function) multiple times as long as the
-- parser is able to successfully take a parser-specific expression. See how
-- many times the parser was executed successfully (how many consecutive
-- parser-specific expressions were consumed successfully).
--
-- An universal multiplier function that can be used by more specialized
-- multipliers.
--
-- First two arguments specify count of successful calls of parser, as a
-- range. If given parser function successfully takes N consecutive
-- expressions, and N is between lower and upper (inclusive), then multiplier
-- succeeds.
--
-- If 'upper' is (-1) then (in theory) there is no upper limit on count of
-- successes (count of times the parser will be called to get next matching
-- expression). In practice the count of successful calls will be limited to
-- some hardcoded value to avoid infinite loop on malformed or malicious
-- input.
multiplier :: Int -> Int -> MyParser p a -> p -> (p, Maybe [a])
multiplier lower upper function pat | len >= lower && len <= upper = (pat'', Just . L.concat $ result)
                                    | otherwise                    = (pat, Nothing)
  where
    (pat'', result) = callUntilFail function pat [] hardLimit
    len = L.length result

    -- https://www.w3.org/TR/css-values-3/#component-multipliers spec
    -- indicates that implementation must support "at least 20 repetitions"
    -- in case of "X or more" multipliers. Therefore the hardcoded upper
    -- limit is "20".
    --
    -- TODO: write tests for "one or more" multiplier that would show that
    -- the hard limit is observed by this function.
    --
    -- TODO: the function must implement this requirement from the spec: "If
    -- a property value contains more than the supported number of
    -- repetitions, the declaration must be ignored as if it were invalid.".
    hardLimit | upper >= 0 = upper + 1
              | otherwise  = 20

    -- Call given function (either a parser or a combinator) multiple times.
    -- When the function fails to take a parser-specific expression, return
    -- accumulated result of all of the calls that succeeded.
    callUntilFail :: MyParser p a -> p -> [[a]] -> Int -> (p, [[a]])
    callUntilFail fun patArg accs limit = case fun patArg of
                                            (pat', Just acc') | L.length accs > limit -> (pat', accs)
                                                              | otherwise             -> callUntilFail fun pat' (accs ++ [acc']) limit
                                            (_, Nothing)                              -> (patArg, accs)




-- Multiplier: run given parser (function) multiple times as long as the
-- parser is able to successfully take a parser-specific expression. See how
-- many times the parser was executed successfully (how many consecutive
-- parser-specific expressions were consumed successfully).
--
-- Return 'Just acc' in property accumulator if count of successes is
-- correct: is either zero or one. If count of successes is higher than one,
-- return failure (Nothing).
--
-- https://www.w3.org/TR/css-values-3/#component-multipliers
-- "A question mark (?) indicates that the preceding type, word, or group is
-- optional (occurs zero or one times)."
--
-- Unit tested? Yes.
multiplierZeroOrOnce :: MyParser p a -> p -> (p, Maybe [a])
multiplierZeroOrOnce = multiplier lower upper
  where
    -- Range of expected successes (inclusive).
    lower = 0
    upper = 1




-- Multiplier: run given parser (function) multiple times as long as the
-- parser is able to successfully take a parser-specific expression. See how
-- many times the parser was executed successfully (how many consecutive
-- parser-specific expressions were consumed successfully).
--
-- Return 'Just acc' in property accumulator if count of successes (count of
-- successfuly taken expressions) is correct: is exactly one. If count of
-- successes is lower or higher than one, return failure (Nothing).
--
-- This multiplier is not described in CSS spec
-- (https://www.w3.org/TR/css-values-3/#component-multipliers), so maybe
-- after all it's not necessary, but for some reason I thought that it would
-- be convenient to explicitly indicate that some expressions in input CSS
-- should appear exactly once. TODO: check if we really need this multiplier.
--
-- Unit tested? Yes.
multiplierOnce :: MyParser p a -> p -> (p, Maybe [a])
multiplierOnce = multiplier lower upper
  where
    -- Range of expected successes (inclusive).
    lower = 1
    upper = 1




concatAccs :: Maybe [a] -> [[a]] -> [a]
concatAccs mAcc accumulators = case mAcc of
                                 Just acc -> acc ++ L.concat accumulators
                                 Nothing  -> []  ++ L.concat accumulators




concatAcc :: Maybe [a] -> [a] -> [a]
concatAcc mAcc acc = case mAcc of
                       Just acc' -> acc' ++ acc
                       Nothing   -> []   ++ acc




runFunctions :: [MyParser p a] -> (p, [[a]]) -> (p, [[a]])
runFunctions (f:fs) (pat, accumulators) = runFunctions fs (pat', acc')
  where (pat', acc') = case f pat of
                         (pat'', Just acc'') -> (pat'', accumulators ++ [acc''])
                         (_, Nothing)        -> (pat,   accumulators)
runFunctions []     (pat, accumulators) = (pat, accumulators)





