{-
Copyright (C) 2021-2023 Kamil Ignacak acerion@wp.pl

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




{-# LANGUAGE ScopedTypeVariables #-}




module Hello.Css.Parser.Combinators
  (
    multiplierZeroOrOnce
  , multiplierOnce

  , combinatorOneOrMoreUnordered
  , combinatorAllInOrder
  , combinatorExactlyOne

  -- These are only for testing combinators/parsers in ghci.
  {-
  , parserA
  , parserB
  , parserC
  -}
  )
where




--import Debug.Trace

--import qualified Data.Text as T
import Data.List as L
import Data.Maybe

import Hello.Utils.Parser




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




{-
parserA :: Parser T.Text (String -> String)
parserA = Parser $ \ text -> case T.uncons text of
                               Just (c, remd) | c == 'a' -> Just (remd, (:) 'a')
                                              | otherwise -> Nothing
                               Nothing -> Nothing


parserB :: Parser T.Text (String -> String)
parserB = Parser $ \ text -> case T.uncons text of
                               Just (c, remd) | c == 'b' -> Just (remd, (:) 'b')
                                              | otherwise -> Nothing
                               Nothing -> Nothing


parserC :: Parser T.Text (String -> String)
parserC = Parser $ \ text -> case T.uncons text of
                               Just (c, remd) | c == 'c' -> Just (remd, (:) 'c')
                                              | otherwise -> Nothing
                               Nothing -> Nothing
-}




-- Apply given parser functions to given state unil none of them matches.
-- Parsers save result in given input acc.
--
-- Succeed if one or more of the parser functions succeeded and has taken
-- some input from front of state.
-- Fail if none of parser functions succeeded in taking any input from front of state.
--
-- Each parser is discarded after successful application. Function tries
-- until all parsers are discarded after their succeses, or until first
-- parser fails. Parser is discarded because it has been already successfully
-- used once, and can't be used for a second time.
--
-- Return Just (new state, modified acc) on success.
-- Return Nothing on failure.
{-
:m +Hello.Utils.Parser
:m +Data.Maybe
:m +Data.Text
:m +Hello.Css.Parser.Combinators
:set prompt >

(fmap . fmap) (\ f -> f "") (runParser (combinatorOneOrMoreUnordered [parserB, parserA]) "ab")
-}
combinatorOneOrMoreUnordered :: [Parser state (a -> a)] -> Parser state (a -> a)
combinatorOneOrMoreUnordered parsers = Parser $ \ state ->
  case tryParsersUnordered state parsers stash fs 0 of
    ((state', fs'), i) | i >= 1    -> Just (state', fs')
                       | otherwise -> Nothing
  where
    stash = [] -- Stash for parsers, necessary for algo of running parsers unordered.
    fs    = id -- Accumulator of functions. 'id' is an empty element for function composition.




-- Run all parsers in given order
--
-- Return success (Just accumulator) if all of the parsers succeeded (returned some accumulator).
-- Return failure (Nothing) if one or more of parsers failed (returned Nothing).
--
-- https://www.w3.org/TR/css-values-3/#component-combinators
-- "Juxtaposing components means that all of them must occur, in the given order."
--
-- Unit tested? Yes.
{-
:m +Hello.Utils.Parser
:m +Data.Maybe
:m +Data.Text
:m +Hello.Css.Parser.Combinators
:set prompt >

(fmap . fmap) (\ f -> f "") (runParser (combinatorAllInOrder [parserA, parserB]) "abC")
-}
combinatorAllInOrder :: [Parser state (a -> a)] -> Parser state (a -> a)
combinatorAllInOrder parsers = Parser $ \ state -> runAllParsers parsers state id




-- Run all parsers from given list. Each parser is ran with state updated by
-- previous parsers' calls. All parsers must succeed.
runAllParsers :: [Parser state (a -> a)] -> state -> (a -> a) -> Maybe (state, (a -> a))
runAllParsers (parser:parsers) state fs = case runParser parser state of
                                            Just (state', f) -> runAllParsers parsers state' (f . fs)
                                            Nothing          -> Nothing
runAllParsers []               state fs = Just (state, fs)



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
{-
:m +Hello.Utils.Parser
:m +Data.Maybe
:m +Data.Text
:m +Hello.Css.Parser.Combinators
:set prompt >

(fmap . fmap) (\ f -> f "") (runParser (multiplierZeroOrOnce (combinatorAllInOrder [parserB, parserA])) "ab")
-}
multiplierZeroOrOnce ::  (Show state) => Parser state (a -> a) -> Parser state (a -> a)
multiplierZeroOrOnce parser = Parser $ \ pat -> multiplier lower upper parser pat
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
multiplierOnce ::  (Show state) => Parser state (a -> a) -> Parser state (a -> a)
multiplierOnce parser = Parser $ \ pat -> multiplier lower upper parser pat
  where
    -- Range of expected successes (inclusive).
    lower = 1
    upper = 1




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
multiplier ::  (Show state) => Int -> Int -> Parser state (a -> a) -> state -> Maybe (state, (a -> a))
multiplier lower upper parser pat | n >= lower && n <= upper = Just (pat'', fs')
                                  | otherwise                = Nothing

  where
    fs = id
    (pat'', fs', n) = callUntilFail parser pat fs 0 hardLimit

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
callUntilFail :: (Show state) => Parser state (a -> a) -> state -> (a -> a) -> Int -> Int -> (state, (a -> a), Int)
callUntilFail parser pat fs i limit = case runParser parser pat of
                                        Just (pat', f)  -> callUntilFail parser pat' (f . fs) (i + 1) limit
                                        Nothing         -> (pat, fs, i)




-- Run all parsers in given order
--
-- Return success (Just accumulator) if exactly one of them succeeds.
-- Return failure (Nothing) if zero, two or more of them succeed.
--
-- https://www.w3.org/TR/css-values-3/#component-combinators
-- "A bar (|) separates two or more alternatives: exactly one of them must occur."
--
-- Unit tested? Yes.
{-
:m +Hello.Utils.Parser
:m +Data.Maybe
:m +Data.Text
:m +Hello.Css.Parser.Combinators
:set prompt >

(fmap . fmap) (\ f -> f "") (runParser (combinatorExactlyOne [parserB, parserA]) "ad")
-}
combinatorExactlyOne :: [Parser p (a -> a)] -> Parser p (a -> a)
combinatorExactlyOne parsers = Parser $ \ state -> case fmap (try state) parsers of
                                                     mapped -> if (L.length . catMaybes $ mapped) == 1 -- Only one of parsers succeeded and returned some Just
                                                               then Just (head . catMaybes $ mapped)
                                                               else Nothing
  where
    -- Run a single parser from list of parsers. Notice that each parser from
    -- input 'parsers' list is executed with the same input state.
    try patArg parser = runParser parser patArg




tryParsersUnordered :: state -> [Parser state (a -> a)] -> [Parser state (a -> a)] -> (a -> a) -> Int -> ((state, (a -> a)), Int)
tryParsersUnordered state (parser:parsers) stash fs i =
  case runParser parser state of
    -- On sucessful parse discard current parser, and re-generate list of parsers to be used in next cycle.
    -- On failed parse move current parser to 'for later' list, and retry with shortened list of current parsers.
    Just (state', f) -> tryParsersUnordered state' (parsers ++ stash) []               (f . fs) (i + 1)
    Nothing          -> tryParsersUnordered state  parsers            (parser:stash)    fs       i
tryParsersUnordered state []                  _  fs i = ((state, fs), i)


