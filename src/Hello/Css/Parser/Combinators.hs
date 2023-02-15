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
    MyParser
  , combinatorExactlyOne
  , combinatorAllInOrder
  , combinatorOneOrMoreUnordered
  , multiplierZeroOrOnce
  , multiplierOnce

  , rewrap

  , combinatorOneOrMoreUnorderedB
  , combinatorAllInOrderB
  , multiplierZeroOrOnceB
  , multiplierOnceB
  , combinatorExactlyOneB
  , parserA
  , parserB
  , parserC
  )
where




--import Debug.Trace

import qualified Data.Text as T
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
    countOfNonEmpty = L.length $ L.filter (not . L.null) accumulators
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
    countOfNonEmpty        = L.length accumulators
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




{-
concatAccs :: Maybe [a] -> [[a]] -> [a]
concatAccs mAcc accumulators = case mAcc of
                                 Just acc -> acc ++ L.concat accumulators
                                 Nothing  -> []  ++ L.concat accumulators




concatAcc :: Maybe [a] -> [a] -> [a]
concatAcc mAcc acc = case mAcc of
                       Just acc' -> acc' ++ acc
                       Nothing   -> []   ++ acc
-}



runFunctions :: [MyParser p a] -> (p, [[a]]) -> (p, [[a]])
runFunctions (f:fs) (pat, accumulators) = runFunctions fs (pat', acc')
  where (pat', acc') = case f pat of
                         (pat'', Just acc'') -> (pat'', accumulators ++ [acc''])
                         (_, Nothing)        -> (pat,   accumulators)
runFunctions []     (pat, accumulators) = (pat, accumulators)




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


{-
:m +Hello.Utils.Parser
:m +Data.Maybe
:m +Data.Text
:m +Hello.Css.Parser.Combinators
:set prompt >

(fmap . fmap) (\ f -> f "") (runParser (combinatorOneOrMoreUnorderedB [parserB, parserA]) "ab")
-}
combinatorOneOrMoreUnorderedB :: [Parser state (a -> a)] -> Parser state (a -> a)
combinatorOneOrMoreUnorderedB parsers = Parser $ \ state ->
  case tryParsersUnordered' state parsers stash fs 0 of
    ((state', fs'), i) | i >= 1    -> Just (state', fs')
                       | otherwise -> Nothing
  where
    stash = [] -- Stash for parsers, necessary for algo of running parsers unordered.
    fs    = id -- Accumulator of functions. 'id' is an empty element for function composition.




{-
:m +Hello.Utils.Parser
:m +Data.Maybe
:m +Data.Text
:m +Hello.Css.Parser.Combinators
:set prompt >

(fmap . fmap) (\ f -> f "") (runParser (combinatorAllInOrderB [parserA, parserB]) "abC")
-}
combinatorAllInOrderB :: [Parser state (a -> a)] -> Parser state (a -> a)
combinatorAllInOrderB parsers = Parser $ \ state -> runAllParsers parsers state id




-- Run all parsers from given list. Each parser is ran with state updated by
-- previous parsers' calls. All parsers must succeed.
runAllParsers :: [Parser state (a -> a)] -> state -> (a -> a) -> Maybe (state, (a -> a))
runAllParsers (parser:parsers) state fs = case runParser parser state of
                                            Just (state', f) -> runAllParsers parsers state' (fs . f)
                                            Nothing          -> Nothing
runAllParsers []               state fs = Just (state, fs)




{-
:m +Hello.Utils.Parser
:m +Data.Maybe
:m +Data.Text
:m +Hello.Css.Parser.Combinators
:set prompt >

(fmap . fmap) (\ f -> f "") (runParser (multiplierZeroOrOnceB (combinatorAllInOrderB [parserB, parserA])) "ab")
-}
multiplierZeroOrOnceB ::  (Show state) => Parser state (a -> a) -> Parser state (a -> a)
multiplierZeroOrOnceB parser = Parser $ \ pat -> multiplierB lower upper parser pat
  where
    -- Range of expected successes (inclusive).
    lower = 0
    upper = 1




multiplierOnceB ::  (Show state) => Parser state (a -> a) -> Parser state (a -> a)
multiplierOnceB parser = Parser $ \ pat -> multiplierB lower upper parser pat
  where
    -- Range of expected successes (inclusive).
    lower = 1
    upper = 1



multiplierB ::  (Show state) => Int -> Int -> Parser state (a -> a) -> state -> Maybe (state, (a -> a))
multiplierB lower upper parser pat | n >= lower && n <= upper = Just (pat'', fs')
                                   | otherwise                = Nothing

  where
    fs = id
    (pat'', fs', n) = callUntilFailB parser pat fs 0 hardLimit

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
callUntilFailB :: (Show state) => Parser state (a -> a) -> state -> (a -> a) -> Int -> Int -> (state, (a -> a), Int)
callUntilFailB parser pat fs i limit = case runParser parser pat of
                                         Just (pat', f)  -> callUntilFailB parser pat' (f . fs) (i + 1) limit
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

(fmap . fmap) (\ f -> f "") (runParser (combinatorExactlyOneB [parserB, parserA]) "ad")
-}
combinatorExactlyOneB :: [Parser p (a -> a)] -> Parser p (a -> a)
combinatorExactlyOneB parsers = Parser $ \ state -> case fmap (try state) parsers of
                                                      mapped -> if (L.length . catMaybes $ mapped) == 1 -- Only one of parsers succeeded and returned some Just
                                                                then Just (head . catMaybes $ mapped)
                                                                else Nothing
  where
    -- Run a single parser from list of parsers. Notice that each parser from
    -- input 'parsers' list is executed with the same input state.
    try patArg parser = runParser parser patArg




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
combinatorOneOrMoreUnordered :: acc -> [acc -> Parser state acc] -> state -> Maybe (state, acc)
combinatorOneOrMoreUnordered acc fs state | i > 0     = Just stateAndAcc
                                          | otherwise = Nothing
  where
    (stateAndAcc, i) = tryParsersUnordered state fs [] acc 0




-- Rewrap combinator that returns new-type value (Maybe (state, value)) into
-- combinator that returns old-style value (state, Maybe value).
rewrap :: ([f] -> [a -> b] -> state -> (Maybe (state, value))) -> [a -> b] -> state -> (state, Maybe value)
rewrap combinator fns pat = case combinator [] fns pat of
                              Just (pat', acc) -> (pat', Just acc)
                              Nothing          -> (pat, Nothing)





-- This function is using Int argument: a counter of successes. This is to
-- know if 'one or more' parsers have succeeded.
--
-- Without it I would have compare the input and output accumulator to see if
-- any parser has modified it, and not all accumulator types will implement
-- Eq class.
--
-- Also the Int type allows me to see how many parsers exactly have succeeded
-- and create different functions, e.g. "zero or more".
tryParsersUnordered :: state -> [acc -> Parser state acc] -> [acc -> Parser state acc] -> acc -> Int -> ((state, acc), Int)
tryParsersUnordered state (f:fs) stash acc i =
  case runParser (f acc) state of
    -- On sucessful parse discard current parser, and re-generate list of parsers to be used in next cycle.
    -- On failed parse move current parser to 'for later' list, and retry with shortened list of current parsers.
    Just (state', acc') -> tryParsersUnordered state' (fs ++ stash) []        acc' (i + 1)
    Nothing             -> tryParsersUnordered state  fs            (f:stash) acc  i
tryParsersUnordered state []        _  acc i = ((state, acc), i)




tryParsersUnordered' :: state -> [Parser state (a -> a)] -> [Parser state (a -> a)] -> (a -> a) -> Int -> ((state, (a -> a)), Int)
tryParsersUnordered' state (parser:parsers) stash fs i =
  case runParser parser state of
    -- On sucessful parse discard current parser, and re-generate list of parsers to be used in next cycle.
    -- On failed parse move current parser to 'for later' list, and retry with shortened list of current parsers.
    Just (state', f) -> tryParsersUnordered' state' (parsers ++ stash) []               (f . fs) (i + 1)
    Nothing          -> tryParsersUnordered' state  parsers            (parser:stash)    fs       i
tryParsersUnordered' state []                  _  fs i = ((state, fs), i)




