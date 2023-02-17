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
Helpers for parsing of shortcut properties with parsers.

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



{-
Apply given parsers to some input unil none of the parsers matches. Parsers
save result in (a -> a) closures.

Succeed if one or more of the parsers succeeded and has taken some token from
front of input.

Fail if none of parsers succeeded in taking any tokens from input.

Each parser is discarded after successful application. This combinator tries
the applications until all parsers are discarded after their succeses, or
until none of parsers can succeed anymore. A pParser is discarded because it
has been already successfully used once, and can't be used for a second time.

Return a new parser returning closures on success.

Return a new parser returning Nothing on failure.

Unit tested? Yes.

:m +Hello.Utils.Parser
:m +Data.Maybe
:m +Data.Text
:m +Hello.Css.Parser.Combinators
:set prompt >
(fmap . fmap) (\ f -> f "") (runParser (combinatorOneOrMoreUnordered [parserB, parserA]) "ab")
-}
combinatorOneOrMoreUnordered :: [Parser input (a -> a)] -> Parser input (a -> a)
combinatorOneOrMoreUnordered parsers = Parser $ \ input ->
  case tryParsersUnordered input parsers stash fs 0 of
    ((input', fs'), i) | i >= 1    -> Just (input', fs')
                       | otherwise -> Nothing
  where
    stash = [] -- Stash for parsers, necessary for algo of running parsers unordered.
    fs    = id -- Accumulator of closures (functions). 'id' is an empty element for function composition.




{-
Run all parsers in given order, once

https://www.w3.org/TR/css-values-3/#component-combinators
"Juxtaposing components means that all of them must occur, in the given order."

Succeed if all of the parsers, called in order (one after another) succeeded.

Fail if one or more of parsers failed.

Return a new parser returning (a -> a) (composed closures from each successful parser) on success.

Return a new parser returning Nothing on failure.

Unit tested? Yes.

:m +Hello.Utils.Parser
:m +Data.Maybe
:m +Data.Text
:m +Hello.Css.Parser.Combinators
:set prompt >
(fmap . fmap) (\ f -> f "") (runParser (combinatorAllInOrder [parserA, parserB]) "abC")
-}
combinatorAllInOrder :: [Parser input (a -> a)] -> Parser input (a -> a)
combinatorAllInOrder parsers = Parser $ \ input -> runAllParsers parsers input id




-- Run all parsers from given list. Each parser is ran with input updated by
-- previous parsers' calls. All parsers must succeed.
runAllParsers :: [Parser input (a -> a)] -> input -> (a -> a) -> Maybe (input, (a -> a))
runAllParsers (parser:parsers) input fs = case runParser parser input of
                                            Just (input', f) -> runAllParsers parsers input' (f . fs)
                                            Nothing          -> Nothing
runAllParsers []               input fs = Just (input, fs)



{-
Multiplier: run given parser multiple times as long as the parser is able
to successfully take a token from input. See how many times the parser was
executed successfully (how many consecutive tokens were consumed
successfully).

https://www.w3.org/TR/css-values-3/#component-multipliers
"A question mark (?) indicates that the preceding type, word, or group is
 optional (occurs zero or one times)."

Succeed if count of successes is correct: is either zero or one.

Fail if If count of successes is higher than one

Return a new parser returning (a -> a) (composed closures from each successful parser) on scuccess.

Return a new parser returning Nothing on failure.

Unit tested? Yes.

:m +Hello.Utils.Parser
:m +Data.Maybe
:m +Data.Text
:m +Hello.Css.Parser.Combinators
:set prompt >
(fmap . fmap) (\ f -> f "") (runParser (multiplierZeroOrOnce (combinatorAllInOrder [parserB, parserA])) "ab")
-}
multiplierZeroOrOnce ::  (Show input) => Parser input (a -> a) -> Parser input (a -> a)
multiplierZeroOrOnce parser = Parser $ \ input -> multiplier lower upper parser input
  where
    -- Range of expected successes (inclusive).
    lower = 0
    upper = 1



{-
Multiplier: run given parser multiple times as long as the parser is able
to successfully take a token from input. See how many times the parser was
executed successfully (how many consecutive tokens were consumed
successfully).

Succeed if count of successes is correct: is exactly one.

Fail if If count of successes is not one.

Return a new parser returning (a -> a) (composed closures from that one single parser) on scuccess.

Return a new parser returning Nothing on failure.

This multiplier is not described in CSS spec
(https://www.w3.org/TR/css-values-3/#component-multipliers), so maybe after
all it's not necessary, but for some reason I thought that it would be
convenient to explicitly indicate that some expressions in input CSS should
appear exactly once. TODO: check if we really need this multiplier.

Unit tested? Yes.
-}
multiplierOnce :: (Show input) => Parser input (a -> a) -> Parser input (a -> a)
multiplierOnce parser = Parser $ \ input -> multiplier lower upper parser input
  where
    -- Range of expected successes (inclusive).
    lower = 1
    upper = 1





{-
Multiplier: run given parser multiple times as long as the parser is able
to successfully take a token from input. See how many times the parser was
executed successfully (how many consecutive tokens were consumed
successfully).

An universal multiplier function that can be used by more specialized
multipliers.

First two arguments specify count of successful calls of parser, as a
range. If given parser function successfully takes N consecutive
tokens, and N is between lower and upper (inclusive), then multiplier
succeeds.

If 'upper' is (-1) then (in theory) there is no upper limit on count of
successes (count of times the parser will be called to get next matching
expression). In practice the count of successful calls will be limited to
some hardcoded value to avoid infinite loop on malformed or malicious
input.
-}
multiplier :: (Show input) => Int -> Int -> Parser input (a -> a) -> input -> Maybe (input, (a -> a))
multiplier lower upper parser input | n >= lower && n <= upper = Just (input', fs')
                                    | otherwise                = Nothing

  where
    fs = id -- Accumulator of closures (functions). 'id' is an empty element for function composition.
    (input', fs', n) = callUntilFail parser input fs 0 hardLimit

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




-- Call given parser multiple times.
-- When the parser finally fails to take a token, return
-- accumulated result of all of the calls that succeeded, and count of successes
callUntilFail :: (Show input) => Parser input (a -> a) -> input -> (a -> a) -> Int -> Int -> (input, (a -> a), Int)
callUntilFail parser input fs i limit = case runParser parser input of
                                          Just (input', f) -> callUntilFail parser input' (f . fs) (i + 1) limit
                                          Nothing          -> (input, fs, i)



{-
Run all parsers in given order

https://www.w3.org/TR/css-values-3/#component-combinators
"A bar (|) separates two or more alternatives: exactly one of them must occur."

Succeed if exactly one of the parsers succeeds.

Fail if zero, two or more of the parsers succedd.

Return a new parser returning (a -> a) (composed closures from each successful parser) on success.

Return a new parser returning Nothing on failure.

TODO: perhaps this can be replaced with more standard "<|>" operator?

TODO: the function does fmap, so all parsers are checked. If first two
parsers from the list succeed, checking a third one and the following ones is
a waste of resources.

Unit tested? Yes.

:m +Hello.Utils.Parser
:m +Data.Maybe
:m +Data.Text
:m +Hello.Css.Parser.Combinators
:set prompt >
(fmap . fmap) (\ f -> f "") (runParser (combinatorExactlyOne [parserB, parserA]) "ad")
-}
combinatorExactlyOne :: [Parser p (a -> a)] -> Parser p (a -> a)
combinatorExactlyOne parsers = Parser $ \ input -> case fmap (try input) parsers of
                                                     mapped -> if (L.length . catMaybes $ mapped) == 1 -- Only one of parsers succeeded and returned some Just
                                                               then Just (head . catMaybes $ mapped)
                                                               else Nothing
  where
    -- Run a single parser from list of parsers. Notice that each parser from
    -- 'parsers' list is executed with the same input.
    try inArg parser = runParser parser inArg



{-
Run parsers until either each parser eventually successfully took some token
from input, or none of them can take a token from input anymore.

Return remaining input, (a -> a) closure with consumed tokens, and count of
parsers that succeeded.
-}
tryParsersUnordered :: input -> [Parser input (a -> a)] -> [Parser input (a -> a)] -> (a -> a) -> Int -> ((input, (a -> a)), Int)
tryParsersUnordered input (parser:parsers) stash fs i =
  case runParser parser input of
    -- On sucessful parse discard current parser, and re-generate list of parsers to be used in next cycle.
    -- On failed parse move current parser to 'for later' list, and retry with shortened list of current parsers.
    Just (input', f) -> tryParsersUnordered input' (parsers ++ stash) []               (f . fs) (i + 1)
    Nothing          -> tryParsersUnordered input  parsers            (parser:stash)    fs       i
tryParsersUnordered input []                  _  fs i = ((input,  fs), i)


