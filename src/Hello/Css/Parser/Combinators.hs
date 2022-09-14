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
  , multiplierZeroOrMore
  , multiplierOnce

  )
where




import Debug.Trace

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
combinatorExactlyOne :: (Show a) => [MyParser p a] -> p -> (p, Maybe [a])
combinatorExactlyOne fs pat = if countOfNonEmpty == 1
                              then trace ("combinatorExactlyOne: success, countOfNonEmpty = " ++ (show countOfNonEmpty) ++ ", " ++ (show accumulators)) (outPat, Just $ L.concat accumulators)
                              else trace ("combinatorExactlyOne: failure, countOfNonEmpty = " ++ (show countOfNonEmpty) ++ ", " ++ (show accumulators)) (pat, Nothing)

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
combinatorAllInOrder :: (Show a) => [MyParser p a] -> p -> (p, Maybe [a])
combinatorAllInOrder functions pat = if countOfNonEmpty == L.length functions
                                     then trace ("combinatorAllInOrder: success, L.length functions = " ++ (show . L.length $ functions) ++ ", countOfNonEmpty = " ++ (show countOfNonEmpty)) (outPat, Just $ L.concat accumulators)
                                     else trace ("combinatorAllInOrder: failure, L.length functions = " ++ (show . L.length $ functions) ++ ", countOfNonEmpty = " ++ (show countOfNonEmpty)) (pat, Nothing)

  where
    countOfNonEmpty = trace ("combinatorAllInOrder: accumulators = " ++ (show accumulators)) (L.length accumulators)
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
combinatorOneOrMoreUnordered :: (Show a) => [MyParser p a] -> p -> (p, Maybe [a])
combinatorOneOrMoreUnordered functions pat = if successes >= 1
                                             then trace ("combinatorOneOrMoreUnordered: success, count = " ++ (show $ L.concat accumulators)) (outPat, Just $ L.concat accumulators)
                                             else trace ("combinatorOneOrMoreUnordered: failure, count = " ++ (show successes))               (pat, Nothing)

  where
    successes = L.length accumulators
    (outPat, accumulators) = runFunctions functions (pat, [])




-- Multiplier: run given function, see how many successes it returns.
--
-- Return 'Just acc' in property accumulator if count of successes is correct.
--
-- Since "zero" is also a success, this function always succeeds (always
-- returns some accumulator).
--
-- https://www.w3.org/TR/css-values-3/#component-multipliers
multiplierZeroOrMore :: (Show a) => MyParser p a -> p -> (p, Maybe [a])
multiplierZeroOrMore function pat = case function pat of
                                      (pat', Just acc') ->                                                               (pat', Just acc')
                                      -- Returning 'Just []' to indicate success, because zero child items have succeeded.
                                      (pat', Nothing)   -> trace ("multiplierZeroOrMore returning empty acc on Nothing") (pat, Just [])




-- Multiplier: run given function, see how many successes it returns.
--
-- Return 'Just acc' in property accumulator if count of successes is correct.
-- Return Nothing in property accumulator if count of successes is incorrect.
--
-- TODO: write a correct version.
--
-- https://www.w3.org/TR/css-values-3/#component-multipliers
multiplierOnce :: (Show a) => MyParser p a -> p -> (p, Maybe [a])
multiplierOnce function pat = case function pat of
                                (pat', Just acc') -> trace ("multiplierOnce: acc = " ++ (show acc')) (pat', Just acc')
                                (pat', Nothing)   ->                                                 (pat, Nothing)




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
                         (pat'', Nothing)    -> (pat,   accumulators)
runFunctions []     (pat, accumulators) = (pat, accumulators)





