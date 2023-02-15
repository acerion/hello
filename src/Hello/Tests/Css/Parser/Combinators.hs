{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}




module Hello.Tests.Css.Parser.Combinators
  (
    testsCssParserCombinators
  , takeChar
  )
where




import Control.Applicative (Alternative(..), many, some)
import qualified Data.Text as T

--import Debug.Trace

import Test.HUnit

import Hello.Css.Parser.Combinators
import Hello.Utils.Parser




-- --------------------------------------------------------------------------
-- Common definitions
-- --------------------------------------------------------------------------




-- Simple parser taking input string and trying to match a character.
type TestParser = MyParser T.Text Char
type Combinator = [MyParser T.Text Char] -> T.Text -> (T.Text, Maybe [Char])
type Multiplier =  MyParser T.Text Char  -> T.Text -> (T.Text, Maybe [Char])




-- Parser function to be specialized with specific characters.
takeChar :: Char -> TestParser
takeChar char text = case T.uncons text of
                       Just (c, remd) | c == char -> (remd, Just [c])
                                      | otherwise -> (text, Nothing)
                       Nothing                    -> (text, Nothing)




-- Finally our parsers that try to take/match specific character from input.
takeA :: TestParser
takeA = takeChar 'a'

takeB :: TestParser
takeB = takeChar 'b'

takeC :: TestParser
takeC = takeChar 'c'




mkParserCharA :: AccA -> Char -> Parser StateA AccA
mkParserCharA acc char = Parser $ \ txt -> case T.uncons txt of
                                             Just (c, remd) | c == char -> Just (remd, acc ++ [c])
                                                            | otherwise -> Nothing
                                             Nothing -> Nothing




takeE :: AccA -> Parser T.Text AccA
takeE acc = mkParserCharA acc 'e'

takeF :: AccA -> Parser T.Text AccA
takeF acc = mkParserCharA acc 'f'

takeG :: AccA -> Parser T.Text AccA
takeG acc = mkParserCharA acc 'g'




data CombinatorTestData = CombinatorTestData
  { parsers          :: [TestParser]
  , input            :: T.Text
  , expectedOutput   :: (T.Text, Maybe [Char])
  }



type OutputA = Maybe (StateA, AccA)
type StateA = T.Text
type AccA = [Char]
type CombinatorA   = AccA -> [AccA -> Parser StateA AccA] -> StateA -> OutputA
type TestedParserA = AccA -> Parser StateA AccA


data CombinatorTestDataA = CombinatorTestDataA
  { inputAccA         :: AccA
  , parsersA          :: [TestedParserA]
  , inputStateA       :: StateA
  , expectedOutputA   :: OutputA
  }




data MultiplierTestData = MultiplierTestData
  { childItems2       :: TestParser
  , input2            :: T.Text
  , expectedOutput2   :: (T.Text, Maybe [Char])
  }




-- On success return empty string. On failure return string showing
-- approximately where the problem is.
combinatorTestFunction :: Combinator -> [CombinatorTestData] -> T.Text
combinatorTestFunction _          []     = ""
combinatorTestFunction combinator (x:xs) = if not success
                                           then T.pack ("Got: " ++ show output ++ ", Expected: " ++ (show . expectedOutput $ x) ++ "; input = " ++ (show . input $ x))
                                           else combinatorTestFunction combinator xs
  where
    success = output == expectedOutput x
    output = combinator (parsers x) (input x)




-- On success return empty string. On failure return string showing
-- approximately where the problem is.
combinatorTestFunctionA :: CombinatorA -> [CombinatorTestDataA] -> T.Text
combinatorTestFunctionA _          []     = ""
combinatorTestFunctionA combinator (x:xs) = if not success
                                           then T.pack ("Got: " ++ show output ++ ", Expected: " ++ (show expectedOut) ++ "; input = " ++ (show inputState))
                                           else combinatorTestFunctionA combinator xs
  where
    success = output == expectedOut

    inputAcc     = inputAccA x
    inputParsers = parsersA x
    inputState   = inputStateA x
    expectedOut  = expectedOutputA x

    output :: Maybe (T.Text, [Char])
    output = combinator inputAcc inputParsers inputState





-- On success return empty string. On failure return string showing
-- approximately where the problem is.
multiplierTestFunction :: Multiplier -> [MultiplierTestData] -> T.Text
multiplierTestFunction _ []       = ""
multiplierTestFunction multiplier (x:xs) = if not success
                                           then T.pack ("Got: " ++ show output ++ ", Expected: " ++ (show . expectedOutput2 $ x) ++ "; input = " ++ (show . input2 $ x))
                                           else multiplierTestFunction multiplier xs
  where
    success = output == expectedOutput2 x
    output = multiplier (childItems2 x) (input2 x)




{- -------------------------------------------------------------------------- -}




-- We expect here that tested combinator will return some 'Just' only when a
-- single (one) parser from CombinatorTestData::parsers succeeds. When zero or
-- two or three parsers succeed, then the tested combinator should return
-- Nothing.
combinatorExactlyOneTestData :: [CombinatorTestData]
combinatorExactlyOneTestData =
  [
    -- Success cases

    -- There is only one parser, and this parser has matching data in input, so we have a success.
    CombinatorTestData { parsers = [takeA],               input = "a",   expectedOutput = ("", Just ['a']) }
  , CombinatorTestData { parsers = [takeA],               input = "ab",  expectedOutput = ("b", Just ['a']) }

    -- Two parsers, only first parser succeeds: it matches the first letter.
  , CombinatorTestData { parsers = [takeA, takeB],        input = "atb", expectedOutput = ("tb", Just ['a']) }

    -- Only first parser succeeds: it matches the first letter.
  , CombinatorTestData { parsers = [takeA, takeB, takeC], input = "a12", expectedOutput = ("12", Just ['a']) }

    -- Only second parser succeeds: it matches the first letter.
  , CombinatorTestData { parsers = [takeA, takeB, takeC], input = "btq", expectedOutput = ("tq", Just ['b']) }

    -- Only third parser succeeds: it matches the first letter.
  , CombinatorTestData { parsers = [takeA, takeB, takeC], input = "c~+", expectedOutput = ("~+", Just ['c']) }

    -- All the letters are there in the input, but in wrong order, and only the third parser succeeds.
  , CombinatorTestData { parsers = [takeA, takeB, takeC], input = "cab", expectedOutput = ("ab", Just ['c']) }

    -- There is only one letter in input data, and it is successfully parsed.
    -- The other two parsers will fail.
  , CombinatorTestData { parsers = [takeA, takeB, takeC], input = "a",   expectedOutput = ("", Just ['a']) }



    -- Failure cases

    -- There are zero parsers to run, so there cannot be one parser to succeed.
  , CombinatorTestData { parsers = [], input = "abc", expectedOutput = ("abc", Nothing) }

    -- There is only one parser, but this parser doesn't have matching data in input, so we have zero matching parsers.
  , CombinatorTestData { parsers = [takeA],               input = "o",   expectedOutput = ("o", Nothing) }
  , CombinatorTestData { parsers = [takeA],               input = "oh",  expectedOutput = ("oh", Nothing) }

    -- Two parsers, both succeed. Two successful parsers is too much.
  , CombinatorTestData { parsers = [takeA, takeB],        input = "abe", expectedOutput = ("abe", Nothing) }

    -- Two parsers, none of them succeed. Zero successes is not "exactly one".
  , CombinatorTestData { parsers = [takeA, takeB],        input = "two", expectedOutput = ("two", Nothing) }

    -- All three parsers succeed, each of them taking their letter. Three successful parsers is too much.
  , CombinatorTestData { parsers = [takeA, takeB, takeC], input = "abc", expectedOutput = ("abc", Nothing) }

    -- First two parsers succeed, third fails. Two successful parsers is too much.
  , CombinatorTestData { parsers = [takeA, takeB, takeC], input = "abT", expectedOutput = ("abT", Nothing) }

    -- First succeeds, second fails, but the third again succeeds. Two successful parsers is too much.
  , CombinatorTestData { parsers = [takeA, takeB, takeC], input = "acX", expectedOutput = ("acX", Nothing) }

    -- First parser fails, but the second and third succeed. Two successful parsers is too much.
  , CombinatorTestData { parsers = [takeA, takeB, takeC], input = "1bc", expectedOutput = ("1bc", Nothing) }

    -- None of acceptable letters are in input data, so zero tests pass. Zero is not "exactly one".
  , CombinatorTestData { parsers = [takeA, takeB, takeC], input = "xyz", expectedOutput = ("xyz", Nothing) }

    -- Input string is empty, so none of parsers succeed. Zero is not "exactly one".
  , CombinatorTestData { parsers = [takeA, takeB, takeC], input = "",    expectedOutput = ("", Nothing) }
  ]





{- -------------------------------------------------------------------------- -}




-- We expect here that tested combinator will return some 'Just' only when a
-- single (one) parser from CombinatorTestData::parsers succeeds. When zero or
-- two or three parsers succeed, then the tested combinator should return
-- Nothing.
combinatorAllInOrderTestData :: [CombinatorTestData]
combinatorAllInOrderTestData =
  [
    -- Success cases

    -- There is only one parser, and this parser has matching data in input, so we have a success.
    CombinatorTestData { parsers = [takeA],               input = "a",   expectedOutput = ("", Just ['a']) }
  , CombinatorTestData { parsers = [takeA],               input = "ab",  expectedOutput = ("b", Just ['a']) }

    -- Two parsers, both of them succeed.
  , CombinatorTestData { parsers = [takeA, takeB],        input = "ab",  expectedOutput = ("", Just ['a', 'b']) }

    -- Two parsers, both of them succeed.
  , CombinatorTestData { parsers = [takeA, takeB],        input = "abc", expectedOutput = ("c", Just ['a', 'b']) }

    -- Two parsers, both of them succeed.
  , CombinatorTestData { parsers = [takeA, takeB],        input = "abe", expectedOutput = ("e", Just ['a', 'b']) }

    -- All three parsers succeed, each of them taking their letter.
  , CombinatorTestData { parsers = [takeA, takeB, takeC], input = "abc", expectedOutput = ("", Just ['a', 'b', 'c']) }



    -- Failure cases

    -- Two parsers. There are matching letters in input string for both of
    -- them, but they are separated by a letter that won't be taken by any of
    -- the two parsers.
  , CombinatorTestData { parsers = [takeA, takeB],        input = "acb", expectedOutput = ("acb", Nothing) }

    -- Only first parser succeeds: it matches the first letter. But the other
    -- two don't match, so no success overall.
  , CombinatorTestData { parsers = [takeA, takeB, takeC], input = "a12", expectedOutput = ("a12", Nothing) }

  -- Only second parser would succeed, but the other two don't match, so no success overall.
  , CombinatorTestData { parsers = [takeA, takeB, takeC], input = "btq", expectedOutput = ("btq", Nothing) }

    -- Only third parser would succeed, but the other two don't match, so no success overall.
  , CombinatorTestData { parsers = [takeA, takeB, takeC], input = "c~+", expectedOutput = ("c~+", Nothing) }

    -- All the letters are there in the input, but in wrong order.
  , CombinatorTestData { parsers = [takeA, takeB, takeC], input = "cab", expectedOutput = ("cab", Nothing) }

    -- There is only one letter in input data, and it is successfully parsed.
    -- The other two parsers will fail, so overall it's a fail.
  , CombinatorTestData { parsers = [takeA, takeB, takeC], input = "a",   expectedOutput = ("a", Nothing) }

  {-
    -- There are zero parsers to run. On one hand there are no parsers that
    -- could fail, but on the other hand there is not a single parser that
    -- would return some accumulator.
    --
    -- TODO: tested function returns ("abc", Just []). What to do?
  , CombinatorTestData { parsers = [],                    input = "abc", expectedOutput = ("abc", Nothing) }
  -}


    -- There is only one parser, but this parser doesn't have matching data in input, so we have zero matching parsers.
  , CombinatorTestData { parsers = [takeA],               input = "",    expectedOutput = ("", Nothing) }
  , CombinatorTestData { parsers = [takeA],               input = "o",   expectedOutput = ("o", Nothing) }
  , CombinatorTestData { parsers = [takeA],               input = "oh",  expectedOutput = ("oh", Nothing) }
  , CombinatorTestData { parsers = [takeA],               input = "oa",  expectedOutput = ("oa", Nothing) }

    -- Two parsers, none of them has matching letter.
  , CombinatorTestData { parsers = [takeA, takeB],        input = "two", expectedOutput = ("two", Nothing) }

    -- Three parsers, none of them has matching letter.
  , CombinatorTestData { parsers = [takeA, takeB, takeC], input = "xyz", expectedOutput = ("xyz", Nothing) }

    -- First two parsers succeed, third fails. Two out of three successful parsers is not enough.
  , CombinatorTestData { parsers = [takeA, takeB, takeC], input = "abT", expectedOutput = ("abT", Nothing) }

    -- First parser fails, but the second and third would succeed. But they will never be called.
  , CombinatorTestData { parsers = [takeA, takeB, takeC], input = "1bc", expectedOutput = ("1bc", Nothing) }

    -- Input string is empty, so none of parsers succeed. Zero is not "exactly one".
  , CombinatorTestData { parsers = [takeA, takeB, takeC], input = "",    expectedOutput = ("", Nothing) }
  ]




{- -------------------------------------------------------------------------- -}




-- We expect here that tested combinator will return some 'Just' when one or
-- more parsers from CombinatorTestData::parsers succeeds. Valid sets of
-- input tokens may have various orders of the tokens, so the parsers
-- themselves may be called in various order (in order different from the
-- order of appearance on the list).
--
-- TODO: the "unordered" part of the combinator is not implemented yet. Once
-- it is, expand the tests to cover the expanded functionality.
combinatorOneOrMoreUnorderedTestData :: [CombinatorTestDataA]
combinatorOneOrMoreUnorderedTestData =
  [
    -- Success cases

    -- There is only one parser, and this parser has matching data in input, so we have a success.
    CombinatorTestDataA { inputAccA = [], parsersA = [takeE],               inputStateA = "e",   expectedOutputA = Just ("", ['e']) }
  , CombinatorTestDataA { inputAccA = [], parsersA = [takeE],               inputStateA = "ef",  expectedOutputA = Just ("f", ['e']) }

    -- Two parsers, both of them succeed.
  , CombinatorTestDataA { inputAccA = [], parsersA = [takeE, takeF],        inputStateA = "ef",  expectedOutputA = Just ("", ['e', 'f']) }

    -- Two parsers in reverse order, both of them succeed (because the combinator is called "unordered").
  , CombinatorTestDataA { inputAccA = [], parsersA = [takeF, takeE],        inputStateA = "ef",  expectedOutputA = Just ("", ['e', 'f']) }

    -- Two parsers, both of them succeed.
  , CombinatorTestDataA { inputAccA = [], parsersA = [takeE, takeF],        inputStateA = "efg", expectedOutputA = Just ("g", ['e', 'f']) }

    -- Two parsers, both of them succeed.
  , CombinatorTestDataA { inputAccA = [], parsersA = [takeE, takeF],        inputStateA = "efv", expectedOutputA = Just ("v", ['e', 'f']) }

    -- All three parsers succeed, each of them taking their letter.
  , CombinatorTestDataA { inputAccA = [], parsersA = [takeE, takeF, takeG], inputStateA = "efg", expectedOutputA = Just ("", ['e', 'f', 'g']) }

    -- All three parsers succeed, each of them taking their letter. The
    -- combinators are in random order, but they all succeed because the
    -- combinator is called "unordered".
  , CombinatorTestDataA { inputAccA = [], parsersA = [takeF, takeG, takeE], inputStateA = "efg", expectedOutputA = Just ("", ['e', 'f', 'g']) }

    -- First parser succeed, second and third fails. One out of three successful parsers is enough.
  , CombinatorTestDataA { inputAccA = [], parsersA = [takeE, takeF, takeG], inputStateA = "eVT", expectedOutputA = Just ("VT", ['e']) }

    -- One of the parsers will succeed. "unordered" combinator will try to
    -- match them agains first input token ("e") until one of parsers
    -- succeeds or the combinator runs out of parsers to try.
  , CombinatorTestDataA { inputAccA = [], parsersA = [takeF, takeG, takeE], inputStateA = "eVT", expectedOutputA = Just ("VT", ['e']) }

    -- First two parsers succeed, third fails. Two out of three successful parsers is enough.
  , CombinatorTestDataA { inputAccA = [], parsersA = [takeE, takeF, takeG], inputStateA = "efT", expectedOutputA = Just ("T", ['e', 'f']) }


  -- First two parsers succeed, third fails. Two out of three successful parsers is enough.
  -- This time also check if output of combinator is property appended to non-empty input acc.
  , CombinatorTestDataA { inputAccA = ['1', '2', '3']
                        ,parsersA = [takeE, takeF, takeG], inputStateA = "efT", expectedOutputA = Just ("T", ['1', '2', '3', 'e', 'f']) }



    -- Failure cases

    -- Two parsers. None of parsers succeed (there is no token in input that
    -- could be taken by any of the parsers).
  , CombinatorTestDataA { inputAccA = [], parsersA = [takeE, takeF],        inputStateA = "two", expectedOutputA = Nothing }

    -- Three parsers. None of parsers succeed (there is no token in input
    -- that could be taken by any of the parsers).
  , CombinatorTestDataA { inputAccA = [], parsersA = [takeE, takeF, takeG], inputStateA = "ztq", expectedOutputA = Nothing }

    -- There are zero parsers to run. This means that we can't meet the
    -- requirement of "one or more" successful parses.
  , CombinatorTestDataA { inputAccA = [], parsersA = [],                    inputStateA = "efg", expectedOutputA = Nothing }

    -- There is only one parser, but this parser doesn't have matching data in input, so we have zero matching parsers.
  , CombinatorTestDataA { inputAccA = [], parsersA = [takeE],               inputStateA = "",    expectedOutputA = Nothing }
  , CombinatorTestDataA { inputAccA = [], parsersA = [takeE],               inputStateA = "o",   expectedOutputA = Nothing }
  , CombinatorTestDataA { inputAccA = [], parsersA = [takeE],               inputStateA = "oh",  expectedOutputA = Nothing }

    -- Input string is empty, so none of parsers succeed. Zero is not "one or more".
  , CombinatorTestDataA { inputAccA = [], parsersA = [takeE, takeF, takeG], inputStateA = "",    expectedOutputA = Nothing }

  , CombinatorTestDataA { inputAccA = [], parsersA = [takeE],               inputStateA = "oe",  expectedOutputA = Nothing }

    -- First parser fails, but the second and third would succeed. But they will never be called.
  , CombinatorTestDataA { inputAccA = [], parsersA = [takeE, takeF, takeG], inputStateA = "1fg", expectedOutputA = Nothing }
  ]




{- -------------------------------------------------------------------------- -}




-- We expect here that tested multiplier will return some 'Just' for zero
-- succeeses or one success.
multiplierZeroOrOnceTestData :: [MultiplierTestData]
multiplierZeroOrOnceTestData =
  [
    -- Success cases.

    -- A parser doesn't have matching data in input (will be able to take
    -- zero 'a' tokens), so we have a parser that matches zero times. But
    -- zero matches is ok, and tested multiplier succeeds.
    MultiplierTestData { childItems2 = takeA,               input2 = "",    expectedOutput2 = ("",    Just []) }
  , MultiplierTestData { childItems2 = takeA,               input2 = "o",   expectedOutput2 = ("o",   Just []) }
  , MultiplierTestData { childItems2 = takeA,               input2 = "oh",  expectedOutput2 = ("oh",  Just []) }
  , MultiplierTestData { childItems2 = takeA,               input2 = "oa",  expectedOutput2 = ("oa",  Just []) }

    -- A parser has single matching data in input (will be able to
    -- successfully take just one 'a' token), so we have a parser that
    -- matches once. One match is enough to have multiplier that returns
    -- success.
  , MultiplierTestData { childItems2 = takeA,               input2 = "a",   expectedOutput2 = ("",    Just ['a']) }
  , MultiplierTestData { childItems2 = takeA,               input2 = "ab",  expectedOutput2 = ("b",   Just ['a']) }
  , MultiplierTestData { childItems2 = takeA,               input2 = "abc", expectedOutput2 = ("bc",  Just ['a']) }

    -- A set of parsers (a combinator) succeeds zero times, but this is
    -- enough for tested multiplier to return success.
  , MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "",    expectedOutput2 = ("",       Just []) }
  , MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "a",   expectedOutput2 = ("a",      Just []) }
  , MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "ab",  expectedOutput2 = ("ab",     Just []) }
  , MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "acb", expectedOutput2 = ("acb",    Just []) }
  , MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "xyz", expectedOutput2 = ("xyz",    Just []) }

    -- A set of parsers (a combinator) is successfully applied once (only
    -- one/first application of the set was successful). This means that
    -- multiplier succeeded in applying the set of parsers once. This means
    -- that multiplier can return success.
  , MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "abc",   expectedOutput2 = ("",     Just ['a', 'b', 'c']) }
  , MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "abcd",  expectedOutput2 = ("d",    Just ['a', 'b', 'c']) }
  , MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "abcab", expectedOutput2 = ("ab",   Just ['a', 'b', 'c']) }
  , MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "abcxy", expectedOutput2 = ("xy",   Just ['a', 'b', 'c']) }

    -- A set of parsers (a combinator) is successfully applied once (only
    -- one/first application of the set was successful). This means that
    -- multiplier succeeded in applying the set of parsers once. This means
    -- that multiplier can return success.
  , MultiplierTestData { childItems2 = rewrap combinatorOneOrMoreUnordered [takeE, takeF, takeG],   input2 = "eXY",   expectedOutput2 = ("XY",   Just ['e']) }
  , MultiplierTestData { childItems2 = rewrap combinatorOneOrMoreUnordered [takeE, takeF, takeG],   input2 = "efXY",  expectedOutput2 = ("XY",   Just ['e', 'f']) }
  , MultiplierTestData { childItems2 = rewrap combinatorOneOrMoreUnordered [takeE, takeF, takeG],   input2 = "efgXY", expectedOutput2 = ("XY",   Just ['e', 'f', 'g']) }


    -- A set of parsers (a combinator) is successfully applied zero times
    -- (zero applications of the set was successful). This means that
    -- multiplier succeeded in applying the set of parsers zero times. This
    -- means that this particular multiplier can return success.
  , MultiplierTestData { childItems2 = rewrap combinatorOneOrMoreUnordered [takeE, takeF, takeG],   input2 = "XYZ",   expectedOutput2 = ("XYZ",   Just []) }





    -- Failure cases.

    -- A parser has two or more matching pieces of data in input (will be
    -- able to take two or more 'a' tokens). Two or more matches of a parser
    -- is too much for the tested multiplier, and the tested multiplier must
    -- return failure.
  , MultiplierTestData { childItems2 = takeA,               input2 = "aa",   expectedOutput2 = ("aa",  Nothing) }
  , MultiplierTestData { childItems2 = takeA,               input2 = "aaa",  expectedOutput2 = ("aaa", Nothing) }
  , MultiplierTestData { childItems2 = takeA,               input2 = "aab",  expectedOutput2 = ("aab", Nothing) }

    -- A set of parsers (a combinator) succeeds two times, but this is too
    -- much for tested multiplier.
  , MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "abcabc",   expectedOutput2 = ("abcabc",   Nothing) }
  , MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "abcabcd",  expectedOutput2 = ("abcabcd",  Nothing) }

    -- A set of parsers (a combinator) is successfully applied twice (two
    -- applications of the set were successful). This means that multiplier
    -- failed in applying the set of parsers just once. This means that
    -- multiplier cannot return success.
  , MultiplierTestData { childItems2 = rewrap combinatorOneOrMoreUnordered [takeE, takeF, takeG],   input2 = "eeWZ",     expectedOutput2 = ("eeWZ",     Nothing) }
  , MultiplierTestData { childItems2 = rewrap combinatorOneOrMoreUnordered [takeE, takeF, takeG],   input2 = "efefWZ",   expectedOutput2 = ("efefWZ",   Nothing) }
  , MultiplierTestData { childItems2 = rewrap combinatorOneOrMoreUnordered [takeE, takeF, takeG],   input2 = "efgefgWZ", expectedOutput2 = ("efgefgWZ", Nothing) }
  ]




{- -------------------------------------------------------------------------- -}




-- We expect here that tested multiplier will return some 'Just' for exactly one success.
multiplierOnceTestData :: [MultiplierTestData]
multiplierOnceTestData =
  [
    -- Success cases.

    -- A parser has single matching data in input (will be able to
    -- successfully take just one 'a' token), so we have a parser that
    -- matches once. One match is just what is needed to have multiplier that
    -- returns success.
    MultiplierTestData { childItems2 = takeA,               input2 = "a",   expectedOutput2 = ("",    Just ['a']) }
  , MultiplierTestData { childItems2 = takeA,               input2 = "ab",  expectedOutput2 = ("b",   Just ['a']) }
  , MultiplierTestData { childItems2 = takeA,               input2 = "abc", expectedOutput2 = ("bc",  Just ['a']) }

    -- A set of parsers (a combinator) is successfully applied once (only
    -- one/first application of the set was successful). This means that
    -- multiplier succeeded in applying the set of parsers once. This means
    -- that multiplier can return success.
  , MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "abc",   expectedOutput2 = ("",     Just ['a', 'b', 'c']) }
  , MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "abcd",  expectedOutput2 = ("d",    Just ['a', 'b', 'c']) }
  , MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "abcab", expectedOutput2 = ("ab",   Just ['a', 'b', 'c']) }
  , MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "abcxy", expectedOutput2 = ("xy",   Just ['a', 'b', 'c']) }




    -- Failure cases.

    -- A parser doesn't have matching data in input (will be able to take
    -- zero 'a' tokens), so we have a parser that matches zero times. Zero
    -- matches is too little for this multiplier to succeed.
  , MultiplierTestData { childItems2 = takeA,               input2 = "",    expectedOutput2 = ("",    Nothing) }
  , MultiplierTestData { childItems2 = takeA,               input2 = "o",   expectedOutput2 = ("o",   Nothing) }
  , MultiplierTestData { childItems2 = takeA,               input2 = "oh",  expectedOutput2 = ("oh",  Nothing) }
  , MultiplierTestData { childItems2 = takeA,               input2 = "oa",  expectedOutput2 = ("oa",  Nothing) }

    -- A set of parsers (a combinator) succeeds zero times. Zero matches is
    -- too little for this multiplier to succeed.
  , MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "",    expectedOutput2 = ("",       Nothing) }
  , MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "a",   expectedOutput2 = ("a",      Nothing) }
  , MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "ab",  expectedOutput2 = ("ab",     Nothing) }
  , MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "acb", expectedOutput2 = ("acb",    Nothing) }
  , MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "xyz", expectedOutput2 = ("xyz",    Nothing) }

    -- A parser has two or more matching pieces of data in input (will be
    -- able to take two or more 'a' tokens). Two or more matches of a parser
    -- is too much for the tested multiplier, and the tested multiplier must
    -- return failure.
  , MultiplierTestData { childItems2 = takeA,               input2 = "aa",   expectedOutput2 = ("aa",  Nothing) }
  , MultiplierTestData { childItems2 = takeA,               input2 = "aaa",  expectedOutput2 = ("aaa", Nothing) }
  , MultiplierTestData { childItems2 = takeA,               input2 = "aab",  expectedOutput2 = ("aab", Nothing) }

    -- A set of parsers (a combinator) succeeds two times, but this is too
    -- much for tested multiplier.
  , MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "abcabc",   expectedOutput2 = ("abcabc",   Nothing) }
  , MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "abcabcd",  expectedOutput2 = ("abcabcd",  Nothing) }

    -- A set of parsers (a combinator) is successfully applied zero times
    -- (none of applications of the set were successful). This means that the
    -- multipier didn't succeed once and so it cannot return success.
  , MultiplierTestData { childItems2 = rewrap combinatorOneOrMoreUnordered [takeE, takeF, takeG],   input2 = "XYZ",   expectedOutput2 = ("XYZ",   Nothing) }
  ]




{- -------------------------------------------------------------------------- -}




type FnParser = Parser T.Text (T.Text -> T.Text)




-- Make a parser that returns a closure with a parsed value.
mkParserChar :: Char -> Parser T.Text (T.Text -> T.Text)
mkParserChar char = Parser $ \ txt -> case T.uncons txt of
                                        Just (c, remd) | c == char -> Just (remd, flip T.snoc c)
                                                       | otherwise -> Nothing
                                        Nothing -> Nothing




parserCharA :: Parser T.Text (T.Text -> T.Text)
parserCharA = mkParserChar 'a'




parserCharB :: Parser T.Text (T.Text -> T.Text)
parserCharB = mkParserChar 'b'



parserCharC :: Parser T.Text (T.Text -> T.Text)
parserCharC = mkParserChar 'c'





data CombinatorTestDataFn = CombinatorTestDataFn
  { parsersFn          :: [Parser T.Text (T.Text -> T.Text)]
  , inputStateFn       :: T.Text
  , expectedOutputFn   :: Maybe (T.Text, T.Text) -- Remainder + Parsed value
  }




-- On success return empty string. On failure return string showing
-- approximately where the problem is.
combinatorTestFunctionFn :: ([FnParser] -> FnParser) -> [CombinatorTestDataFn] -> T.Text
combinatorTestFunctionFn _          []     = ""

combinatorTestFunctionFn combinator (x:xs) = if not success
                                             then T.pack ("Got: " ++ show output ++ ", Expected: " ++ (show . expectedOutputFn $ x) ++ "; input = " ++ (show . inputStateFn $ x))
                                             else combinatorTestFunctionFn combinator xs
  where
    success = output == expectedOutputFn x

    -- Parser combinator produces a parser.
    combinedParsers :: Parser T.Text (T.Text -> T.Text)
    combinedParsers = combinator (parsersFn x)

    -- Running combined parser gives us some Maybe.
    parsingResult :: Maybe (T.Text, T.Text -> T.Text)
    parsingResult = runParser combinedParsers (inputStateFn x)

    -- The output from combined parser contains a series of closures. Let's
    -- execute them to get Maybe (T.Text, T.Text). "" is a null/empty/initial
    -- element for the closures.
    output :: Maybe (T.Text, T.Text)
    output = (fmap . fmap) (flip ($) "") parsingResult




-- We expect here that tested combinator will return some 'Just' only when a
-- single (one) parser from CombinatorTestData::parsers succeeds. When zero or
-- two or three parsers succeed, then the tested combinator should return
-- Nothing.
combinatorOneOrMoreUnorderedFnTestData :: [CombinatorTestDataFn]
combinatorOneOrMoreUnorderedFnTestData =
  [
    -- Success cases

    -- There is only one parser, and this parser has matching data in input, so we have a success.
    CombinatorTestDataFn { inputStateFn     = "a"
                         , parsersFn        = [parserCharA]
                         , expectedOutputFn = Just ("", "a")
                         }

  , CombinatorTestDataFn { inputStateFn     = "ab"
                         , parsersFn        = [parserCharA]
                         , expectedOutputFn = Just ("b", "a")
                         }

    -- Two parsers, both of them succeed.
  , CombinatorTestDataFn { inputStateFn     = "abX"
                         , parsersFn        = [parserCharA, parserCharB]
                         , expectedOutputFn = Just ("X", "ab")
                         }

    -- Two parsers in reverse order, both of them succeed (because the combinator is called "unordered").
  , CombinatorTestDataFn { inputStateFn     = "abX"
                         , parsersFn        = [parserCharB, parserCharA]
                         , expectedOutputFn = Just ("X", "ab")
                         }

    -- Three parsers, all three parsers succeed, each of them taking their letter.
  , CombinatorTestDataFn { inputStateFn     = "abcX"
                         , parsersFn        = [parserCharA, parserCharB, parserCharC]
                         , expectedOutputFn = Just ("X", "abc")
                         }

    -- All three parsers succeed, each of them taking their letter. The
    -- combinators are in random order, but they all succeed because the
    -- combinator is called "unordered".
  , CombinatorTestDataFn { inputStateFn     = "abcX"
                         , parsersFn        = [parserCharC, parserCharA, parserCharB]
                         , expectedOutputFn = Just ("X", "abc")
                         }

    -- First parser succeed, second and third fails. One out of three successful parsers is enough to succeed.
  , CombinatorTestDataFn { inputStateFn     = "aXY"
                         , parsersFn        = [parserCharA, parserCharB, parserCharC]
                         , expectedOutputFn = Just ("XY", "a")
                         }

    -- One of the parsers will succeed. "unordered" combinator will try to
    -- match them agains first input token ("e") until one of parsers
    -- succeeds or the combinator runs out of parsers to try.
  , CombinatorTestDataFn { inputStateFn     = "aXY"
                         , parsersFn        = [parserCharB, parserCharC, parserCharA]
                         , expectedOutputFn = Just ("XY", "a")
                         }

  -- First two parsers succeed, third fails. Two out of three successful parsers is enough.
  , CombinatorTestDataFn { inputStateFn     = "baXY"
                         , parsersFn        = [parserCharA, parserCharB, parserCharA]
                         , expectedOutputFn = Just ("XY", "ba")
                         }

    -- Check that we can use some parser tricks with our parsers that are
    -- then passed to our combinator.
  , CombinatorTestDataFn { inputStateFn     = "bacXY"
                         , parsersFn        = [parserCharB *> parserCharA, parserCharC]
                         , expectedOutputFn = Just ("XY", "ac")
                         }
  , CombinatorTestDataFn { inputStateFn     = "bacXY"
                         , parsersFn        = [parserCharB <* parserCharA, parserCharC]
                         , expectedOutputFn = Just ("XY", "bc")
                         }
  , CombinatorTestDataFn { inputStateFn     = "bcXY"
                         , parsersFn        = [many parserCharA *> parserCharB, parserCharC]
                         , expectedOutputFn = Just ("XY", "bc")
                         }
  , CombinatorTestDataFn { inputStateFn     = "aaabcXY"
                         , parsersFn        = [many parserCharA *> parserCharB, parserCharC]
                         , expectedOutputFn = Just ("XY", "bc")
                         }
  , CombinatorTestDataFn { inputStateFn     = "bcXY"
                         , parsersFn        = [some parserCharA *> parserCharB, parserCharC]
                         , expectedOutputFn = Nothing
                         }




    -- Failure cases

    -- Two parsers. None of parsers succeed (there is no token in input that
    -- could be taken by any of the parsers).
  , CombinatorTestDataFn { inputStateFn     = ""
                         , parsersFn        = [parserCharA, parserCharB]
                         , expectedOutputFn = Nothing
                         }

    -- There are zero parsers to run. This means that we can't meet the
    -- requirement of "one or more" successful parses.
  , CombinatorTestDataFn { inputStateFn     = ""
                         , parsersFn        = []
                         , expectedOutputFn = Nothing
                         }
  , CombinatorTestDataFn { inputStateFn     = "abc"
                         , parsersFn        = []
                         , expectedOutputFn = Nothing
                         }

    -- There is only one parser, but this parser doesn't have matching data in input, so we have zero matching parsers.
  , CombinatorTestDataFn { inputStateFn     = ""
                         , parsersFn        = [parserCharA]
                         , expectedOutputFn = Nothing
                         }
  , CombinatorTestDataFn { inputStateFn     = "X"
                         , parsersFn        = [parserCharA]
                         , expectedOutputFn = Nothing
                         }
  , CombinatorTestDataFn { inputStateFn     = "XY"
                         , parsersFn        = [parserCharA]
                         , expectedOutputFn = Nothing
                         }


    -- First token doesn't have a matching parser. The second and third token
    -- have matching parser, but the matching parsers will never be called.
  , CombinatorTestDataFn { inputStateFn     = "Xab"
                         , parsersFn        = [parserCharA, parserCharB, parserCharC]
                         , expectedOutputFn = Nothing
                         }

    -- Input string doesn't match the parsers at all.
  , CombinatorTestDataFn { inputStateFn     = "XYZ"
                         , parsersFn        = [parserCharA, parserCharB]
                         , expectedOutputFn = Nothing
                         }
  ]




{- -------------------------------------------------------------------------- -}




-- If some error is found, test function returns non-empty string which can
-- help identify a test that failed.
testCases :: [Test]
testCases =
  [
    TestCase (do assertEqual "manual tests of combinatorExactlyOne"              "" (combinatorTestFunction combinatorExactlyOne         combinatorExactlyOneTestData))
  , TestCase (do assertEqual "manual tests of combinatorAllInOrder"              "" (combinatorTestFunction combinatorAllInOrder         combinatorAllInOrderTestData))
  , TestCase (do assertEqual "manual tests of combinatorOneOrMoreUnordered"      "" (combinatorTestFunctionA combinatorOneOrMoreUnordered combinatorOneOrMoreUnorderedTestData))

  , TestCase (do assertEqual "manual tests of multiplierZeroOrOnce"              "" (multiplierTestFunction multiplierZeroOrOnce multiplierZeroOrOnceTestData))
  , TestCase (do assertEqual "manual tests of multiplierOnce"                    "" (multiplierTestFunction multiplierOnce       multiplierOnceTestData))

  , TestCase (do assertEqual "manual tests of combinatorOneOrMoreUnorderedFn"     "" (combinatorTestFunctionFn combinatorOneOrMoreUnorderedB       combinatorOneOrMoreUnorderedFnTestData))
  ]




testsCssParserCombinators :: IO String
testsCssParserCombinators = do
  testCounts <- runTestTT (TestList testCases)
  if errors testCounts + failures testCounts == 0
    then return ""
    else return "[EE] Hello.Tests.Css.Parser.Combinators failed"

