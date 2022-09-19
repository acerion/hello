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




import qualified Data.Text as T

import Debug.Trace

import Test.HUnit

import Hello.Css.Parser.Combinators

import Hello.Utils




-- --------------------------------------------------------------------------
-- Common definitions
-- --------------------------------------------------------------------------




-- Simple parser taking input string and trying to match a character.
type TestParser = MyParser T.Text Char




-- Parser function to be specialized with specific characters.
takeChar :: Char -> TestParser
takeChar char text = case T.uncons text of
                       Just (c, rem) | c == char -> (rem, Just [c])
                                     | otherwise -> (text, Nothing)
                       Nothing               -> (text, Nothing)




-- Finally our parsers that try to take/match specific character from input.
takeA = takeChar 'a'
takeB = takeChar 'b'
takeC = takeChar 'c'
takeD = takeChar 'd'
takeE = takeChar 'e'
takeF = takeChar 'f'
takeG = takeChar 'g'
takeH = takeChar 'h'
takeI = takeChar 'i'




data CombinatorTestData = CombinatorTestData
  { parsers          :: [TestParser]
  , input            :: T.Text
  , expectedOutput   :: (T.Text, Maybe [Char])
  }




data MultiplierTestData = MultiplierTestData
  { childItems2       :: TestParser
  , input2            :: T.Text
  , expectedOutput2   :: (T.Text, Maybe [Char])
  }




{- -------------------------------------------------------------------------- -}




-- We expect here that tested combinator will return some 'Just' only when a
-- single (one) parser from CombinatorTestData::parsers succeeds. When zero or
-- two or three parsers succeed, then the tested combinator should return
-- Nothing.
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





-- On success return empty string. On failure return string showing
-- approximately where the problem is.
combinatorExactlyOneTestFunction :: [CombinatorTestData] -> T.Text
combinatorExactlyOneTestFunction []     = ""
combinatorExactlyOneTestFunction (x:xs) = if not success
                                          then T.pack ("Got: " ++ show output ++ ", Expected: " ++ (show . expectedOutput $ x) ++ "; input = " ++ (show . input $ x))
                                          else combinatorExactlyOneTestFunction xs
  where
    success = output == (expectedOutput x)
    output = combinatorExactlyOne (parsers x) (input x)




{- -------------------------------------------------------------------------- -}




-- We expect here that tested combinator will return some 'Just' only when a
-- single (one) parser from CombinatorTestData::parsers succeeds. When zero or
-- two or three parsers succeed, then the tested combinator should return
-- Nothing.
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





-- On success return empty string. On failure return string showing
-- approximately where the problem is.
combinatorAllInOrderTestFunction :: [CombinatorTestData] -> T.Text
combinatorAllInOrderTestFunction []     = ""
combinatorAllInOrderTestFunction (x:xs) = if not success
                                          then T.pack ("Got: " ++ show output ++ ", Expected: " ++ (show . expectedOutput $ x) ++ "; input = " ++ (show . input $ x))
                                          else combinatorAllInOrderTestFunction xs
  where
    success = output == (expectedOutput x)
    output = combinatorAllInOrder (parsers x) (input x)




{- -------------------------------------------------------------------------- -}




-- We expect here that tested multiplier will return some 'Just' for zero
-- succeeses or one success.
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


    -- A set of parsers (a combinator) succeeds one time, and this is enough
    -- for tested multiplier to return success.
  , MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "abc",   expectedOutput2 = ("",     Just ['a', 'b', 'c']) }
  , MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "abcd",  expectedOutput2 = ("d",    Just ['a', 'b', 'c']) }
  , MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "abcab", expectedOutput2 = ("ab",   Just ['a', 'b', 'c']) }
  , MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "abcxy", expectedOutput2 = ("xy",   Just ['a', 'b', 'c']) }




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
  ]




-- On success return empty string. On failure return string showing
-- approximately where the problem is.
multiplierZeroOrOnceTestFunction :: [MultiplierTestData] -> T.Text
multiplierZeroOrOnceTestFunction []     = ""
multiplierZeroOrOnceTestFunction (x:xs) = if not success
                                          then T.pack ("Got: " ++ show output ++ ", Expected: " ++ (show . expectedOutput2 $ x) ++ "; input = " ++ (show . input2 $ x))
                                          else multiplierZeroOrOnceTestFunction xs
  where
    success = output == (expectedOutput2 x)
    output = multiplierZeroOrOnce (childItems2 x) (input2 x)




{- -------------------------------------------------------------------------- -}




-- If some error is found, test function returns non-empty string which can
-- help identify a test that failed.
testCases =
  [
    TestCase (do assertEqual "manual tests of combinatorExactlyOneTestFunction"              "" (combinatorExactlyOneTestFunction combinatorExactlyOneTestData))
  , TestCase (do assertEqual "manual tests of combinatorAllInOrderTestFunction"              "" (combinatorAllInOrderTestFunction combinatorAllInOrderTestData))
  , TestCase (do assertEqual "manual tests of multiplierZeroOrOnceTestFunction"              "" (multiplierZeroOrOnceTestFunction multiplierZeroOrOnceTestData))
  ]




testsCssParserCombinators :: IO String
testsCssParserCombinators = do
  counts <- runTestTT (TestList (testCases))
  if (errors counts + failures counts == 0)
    then return ""
    else return "[EE] Hello.Tests.Css.Parser.Combinators failed"

