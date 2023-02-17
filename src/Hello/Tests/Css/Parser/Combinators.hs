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

{-
takeB :: TestParser
takeB = takeChar 'b'

takeC :: TestParser
takeC = takeChar 'c'
-}



data MultiplierTestData = MultiplierTestData
  { childItems2       :: TestParser
  , input2            :: T.Text
  , expectedOutput2   :: (T.Text, Maybe [Char])
  }




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




-- Parser matching any char.
parserCharAny :: Parser T.Text (T.Text -> T.Text)
parserCharAny = Parser $ \ txt -> case T.uncons txt of
                                    Just (_, remd) -> Just (remd, id)
                                    Nothing        -> Nothing




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




{- -------------------------------------------------------------------------- -}




-- We expect here that tested combinator will return some 'Just' only when a
-- single (one) parser from CombinatorTestData::parsers succeeds. When zero or
-- two or three parsers succeed, then the tested combinator should return
-- Nothing.
--
-- https://www.w3.org/TR/css-values-4/#component-combinators:
-- "A bar (|) separates two or more alternatives: exactly one of them must occur."
--
-- My assumption here is that all parses are tried agains head of input - all
-- N of them on the same head of the same input. If exactly one of them
-- succeeds for the head of the same input, then it's success. Otherwise it's
-- failure.
combinatorExactlyOneTestData :: [CombinatorTestDataFn]
combinatorExactlyOneTestData =
  [
    -- Success cases

    -- I'm not sure if for 'alternative' combinator it makes sense to create
    -- a test case, but here it goes.
    CombinatorTestDataFn { inputStateFn     = "a"
                         , parsersFn        = [parserCharA]
                         , expectedOutputFn = Just ("", "a") }
  , CombinatorTestDataFn { inputStateFn     = "ab"
                         , parsersFn        = [parserCharA]
                         , expectedOutputFn = Just ("b", "a") }

    -- Only one of the two given parsers will succeed for given input.
  , CombinatorTestDataFn { inputStateFn     = "atb"
                         , parsersFn        = [parserCharA, parserCharB]
                         , expectedOutputFn = Just ("tb", "a") }

    -- Only one of the two given parsers will succeed for given input.
  , CombinatorTestDataFn { inputStateFn     = "atb"
                         , parsersFn        = [parserCharB, parserCharA]
                         , expectedOutputFn = Just ("tb", "a") }

    -- Only one of the three given parsers will succeed for given input.
  , CombinatorTestDataFn { inputStateFn     = "a12"
                         , parsersFn        = [parserCharA, parserCharB, parserCharC]
                         , expectedOutputFn = Just ("12", "a") }

    -- Only one of the two given parsers will succeed for given input.
  , CombinatorTestDataFn { inputStateFn     = "btq"
                         , parsersFn        = [parserCharA, parserCharB, parserCharC]
                         , expectedOutputFn = Just ("tq", "b") }

    -- Only one of the three given parsers will succeed for given input.
  , CombinatorTestDataFn { inputStateFn     = "c~+"
                         , parsersFn        = [parserCharA, parserCharB, parserCharC]
                         , expectedOutputFn = Just ("~+", "c") }

    -- Only one of the three parsers will succeed for given input.
  , CombinatorTestDataFn { inputStateFn     = "cab"
                         , parsersFn        = [parserCharA, parserCharB, parserCharC]
                         , expectedOutputFn = Just ("ab", "c") }

    -- Only one of the three given parsers will succeed for given input.
  , CombinatorTestDataFn { inputStateFn     = "a"
                         , parsersFn        = [parserCharA, parserCharB, parserCharC]
                         , expectedOutputFn = Just ("", "a") }



    -- Failure cases

    -- I'm not sure if for 'alternative' combinator it makes sense to create
    -- a test case, but here it goes.

    -- There are zero parsersFn to run, so there cannot be one parser to succeed.
  , CombinatorTestDataFn { inputStateFn     = "abc"
                         , parsersFn        = []
                         , expectedOutputFn = Nothing }
    -- There is only one parser, but this parser doesn't have matching data
    -- in inputStateFn, so we have zero matching parsersFn.
  , CombinatorTestDataFn { inputStateFn     = "o"
                         , parsersFn        = [parserCharA]
                         , expectedOutputFn = Nothing }
  , CombinatorTestDataFn { inputStateFn     = "oh"
                         , parsersFn        = [parserCharA]
                         , expectedOutputFn = Nothing }


    -- Two parsers, both succeed. Two successful parsers is too much.
  , CombinatorTestDataFn { inputStateFn     = "a"
                         , parsersFn        = [parserCharA, parserCharA]
                         , expectedOutputFn = Nothing }

    -- Two parsers, none of them succeed. Zero successes is not "exactly one".
  , CombinatorTestDataFn { inputStateFn     = "two"
                         , parsersFn        = [parserCharA, parserCharB]
                         , expectedOutputFn = Nothing }

    -- All three parsers succeed, each of them taking their letter. Three successful parsers is too much.
  , CombinatorTestDataFn { inputStateFn     = "abc"
                         , parsersFn        = [parserCharA, parserCharA, parserCharA]
                         , expectedOutputFn = Nothing }

    -- Both parsers would succeed.
  , CombinatorTestDataFn { inputStateFn     = "abc"
                         , parsersFn        = [parserCharA, parserCharAny]
                         , expectedOutputFn = Nothing }

    -- First and third parser would succeed. Two successful parsers is too much.
  , CombinatorTestDataFn { inputStateFn     = "abT"
                         , parsersFn        = [parserCharA, parserCharB, parserCharA],
                           expectedOutputFn = Nothing }

    -- First parser fails, but the second and third succeed. Two successful parsers is too much.
  , CombinatorTestDataFn { inputStateFn     = "1bc"
                         , parsersFn        = [parserCharA, parserCharB, parserCharC]
                         , expectedOutputFn = Nothing }

    -- None of acceptable letters are in inputStateFn data, so zero tests pass. Zero is not "exactly one".
  , CombinatorTestDataFn { inputStateFn     = "xyz"
                         , parsersFn        = [parserCharA, parserCharB, parserCharC]
                         , expectedOutputFn = Nothing }

    -- InputStateFn string is empty, so none of parsersFn succeed. Zero is not "exactly one".
  , CombinatorTestDataFn { inputStateFn     = ""
                         , parsersFn        = [parserCharA, parserCharB, parserCharC]
                         , expectedOutputFn = Nothing }
  ]





{- -------------------------------------------------------------------------- -}




-- We expect here that tested combinator will return some 'Just' only when a
-- single (one) parser from CombinatorTestDataFn::parsersFn succeeds. When zero or
-- two or three parsersFn succeed, then the tested combinator should return
-- Nothing.
combinatorAllInOrderTestData :: [CombinatorTestDataFn]
combinatorAllInOrderTestData =
  [
    -- Success cases

    -- There is only one parser, and this parser has matching data in inputStateFn, so we have a success.
    CombinatorTestDataFn { inputStateFn     = "a"
                         , parsersFn        = [parserCharA]
                         , expectedOutputFn = Just ("", "a") }
  , CombinatorTestDataFn { inputStateFn     = "ab"
                         , parsersFn        = [parserCharA]
                         , expectedOutputFn = Just ("b", "a") }

    -- Two parsers, both of them succeed.
  , CombinatorTestDataFn { inputStateFn     = "ab"
                         , parsersFn        = [parserCharA, parserCharB]
                         , expectedOutputFn = Just ("", "ab") }

    -- Two parsers, both of them succeed.
  , CombinatorTestDataFn { inputStateFn     = "abc"
                         , parsersFn        = [parserCharA, parserCharB]
                         , expectedOutputFn = Just ("c", "ab") }

    -- Two parsers, both of them succeed.
  , CombinatorTestDataFn { inputStateFn     = "abe"
                         , parsersFn        = [parserCharA, parserCharB]
                         , expectedOutputFn = Just ("e", "ab") }

    -- All three parsers succeed, each of them taking their letter.
  , CombinatorTestDataFn { inputStateFn     = "abc"
                         , parsersFn        = [parserCharA, parserCharB, parserCharC]
                         , expectedOutputFn = Just ("", "abc") }



    -- Failure cases

    -- Two parsersFn. There are matching letters in inputStateFn string for both of
    -- them, but they are separated by a letter that won't be parserCharn by any of
    -- the two parsersFn.
  , CombinatorTestDataFn { inputStateFn     = "acb"
                         , parsersFn        = [parserCharA, parserCharB]
                         , expectedOutputFn = Nothing }

    -- Only first parser succeeds: it matches the first letter. But the other
    -- two don't match, so no success overall.
  , CombinatorTestDataFn { inputStateFn     = "a12"
                         , parsersFn        = [parserCharA, parserCharB, parserCharC]
                         , expectedOutputFn = Nothing }

  -- Only second parser would succeed, but the other two don't match, so no success overall.
  , CombinatorTestDataFn { inputStateFn     = "btq"
                         , parsersFn        = [parserCharA, parserCharB, parserCharC]
                         , expectedOutputFn = Nothing }

    -- Only third parser would succeed, but the other two don't match, so no success overall.
  , CombinatorTestDataFn { inputStateFn     = "c~+"
                         , parsersFn        = [parserCharA, parserCharB, parserCharC]
                         , expectedOutputFn = Nothing }

    -- All the letters are there in the inputStateFn, but in wrong order.
  , CombinatorTestDataFn { inputStateFn     = "cab"
                         , parsersFn        = [parserCharA, parserCharB, parserCharC]
                         , expectedOutputFn = Nothing }

    -- There is only one letter in inputStateFn data, and it is successfully parsed.
    -- The other two parsersFn will fail, so overall it's a fail.
  , CombinatorTestDataFn { inputStateFn     = "a"
                         , parsersFn        = [parserCharA, parserCharB, parserCharC]
                         , expectedOutputFn = Nothing }

  {-
    -- There are zero parsersFn to run. On one hand there are no parsersFn that
    -- could fail, but on the other hand there is not a single parser that
    -- would return some accumulator.
    --
    -- TODO: how this case should behave?
  , CombinatorTestDataFn { inputStateFn = "abc", parsersFn = [], expectedOutputFn = ("abc", Nothing) }
  -}


    -- There is only one parser, but this parser doesn't have matching data in inputStateFn, so we have zero matching parsersFn.
  , CombinatorTestDataFn { inputStateFn     = ""
                         , parsersFn        = [parserCharA]
                         , expectedOutputFn = Nothing }
  , CombinatorTestDataFn { inputStateFn     = "o"
                         , parsersFn        = [parserCharA]
                         , expectedOutputFn = Nothing }
  , CombinatorTestDataFn { inputStateFn     = "oh"
                         , parsersFn        = [parserCharA]
                         , expectedOutputFn = Nothing }
  , CombinatorTestDataFn { inputStateFn     = "oa"
                         , parsersFn        = [parserCharA]
                         , expectedOutputFn = Nothing }

    -- Two parsersFn, none of them has matching letter.
  , CombinatorTestDataFn { inputStateFn     = "two"
                         , parsersFn        = [parserCharA, parserCharB]
                         , expectedOutputFn = Nothing }

    -- Three parsersFn, none of them has matching letter.
  , CombinatorTestDataFn { inputStateFn     = "xyz"
                         , parsersFn        = [parserCharA, parserCharB, parserCharC]
                         , expectedOutputFn = Nothing }

    -- First two parsersFn succeed, third fails. Two out of three successful parsersFn is not enough.
  , CombinatorTestDataFn { inputStateFn     = "abT"
                         , parsersFn        = [parserCharA, parserCharB, parserCharC]
                         , expectedOutputFn = Nothing }

    -- First parser fails, but the second and third would succeed. But they will never be called.
  , CombinatorTestDataFn { inputStateFn     = "1bc"
                         , parsersFn        = [parserCharA, parserCharB, parserCharC]
                         , expectedOutputFn = Nothing }

    -- InputStateFn string is empty, so none of parsersFn succeed. Zero is not "exactly one".
  , CombinatorTestDataFn { inputStateFn     = ""
                         , parsersFn        = [parserCharA, parserCharB, parserCharC]
                         , expectedOutputFn = Nothing }
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
  --, MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "",    expectedOutput2 = ("",       Just []) }
  --, MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "a",   expectedOutput2 = ("a",      Just []) }
  --, MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "ab",  expectedOutput2 = ("ab",     Just []) }
  --, MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "acb", expectedOutput2 = ("acb",    Just []) }
  --, MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "xyz", expectedOutput2 = ("xyz",    Just []) }

    -- A set of parsers (a combinator) is successfully applied once (only
    -- one/first application of the set was successful). This means that
    -- multiplier succeeded in applying the set of parsers once. This means
    -- that multiplier can return success.
  --, MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "abc",   expectedOutput2 = ("",     Just ['a', 'b', 'c']) }
  --, MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "abcd",  expectedOutput2 = ("d",    Just ['a', 'b', 'c']) }
  --, MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "abcab", expectedOutput2 = ("ab",   Just ['a', 'b', 'c']) }
  --, MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "abcxy", expectedOutput2 = ("xy",   Just ['a', 'b', 'c']) }

    -- A set of parsers (a combinator) is successfully applied once (only
    -- one/first application of the set was successful). This means that
    -- multiplier succeeded in applying the set of parsers once. This means
    -- that multiplier can return success.
  --, MultiplierTestData { childItems2 = combinatorOneOrMoreUnordered [takeE, takeF, takeG],   input2 = "eXY",   expectedOutput2 = ("XY",   Just ['e']) }
  --, MultiplierTestData { childItems2 = combinatorOneOrMoreUnordered [takeE, takeF, takeG],   input2 = "efXY",  expectedOutput2 = ("XY",   Just ['e', 'f']) }
  --, MultiplierTestData { childItems2 = combinatorOneOrMoreUnordered [takeE, takeF, takeG],   input2 = "efgXY", expectedOutput2 = ("XY",   Just ['e', 'f', 'g']) }


    -- A set of parsers (a combinator) is successfully applied zero times
    -- (zero applications of the set was successful). This means that
    -- multiplier succeeded in applying the set of parsers zero times. This
    -- means that this particular multiplier can return success.
  --, MultiplierTestData { childItems2 = combinatorOneOrMoreUnordered [takeE, takeF, takeG],   input2 = "XYZ",   expectedOutput2 = ("XYZ",   Just []) }





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
  --, MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "abcabc",   expectedOutput2 = ("abcabc",   Nothing) }
  --, MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "abcabcd",  expectedOutput2 = ("abcabcd",  Nothing) }

    -- A set of parsers (a combinator) is successfully applied twice (two
    -- applications of the set were successful). This means that multiplier
    -- failed in applying the set of parsers just once. This means that
    -- multiplier cannot return success.
  --, MultiplierTestData { childItems2 = combinatorOneOrMoreUnordered [takeE, takeF, takeG],   input2 = "eeWZ",     expectedOutput2 = ("eeWZ",     Nothing) }
  --, MultiplierTestData { childItems2 = combinatorOneOrMoreUnordered [takeE, takeF, takeG],   input2 = "efefWZ",   expectedOutput2 = ("efefWZ",   Nothing) }
  --, MultiplierTestData { childItems2 = combinatorOneOrMoreUnordered [takeE, takeF, takeG],   input2 = "efgefgWZ", expectedOutput2 = ("efgefgWZ", Nothing) }
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
  --, MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "abc",   expectedOutput2 = ("",     Just ['a', 'b', 'c']) }
  --, MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "abcd",  expectedOutput2 = ("d",    Just ['a', 'b', 'c']) }
  --, MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "abcab", expectedOutput2 = ("ab",   Just ['a', 'b', 'c']) }
  --, MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "abcxy", expectedOutput2 = ("xy",   Just ['a', 'b', 'c']) }




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
  --, MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "",    expectedOutput2 = ("",       Nothing) }
  --, MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "a",   expectedOutput2 = ("a",      Nothing) }
  --, MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "ab",  expectedOutput2 = ("ab",     Nothing) }
  --, MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "acb", expectedOutput2 = ("acb",    Nothing) }
  --, MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "xyz", expectedOutput2 = ("xyz",    Nothing) }

    -- A parser has two or more matching pieces of data in input (will be
    -- able to take two or more 'a' tokens). Two or more matches of a parser
    -- is too much for the tested multiplier, and the tested multiplier must
    -- return failure.
  , MultiplierTestData { childItems2 = takeA,               input2 = "aa",   expectedOutput2 = ("aa",  Nothing) }
  , MultiplierTestData { childItems2 = takeA,               input2 = "aaa",  expectedOutput2 = ("aaa", Nothing) }
  , MultiplierTestData { childItems2 = takeA,               input2 = "aab",  expectedOutput2 = ("aab", Nothing) }

    -- A set of parsers (a combinator) succeeds two times, but this is too
    -- much for tested multiplier.
  --, MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "abcabc",   expectedOutput2 = ("abcabc",   Nothing) }
  --, MultiplierTestData { childItems2 = combinatorAllInOrder [takeA, takeB, takeC],   input2 = "abcabcd",  expectedOutput2 = ("abcabcd",  Nothing) }

    -- A set of parsers (a combinator) is successfully applied zero times
    -- (none of applications of the set were successful). This means that the
    -- multipier didn't succeed once and so it cannot return success.
  --, MultiplierTestData { childItems2 = combinatorOneOrMoreUnordered [takeE, takeF, takeG],   input2 = "XYZ",   expectedOutput2 = ("XYZ",   Nothing) }
  ]




{- -------------------------------------------------------------------------- -}


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
    TestCase (do assertEqual "manual tests of multiplierZeroOrOnce"              "" (multiplierTestFunction multiplierZeroOrOnce multiplierZeroOrOnceTestData))
  , TestCase (do assertEqual "manual tests of multiplierOnce"                    "" (multiplierTestFunction multiplierOnce       multiplierOnceTestData))

  , TestCase (do assertEqual "manual tests of combinatorAllInOrder"              "" (combinatorTestFunctionFn combinatorAllInOrder combinatorAllInOrderTestData))
  , TestCase (do assertEqual "manual tests of combinatorExactlyOne"              "" (combinatorTestFunctionFn combinatorExactlyOne combinatorExactlyOneTestData))
  , TestCase (do assertEqual "manual tests of combinatorOneOrMoreUnordered"      "" (combinatorTestFunctionFn combinatorOneOrMoreUnordered combinatorOneOrMoreUnorderedFnTestData))
  ]




testsCssParserCombinators :: IO String
testsCssParserCombinators = do
  testCounts <- runTestTT (TestList testCases)
  if errors testCounts + failures testCounts == 0
    then return ""
    else return "[EE] Hello.Tests.Css.Parser.Combinators failed"

