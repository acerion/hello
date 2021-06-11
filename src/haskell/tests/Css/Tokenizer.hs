{-# LANGUAGE OverloadedStrings #-}




module TestsCssParser (testsCssTokenizer
                      ) where




import qualified Data.Text as T
import Test.HUnit
import System.Exit
import CssParser
import HelloUtils




-- Tests for parsing strings as <number-token> (CssTokNumX),
-- <percentage-token> (CssTokPercX) or <dimension-token> (CssTokDimX).
--
-- This array is called "Manual" because these tests were written manually.
-- Perhaps in the future I will write some generator of test data.
tokenizerNumbersTestManualData = [
  -- parser's remainder before     expected token           parser's remainder after

  -- Tests of <number-token>
    ( "0",                     CssTokNum $ CssNumI 0,                ""  )
  , ( "0}",                    CssTokNum $ CssNumI 0,                "}" )
  , ( "-0}",                   CssTokNum $ CssNumI 0,                "}" )
  , ( "10}",                   CssTokNum $ CssNumI 10,               "}" )
  , ( "+10}",                  CssTokNum $ CssNumI 10,               "}" )
  , ( "-10}",                  CssTokNum $ CssNumI (-10),            "}" )
  , ( "56298453554,",          CssTokNum $ CssNumI 56298453554,      "," )
  , ( "512 dragons",           CssTokNum $ CssNumI 512,              " dragons" )
  , ( "+512 dragons",          CssTokNum $ CssNumI 512,              " dragons" )
  , ( "-512 dragons",          CssTokNum $ CssNumI (-512),           " dragons" )

  , ( "0.0",                   CssTokNum $ CssNumF 0.0,              ""  )
  , ( "0.0}",                  CssTokNum $ CssNumF 0.0,              "}" )
  , ( "+0.0}",                 CssTokNum $ CssNumF 0.0,              "}" )
  , ( "-0.0}",                 CssTokNum $ CssNumF (-0.0),           "}" )
  , ( "0.1}",                  CssTokNum $ CssNumF 0.1,              "}" )
  , ( "+0.1}",                 CssTokNum $ CssNumF 0.1,              "}" )
  , ( "-0.1}",                 CssTokNum $ CssNumF (-0.1),           "}" )
  , ( "12.333}",               CssTokNum $ CssNumF 12.333,           "}" )
  , ( "+12.333}",              CssTokNum $ CssNumF 12.333,           "}" )
  , ( "-12.333}",              CssTokNum $ CssNumF (-12.333),        "}" )
  , ( "12345.1,",              CssTokNum $ CssNumF 12345.1,          "," )
  , ( "76.5 computers",        CssTokNum $ CssNumF 76.5,             " computers" )
  , ( "44.2,",                 CssTokNum $ CssNumF 44.2,             "," )
  -- , ( ".2,",                   CssTokNum $ CssNumF 0.2,              "," ) -- TODO: implement support for this format
  -- , ( "10e2}",                 CssTokNum $ CssNumF 1000.0,           "}" ) -- TODO: implement support for this format
  , ( "1.2e-3}",               CssTokNum $ CssNumF 0.0012,           "}" )
  , ( "+1.2e-3}",              CssTokNum $ CssNumF 0.0012,           "}" )
  , ( "-1.2e-3}",              CssTokNum $ CssNumF (-0.0012),        "}" )




  -- Tests of <percentage-token>
  , ( "13%;",                  CssTokPerc $ CssNumI 13,              ";" )
  , ( "-13%;",                 CssTokPerc $ CssNumI (-13),           ";" )
  , ( "+13%;",                 CssTokPerc $ CssNumI 13,              ";" )
  -- This is a percentage followed immediately by some identifier
  , ( "31%px",                 CssTokPerc $ CssNumI 31,              "px" )

  , ( "99.9%;",                CssTokPerc $ CssNumF 99.9,            ";" )
  , ( "-99.9%;",               CssTokPerc $ CssNumF (-99.9),         ";" )
  , ( "+99.9%;",               CssTokPerc $ CssNumF 99.9,            ";" )
  -- This is a percentage followed immediately by some identifier
  , ( "45.8%px",               CssTokPerc $ CssNumF 45.8,            "px" )




  -- Tests of <dimension-token>
  , ( "127px;",                CssTokDim (CssNumI 127) "px",         ";" )
  , ( "-1pt ",                 CssTokDim (CssNumI (-1)) "pt",        " " )
  , ( "+1pt ",                 CssTokDim (CssNumI 1) "pt",           " " )
  -- Tokenizer doesn't validate correctness of units, it just appends
  -- identifier to number to get <dimension-token>. Verifying units is done
  -- by parser, not tokenizer.
  , ( "15cars}",               CssTokDim (CssNumI 15) "cars",        "}" )

  , ( "99.9mm;",               CssTokDim (CssNumF 99.9) "mm",        ";" )
  , ( "-15.5in ",              CssTokDim (CssNumF (-15.5)) "in",     " " )
  , ( "+15.5in ",              CssTokDim (CssNumF 15.5) "in",        " " )
  -- Tokenizer doesn't validate correctness of units, it just appends
  -- identifier to number to get <dimension-token>. Verifying units is done
  -- by parser, not tokenizer.
  , ( "64.22cars}",            CssTokDim (CssNumF 64.22) "cars",     "}" )
  ]




-- On success return empty string. On failure return string representation of
-- remainder string in a row, for which test failed.
tokenizerNumbersTest :: [(T.Text, CssToken, T.Text)] -> T.Text
tokenizerNumbersTest []     = ""
tokenizerNumbersTest (x:xs) = if expectedToken /= token || remainderAfter /= (remainder parser)
                              then remainderBefore
                              else tokenizerNumbersTest xs
  where
    remainderBefore = tripletFst x
    expectedToken   = tripletSnd x
    remainderAfter  = tripletThrd x
    (parser, token) = nextToken defaultParser{remainder = remainderBefore}





-- Tests for parsing strings as <hash-token> (CssTokHash).
--
-- This array is called "Manual" because these tests were written manually.
-- Perhaps in the future I will write some generator of test data.
tokenizerHashTestManualData = [
  -- parser's remainder before     expected token           parser's remainder after

    ( "#0",                    CssTokHash "0",                        ""  )
  , ( "#02553}",               CssTokHash "02553",                    "}" )
  , ( "#name+",                CssTokHash "name",                     "+" )
  , ( "#aD-9_1%",              CssTokHash "aD-9_1",                   "%" )
  ]




-- On success return empty string. On failure return string representation of
-- remainder string in a row, for which test failed.
tokenizerHashTest :: [(T.Text, CssToken, T.Text)] -> T.Text
tokenizerHashTest []     = ""
tokenizerHashTest (x:xs) = if expectedToken /= token || remainderAfter /= (remainder parser)
                           then remainderBefore
                           else tokenizerHashTest xs
  where
    remainderBefore = tripletFst x
    expectedToken   = tripletSnd x
    remainderAfter  = tripletThrd x
    (parser, token) = nextToken defaultParser{remainder = remainderBefore, inBlock = True}




tokenizerTestCases = [
  -- If some error is found, test function returns non-empty string with
  -- representation of token from first column in a row, for which the test
  -- failed.
     TestCase (do
                 assertEqual "manual tests of numbers" "" (tokenizerNumbersTest tokenizerNumbersTestManualData))

   , TestCase (do
                 assertEqual "manual tests of hash"  "" (tokenizerHashTest tokenizerHashTestManualData))
  ]




testsCssTokenizer :: IO ()
testsCssTokenizer = do
  counts <- runTestTT (TestList (tokenizerTestCases))
  if (errors counts + failures counts == 0)
    then exitSuccess
    else exitFailure

