{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-} -- For specifying expected integer values of CssValueTypeMultiEnum.




module Hello.Tests.Css.Tokenizer (testsCssTokenizer
                                 )
where




import qualified Data.Text as T
import Test.HUnit
import System.Exit

import Hello.Css.Tokenizer
import Hello.Utils




-- Tests for parsing strings as <number-token> (CssTokNumX),
-- <percentage-token> (CssTokPercX) or <dimension-token> (CssTokDimX).
--
-- This array is called "Manual" because these tests were written manually.
-- Perhaps in the future I will write some generator of test data.
--
-- We are testing <number-token>'s here. Put space before alphabetical string
-- that come after numeric value, otherwise that string will be treated as
-- ident, interpreted as unit, and the whole token will be of type CssTokDim.
tokenizerNumericTokenTestManualData = [
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
  , ( ".2,",                   CssTokNum $ CssNumF 0.2,              "," )
  , ( "-.4;",                  CssTokNum $ CssNumF (-0.4),           ";" )
  , ( "+.5;",                  CssTokNum $ CssNumF 0.5,              ";" )

  -- Exponential notation. Don't forget about capital 'E'.
  , ( "10e2}",                 CssTokNum $ CssNumF 1000.0,           "}" )
  , ( "10E2}",                 CssTokNum $ CssNumF 1000.0,           "}" )
  , ( "10e+2 px",              CssTokNum $ CssNumF 1000.0,           " px" )
  , ( "3e-2}",                 CssTokNum $ CssNumF 3e-2,             "}" )
  , ( ".5e-2{",                CssTokNum $ CssNumF 0.5e-2,           "{" )
  , ( "-.7e+3{",               CssTokNum $ CssNumF (-0.7e+3),        "{" )
  , ( "-.7E+3{",               CssTokNum $ CssNumF (-0.7e+3),        "{" )
  , ( "-.721e+11{",            CssTokNum $ CssNumF (-0.721e+11),     "{" ) -- With few more digits
  , ( "-.734E+13{",            CssTokNum $ CssNumF (-0.734e+13),     "{" ) -- With few more digits
  , ( "+.4e+4,",               CssTokNum $ CssNumF 0.4e+4,           "," )
  , ( "1.2e-3}",               CssTokNum $ CssNumF (1.2e-3),         "}" )
  , ( "+1.2e-3}",              CssTokNum $ CssNumF (1.2e-3),         "}" )
  , ( "-1.2e-3}",              CssTokNum $ CssNumF (-1.2e-3),        "}" )
  , ( "+12.21e-4}",            CssTokNum $ CssNumF (12.21e-4),       "}" ) -- With few more digits
  , ( "-97.54e-2}",            CssTokNum $ CssNumF (-97.54e-2),      "}" ) -- With few more digits




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
  , ( "1.17em}",               CssTokDim (CssNumF 1.17) "em",        "}" )


  -- Verify that non-number tokens aren't interpreted as numbers.
  , ( "binary{",               CssTokIdent "binary",                 "{" )
  -- Something that a buggy tokenizer may try to fix by prepending zero to
  -- form a valid float.
  , ( ".almostFloat;",         CssTokCh '.',                         "almostFloat;" )



  -- Tricky valid numbers
  -- "e" pretends to be part of float's exponent notantion, but is not
  -- followed by digits. Thus it must be treated as unit of a float.
  -- In particular make sure that CSS "em" unit is handled correctly.
  , ( "+34.4e",                CssTokDim (CssNumF 34.4) "e",         ""  )
  , ( "1.17em",                CssTokDim (CssNumF 1.17) "em",        ""  )
  , ( "+34.4e}",               CssTokDim (CssNumF 34.4) "e",         "}" )
  , ( "1.17em}",               CssTokDim (CssNumF 1.17) "em",        "}" )
  ]




-- On success return empty string. On failure return string representation of
-- remainder string in a row, for which test failed.
tokenizerNumericTokenTest :: [(T.Text, CssToken, T.Text)] -> T.Text
tokenizerNumericTokenTest []     = ""
tokenizerNumericTokenTest (x:xs) = if expectedToken /= token || expectedRemainder /= (remainder parser)
                                   then T.pack . showFailedCase $ x
                                   else tokenizerNumericTokenTest xs
  where
    remainderBefore = tripletFst x
    expectedToken   = tripletSnd x
    expectedRemainder  = tripletThrd x
    (parser, token) = nextToken1 defaultParser{remainder = remainderBefore}

    showFailedCase x =    "Input remainder = "    ++ (show remainderBefore) ++ "; "
                       ++ "Expected remainder = " ++ (show expectedRemainder) ++ "; "
                       ++ "Expected token = "     ++ (show expectedToken) ++ "; "
                       ++ "Output token = "       ++ (show token) ++ "; "





-- Tests for parsing strings as <hash-token> (CssTokHash).
--
-- This array is called "Manual" because these tests were written manually.
-- Perhaps in the future I will write some generator of test data.
tokenizerHashTokenTestManualData = [
  -- parser's remainder before     expected token           parser's remainder after

    ( "#0",                    CssTokHash "0",                        ""  )
  , ( "#02553}",               CssTokHash "02553",                    "}" )
  , ( "#name+",                CssTokHash "name",                     "+" )
  , ( "#aD-9_1%",              CssTokHash "aD-9_1",                   "%" )
  ]




-- On success return empty string. On failure return string representation of
-- remainder string in a row, for which test failed.
tokenizerHashTokenTest :: [(T.Text, CssToken, T.Text)] -> T.Text
tokenizerHashTokenTest []     = ""
tokenizerHashTokenTest (x:xs) = if expectedToken /= token || remainderAfter /= (remainder parser)
                                then remainderBefore
                                else tokenizerHashTokenTest xs
  where
    remainderBefore = tripletFst x
    expectedToken   = tripletSnd x
    remainderAfter  = tripletThrd x
    (parser, token) = nextToken1 defaultParser{remainder = remainderBefore, inBlock = True}




-- Tests for parsing strings as <ident-token> (CssTokIdent).
--
-- This array is called "Manual" because these tests were written manually.
-- Perhaps in the future I will write some generator of test data.
tokenizerIdentTokenTestManualData = [
  -- parser's remainder before     expected token                  parser's remainder after

    ( "a",                         CssTokIdent "a",                ""  )
  , ( "a}",                        CssTokIdent "a",                "}" )
  , ( "BODY}",                     CssTokIdent "BODY",             "}" )
  , ( "BODY}",                     CssTokIdent "BODY",             "}" )

  -- Taken from real css: ".opera-only :-o-prefocus,.pure-g{word-spacing:-.43em}"
  , ( "-o-prefocus,",              CssTokIdent "-o-prefocus",      "," )

  -- Escape sequence, taken from real css:
  -- ".pure-button-active,.pure-button:active{box-shadow:0 0 0 1px rgba(0,0,0,.15) inset,0 0 6px rgba(0,0,0,.2) inset;border-color:#000\9}"
  , ( "\\9",                       CssTokIdent "\t",               "" )
  , ( "\\9}",                      CssTokIdent "\t",               "}" )

  , ( "\\017C}",                   CssTokIdent "ż",                "}" ) -- Polish letter "LATIN SMALL LETTER Z WITH DOT ABOVE"
  ]




tokenizerIdentTokenTest = tokenizerNumericTokenTest




tokenizerTestCases = [
  -- If some error is found, test function returns some data (e.g. non-empty
  -- string or test index) which can help identify which test failed.
     TestCase (do assertEqual "manual tests of numeric token"    "" (tokenizerNumericTokenTest tokenizerNumericTokenTestManualData))
   , TestCase (do assertEqual "manual tests of hash token"       "" (tokenizerHashTokenTest tokenizerHashTokenTestManualData))
   , TestCase (do assertEqual "manual tests of ident token"      "" (tokenizerIdentTokenTest tokenizerIdentTokenTestManualData))
  ]




testsCssTokenizer :: IO ()
testsCssTokenizer = do
  counts <- runTestTT (TestList (tokenizerTestCases))
  if (errors counts + failures counts == 0)
    then exitSuccess
    else exitFailure

