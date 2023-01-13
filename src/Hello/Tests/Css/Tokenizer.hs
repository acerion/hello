{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-} -- For specifying expected integer values of CssValueTypeMultiEnum.




module Hello.Tests.Css.Tokenizer
  (
    testsCssTokenizer
  )
where




import qualified Data.Text as T
import Test.HUnit

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
numericTokenTestManualData :: [(T.Text, CssToken, T.Text)]
numericTokenTestManualData = [
  -- parser's initial remainder     expected token           parser's remainder after

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
  , ( ".almostFloat;",         CssTokDelim '.',                      "almostFloat;" )



  -- Tricky valid numbers
  -- "e" pretends to be part of float's exponent notantion, but is not
  -- followed by digits. Thus it must be treated as unit of a float.
  -- In particular make sure that CSS "em" unit is handled correctly.
  , ( "+34.4e",                CssTokDim (CssNumF 34.4) "e",         ""  )
  , ( "1.17em",                CssTokDim (CssNumF 1.17) "em",        ""  )
  , ( "+34.4e}",               CssTokDim (CssNumF 34.4) "e",         "}" )
  , ( "1.17em}",               CssTokDim (CssNumF 1.17) "em",        "}" )
  ]




-- Tests for parsing strings as <hash-token> (CssTokHashId or CssTokHashUn).
--
-- This array is called "Manual" because these tests were written manually.
-- Perhaps in the future I will write some generator of test data.
hashTokenTestManualData :: [(T.Text, CssToken, T.Text)]
hashTokenTestManualData = [
  -- parser's initial remainder    expected token                  parser's remainder after

    ( "#0",                        CssTokHash CssHashUn "0",               ""  )
  , ( "#012;",                     CssTokHash CssHashUn "012",             ";" )
  , ( "#02553}",                   CssTokHash CssHashUn "02553",           "}" )
  , ( "#01234567890!",             CssTokHash CssHashUn "01234567890",     "!" ) -- Length of a regular text should not matter.

    -- Escape. 91(hex) -> 145(dec). Valid escape is an ID.
  , ( "#\\91",                     CssTokHash CssHashId "\145",            ""  )
  , ( "#\\91;",                    CssTokHash CssHashId "\145",            ";" )
  , ( "#\\r",                      CssTokHash CssHashId "r",               ""  )
  , ( "#\\r@",                     CssTokHash CssHashId "r",               "@" )

  , ( "#name+",                    CssTokHash CssHashId "name",            "+" )
  , ( "#-ident}",                  CssTokHash CssHashId "-ident",          "}" )
  , ( "#aD-9_1%",                  CssTokHash CssHashId "aD-9_1",          "%" )

    -- Not enough good code points to build a proper hash token.
  , ( "#;",                        CssTokDelim '#',                        ";" )
  ]




-- Tests for parsing strings as tokens generated from single character.
--
-- This array is called "Manual" because these tests were written manually.
-- Perhaps in the future I will write some generator of test data.
singleCharTokenTestManualData :: [(T.Text, CssToken, T.Text)]
singleCharTokenTestManualData = [
  -- parser's initial remainder    expected token                  parser's remainder after

    ( ":coffe",                    CssTokColon,                    "coffe"         )
  , ( "::",                        CssTokColon,                    ":"             )
  , ( ":",                         CssTokColon,                    ""              )
  , ( ": ",                        CssTokColon,                    " "             )

  , ( ";next",                     CssTokSemicolon,                "next"          )
  , ( ";} div",                    CssTokSemicolon,                "} div"         )

  , ( ", code, kbd",               CssTokComma,                    " code, kbd"    )
  , ( ", sans-serif;",             CssTokComma,                    " sans-serif;"  )
  , ( ", #submit,",                CssTokComma,                    " #submit,"     )
  , ( ",0,0, 0.2);",               CssTokComma,                    "0,0, 0.2);"    )

  , ( "[type",                     CssTokBraceSquareOpen,          "type"          )
  , ( "[ ",                        CssTokBraceSquareOpen,          " "             )
  , ( "[",                         CssTokBraceSquareOpen,          ""              )

  , ( "],",                        CssTokBraceSquareClose,         ","             )
  , ( "]",                         CssTokBraceSquareClose,         ""              )
  , ( "] ",                        CssTokBraceSquareClose,         " "             )
  , ( "] ",                        CssTokBraceSquareClose,         " "             )
  , ( "]:hover",                   CssTokBraceSquareClose,         ":hover"        )
  , ( "] {border",                 CssTokBraceSquareClose,         " {border"      )

  , ( "(0,0,0)",                   CssTokParenOpen,                "0,0,0)"        )
  , ( "(\"//server",               CssTokParenOpen,                "\"//server"    )
  , ( "(#600",                     CssTokParenOpen,                "#600"          )
  , ( "(",                         CssTokParenOpen,                ""              )

  , ( "), transparent",            CssTokParenClose,               ", transparent" )
  , ( "); a",                      CssTokParenClose,               "; a"           )
  , ( ")",                         CssTokParenClose,               ""              )
  , ( ") ",                        CssTokParenClose,               " "             )

  , ( "{ background",              CssTokBraceCurlyOpen,           " background"   )
  , ( "{{color",                   CssTokBraceCurlyOpen,           "{color"        )
  , ( "{{",                        CssTokBraceCurlyOpen,           "{"             )
  , ( "{",                         CssTokBraceCurlyOpen,           ""              )

  , ( "}.commentTop",              CssTokBraceCurlyClose,          ".commentTop"   )
  , ( "}}.commentTop",             CssTokBraceCurlyClose,          "}.commentTop"  )
  , ( "}",                         CssTokBraceCurlyClose,          ""              )
  , ( "} ",                        CssTokBraceCurlyClose,          " "             )
  , ( "} div",                     CssTokBraceCurlyClose,          " div"          )
  ]




-- Tests for parsing strings as <ident-token> (CssTokIdent).
--
-- This array is called "Manual" because these tests were written manually.
-- Perhaps in the future I will write some generator of test data.
identTokenTestManualData :: [(T.Text, CssToken, T.Text)]
identTokenTestManualData = [
  -- parser's initial remainder    expected token                  parser's remainder after

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




-- Tests for parsing strings as <function-token>, <url-token>,
-- <bad-url-token> (CssTokFunc, CssTokUrl, CssTokBadUrl).
--
-- This array is called "Manual" because these tests were written manually.
-- Perhaps in the future I will write some generator of test data.
functionUrlTokenTestManualData :: [(T.Text, CssToken, T.Text)]
functionUrlTokenTestManualData = [
  -- parser's initial remainder    expected token                  parser's remainder after

    -- Test that some cases of "url" are not recognized as <function-token>
    ( "url",                            CssTokIdent "url",                   ""  )
  , ( "url(a)",                         CssTokUrl "a",                       ""  )

    -- As long as two leading whitespaces are encountered, tokenizer removes
    -- first of them.
  , ( "url(a)",                       CssTokUrl "a",                         ""  )
  , ( "url( a)",                      CssTokUrl " a",                        ""  )
  , ( "url(  a)",                     CssTokUrl " a",                        ""  )
  , ( "url(   a)",                    CssTokUrl " a",                        ""  )

    -- Recognition of "url" in <url-token> should be case-insensitive
  , ( "url(a)",                       CssTokUrl "a",                         ""  )
  , ( "URL(a)",                       CssTokUrl "a",                         ""  )
  , ( "Url(a)",                       CssTokUrl "a",                         ""  )
  , ( "uRl(a)",                       CssTokUrl "a",                         ""  )

    -- If string inside parens starts in with apostrophe or quotation
    -- (possibly preceded by whitespaces), then "url" should be interpreted
    -- as function.
  , ( "url(\'a\')",                   CssTokFunc "url",                      "\'a\')"   )
  , ( "url( \'a\')",                  CssTokFunc "url",                      " \'a\')"  )
  , ( "url(  \'a\')",                 CssTokFunc "url",                      " \'a\')"  )
  , ( "url(   \'a\')",                CssTokFunc "url",                      " \'a\')"  )

  , ( "url(\"a\")",                   CssTokFunc "url",                      "\"a\")"   )
  , ( "url( \"a\")",                  CssTokFunc "url",                      " \"a\")"  )
  , ( "url(  \"a\")",                 CssTokFunc "url",                      " \"a\")"  )
  , ( "url(   \"a\")",                CssTokFunc "url",                      " \"a\")"  )

    -- Test that function name not followed by opening paren is not
    -- recognized as <function-token> or <url-token>. "rgb" is a function
    -- supported by this implementation.
  , ( "rgb{100, 100, 100}",             CssTokIdent "rgb",                   "{100, 100, 100}"    )
  , ( "url{a}",                         CssTokIdent "url",                   "{a}"                )

    -- Opening paren is being consumed, but is neither part of list of
    -- tokens, nor of function name.
  , ( "rgb(100, 100, 100)",             CssTokFunc "rgb",                    "100, 100, 100)"     )

    -- Functions that aren't supported by this implementation (yet), but
    -- still the functions must be recognized as such.
  , ( "rgba(100, 100, 100, 50)",        CssTokFunc "rgba",                   "100, 100, 100, 50)" )
  , ( "linear-gradient(#600, #933);",   CssTokFunc "linear-gradient",        "#600, #933);"       )


    -- Space between ident and paren makes the input a non-function.
  , ( "rgb (100, 100, 100)",            CssTokIdent "rgb",                   " (100, 100, 100)"   )
  ]




-- On success return empty string. On failure return string representation of
-- testcase that failed.
tokenizerTestRunner :: Bool -> [(T.Text, CssToken, T.Text)] -> T.Text
tokenizerTestRunner _ []       = ""
tokenizerTestRunner inB (x:xs) = if expectedToken /= t2 || expectedRemainder /= (remainder p2)
                                 then T.pack . showFailedCase $ x
                                 else tokenizerTestRunner inB xs
  where
    initialRemainder  = triplet1st x
    expectedToken     = triplet2nd x
    expectedRemainder = triplet3rd x
    (p2, t2)          = if inB
                        then nextToken . defaultParserInBlock $ initialRemainder
                        else nextToken . defaultParser $ initialRemainder

    showFailedCase _ =    "Initial remainder = "  ++ (show initialRemainder) ++ "; "
                       ++ "Expected remainder = " ++ (show expectedRemainder) ++ "; "
                       ++ "Expected token = "     ++ (show expectedToken) ++ "; "
                       ++ "Output remainder = "   ++ (show . remainder $ p2) ++ "; "
                       ++ "Output token = "       ++ (show t2)




-- List of token types from https://www.w3.org/TR/css-syntax-3/#tokenization
-- that aren't tested below in tokenizerTestCases yet:
--
-- <at-keyword-token>, <string-token>, <bad-string-token>,
-- <bad-url-token>, <delim-token>, <whitespace-token>,
-- <CDO-token>, <CDC-token>




tokenizerTestCases :: [Test]
tokenizerTestCases = [
  -- If some error is found, tokenizerTestRunner returns non-empty string
  -- which can help in finding out which test failed.

     -- <number-token>, <percentage-token>, <dimension-token>
     TestCase (do assertEqual "manual tests of numeric tokens"           "" (tokenizerTestRunner False numericTokenTestManualData))

     -- <hash-token>
   , TestCase (do assertEqual "manual tests of hash token"               "" (tokenizerTestRunner True hashTokenTestManualData))

     -- <ident-token>
   , TestCase (do assertEqual "manual tests of ident token"              "" (tokenizerTestRunner False identTokenTestManualData))

     -- <function-token>
     -- <url-token>
   , TestCase (do assertEqual "manual tests of function and url token"   "" (tokenizerTestRunner False functionUrlTokenTestManualData))

     -- <colon-token>
     -- <semicolon-token>
     -- <comma-token>
     -- <[-token>
     -- <]-token>
     -- <(-token>
     -- <)-token>
     -- <{-token>
     -- <}-token>
   , TestCase (do assertEqual "manual tests of single-char token"        "" (tokenizerTestRunner False singleCharTokenTestManualData))
  ]




testsCssTokenizer :: IO String
testsCssTokenizer = do
  testCounts <- runTestTT (TestList (tokenizerTestCases))
  if (errors testCounts + failures testCounts == 0)
    then return ""
    else return "[EE] testsCssTokenizer failed"

