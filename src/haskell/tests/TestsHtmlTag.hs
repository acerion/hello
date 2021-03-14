module TestsHtmlTag (testsHtmlTag
                ) where

import Test.HUnit
import System.Exit
import HtmlTag
import qualified Data.Text as T




testAttributeValueData =
  -- test id   full tag to parse              attr name     expected attr value         code              remainder
  [
    ( "010", "<a name1=\"value1\">",              "name1", Just "value1" ) -- " as value delimiter
  , ( "020", "<a name1='value1'>",                "name1", Just "value1" ) -- ' as value delimiter
  , ( "030", "<a name1=value1>",                  "name1", Just "value1" ) -- " as value delimiter
  , ( "040", "<a name1=value1 name2=value2>",     "name2", Just "value2" ) -- neither ' nor " as value delimiter

  , ( "050", "<a name1 = \"value1\" >",           "name1", Just "value1" ) -- " as value delimiter, with more spaces
  , ( "060", "<a name1 = 'value1' >",             "name1", Just "value1" ) -- ' as value delimiter, with more spaces
  , ( "070", "<a name1= value1>",                 "name1", Just "value1" ) -- " as value delimiter, with more spaces
  , ( "080", "<a name1= value1 name2 = value2>",  "name2", Just "value2" ) -- space as value delimiter, with more spaces
  , ( "090", "<a name1 = value1 name2 = value2>", "name2", Just "value2" ) -- space as value delimiter, with more spaces
  , ( "100", "<a name1= value1 name2= value2>",   "name2", Just "value2" ) -- space as value delimiter, with more spaces
  , ( "110", "<a name1 = value1 name2 =value2>",  "name2", Just "value2" ) -- space as value delimiter, with more spaces
  , ( "120", "<a name1= value1 name2= value2 >",  "name2", Just "value2" ) -- space as value delimiter, with more spaces
  , ( "130", "<a name1 = value1 name2 =value2 >", "name2", Just "value2" ) -- space as value delimiter, with more spaces

  , ( "140", "<a name1=\" value1 \">",            "name1", Just "value1" ) -- Spaces in value are stripped
  , ( "150", "<a name1=' value1 '>",              "name1", Just "value1" ) -- Spaces in value are stripped

  , ( "160", "<a name1=\"\"",                     "name1", Just "" ) -- empty value, " as value delimiter
  , ( "170", "<a name1=''",                       "name1", Just "" ) -- empty value, ' as value delimiter
  , ( "180", "<a name1=\"\" name2=\"value2\">",   "name1", Just "" ) -- empty value, " as value delimiter
  , ( "190", "<a name1='' name2=\"value2\">",     "name1", Just "" ) -- empty value, ' as value delimiter

  , ( "200", "<a name1= name2=\"value2\">",       "name1", Just "name2=\"value2\"" ) -- no ' or " delimiters around empty value, remainder returned as value
  , ( "210", "<a name1= name2=\"value2\">",       "name1", Just "name2=\"value2\"" ) -- no ' or " delimiters around empty value, remainder returned as value
  , ( "220", "<a name1= name2 =\"value2\">",      "name1", Just "name2" ) -- no ' or " delimiters around value, next space-separated token returned as value
  , ( "230", "<a name1= name2 =\"value2\">",      "name1", Just "name2" ) -- no ' or " delimiters around value, next space-separated token returned as value

  -- Inconsistent value delimiter leads to returning remainder in search of
  -- matching opening delimiter.
  , ( "240", "<a name1='value1\" name2='value2'>",   "name1", Just "value1\" name2=" )
  , ( "250", "<a name1=\"value1' name2=\"value2\">", "name1", Just "value1' name2=" )

  -- Missing separator between name and previous value is handled
  -- correctly.
  , ( "260", "<a name1=\"value1\" name2='value2'name3='value3'>",   "name3", Just "value3" )
  , ( "270", "<a name1=\"value1\" name2=\"value2\"name3='value3'>", "name3", Just "value3" )

  -- Newline placed between tokens doesn't break search algorithm.
  , ( "280", "<a\n name1\n=\"value1\n\" \n name2\n=\n\"\nvalue2\n\"\nname3='value3'>", "name1", Just "value1" )
  , ( "290", "<a \nname1\n=\"value1\n\" \n name2\n=\n\"\nvalue2\n\"\nname3='value3'>", "name1", Just "value1" )
  , ( "300", "<a\nname1\n=\"value1\n\" \n name2\n=\n\"\nvalue2\n\"\nname3='value3'>",  "name1", Just "value1" )
  , ( "310", "<a\n name1\n=\"value1\n\" \n name2\n=\n\"\nvalue2\n\"\nname3='value3'>", "name2", Just "value2" )
  , ( "320", "<a \nname1\n=\"value1\n\" \n name2\n=\n\"\nvalue2\n\"\nname3='value3'>", "name2", Just "value2" )
  , ( "330", "<a\nname1\n=\"value1\n\" \n name2\n=\n\"\nvalue2\n\"\nname3='value3'>",  "name2", Just "value2" )
  , ( "340", "<a\n name1\n=\"value1\n\" \n name2\n=\n\"\nvalue2\n\"\nname3='value3'>", "name3", Just "value3" )
  , ( "350", "<a \nname1\n=\"value1\n\" \n name2\n=\n\"\nvalue2\n\"\nname3='value3'>", "name3", Just "value3" )
  , ( "360", "<a\nname1\n=\"value1\n\" \n name2\n=\n\"\nvalue2\n\"\nname3='value3'>",  "name3", Just "value3" )

  -- TODO: enable these tests
  --, ( "370", "<a name1=\"value1\" \n name2=\"value2\"name3='val\nue3'>", "name3", Just "value3" ) -- Newline inside of value is recognized and removed
  --, ( "380", "<a name1=\"value1\" \n na\nme2=\"value2\"name3='val\tue3'>", "name2", NULL ) -- Newline inside of name is recognized and removed
  , ( "390", "<a name1=\"value1\" \n name2=\"value2\"name3='val\tue3'>", "name3", Just "val ue3" ) -- TAB inside of value is replaced with space

  -- Properly built tags. Ensure that we can get all values, one after
  -- another. This is a protection against a function that would think "I
  -- don't know what I'm doing so I will always return a first or a last tag,
  -- just to make it easy for myself".
  , ( "400", "<a name1=\"value1\" name2=\"value2\" name3=\"value3\">",       "name1", Just "value1" )
  , ( "410", "<a name1=\"value1\" name2=\"value2\" name3=\"value3\">",       "name2", Just "value2" )
  , ( "420", "<a name1=\"value1\" name2=\"value2\" name3=\"value3\">",       "name3", Just "value3" )
  , ( "430", "<a name1=\"value1\" name2=\"value2\" name3=\"value3\">",       "name4", Nothing )

  , ( "440", "<a name1= name2=\"value2\">",       "name1", Just "name2=\"value2\"" ) -- no ' or " delimiters around empty value, remainder returned as value
  , ( "450", "<a name1= name2 =\"value2\">",      "name1", Just "name2" ) -- no ' or " delimiters around value, next space-separated token returned as value
  , ( "460", "<a name1= name2 =\"value2\">",      "name1", Just "name2" ) -- no ' or " delimiters around value, next space-separated token returned as value

  -- Attribute name does not exit.
  , ( "470", "<a name1=\"value1\">", "name",   Nothing )
  , ( "480", "<a name1=\"value1\">", "name11", Nothing )
  , ( "490", "<a name=\"value1\">",  "name1",  Nothing )

  -- Some real-world scenarios. Adding '/' to a predicate led to returning
  -- attribute value with just a leading dot.
  , ( "500", "<a src=\"./path/to/some_img.png\">",       "src", Just "./path/to/some_img.png" )
  , ( "510", "<a src='./path/to/some_img.png'>",         "src", Just "./path/to/some_img.png" )
  , ( "520", "<a src=./path/to/some_img.png>",           "src", Just "./path/to/some_img.png" )
  , ( "530", "<a src=\"./path/to/some_img.png\" >",      "src", Just "./path/to/some_img.png" )
  -- '/' appears as part of value, and as part of remainder. I know that <a>
  -- tag should not have '/' at the end, but I can't come up right now with
  -- other tag that would be useful as example here.
  --
  -- TODO: current implementation of parser ("stateless parser") can't
  -- recognize when a character is inside of value, or when it's outside of
  -- value.
  , ( "540", "<a src=\"./path/to/some_img.png\"/>",      "src", Just "./path/to/some_img.png" )
  , ( "550", "<a src=\"./path/to/some_img.png\" />",     "src", Just "./path/to/some_img.png" )
  , ( "560", "<a src=\"./path/to/some_img.png\" / >",    "src", Just "./path/to/some_img.png" )

  ]




testAttributeValueFun :: [(T.Text, T.Text, T.Text, Maybe T.Text)] -> T.Text
testAttributeValueFun []     = ""
testAttributeValueFun (x:xs) = if (expectedValue x) == (htmlTagGetAttributeValue (tag x) (attributeName x))
                               then testAttributeValueFun xs
                               else testID x -- Return ID of failed test
  where
    testID        (a, _, _, _) = a
    tag           (_, b, _, _) = b
    attributeName (_, _, c, _) = c
    expectedValue (_, _, _, d) = d




testCases = [
    TestCase(assertEqual "'get attribute value' tests" "" (testAttributeValueFun testAttributeValueData))
  ]




testsHtmlTag :: IO ()
testsHtmlTag = do
  counts <- runTestTT (TestList (testCases))
  if (errors counts + failures counts == 0)
    then exitSuccess
    else exitFailure

