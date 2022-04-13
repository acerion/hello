{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}




module Hello.Tests.Html.Doctype
  (
    testsHtmlDoctype
  )
where




import Debug.Trace
import Test.HUnit
import qualified Data.Text as T

import Hello.Html.Doctype




sanitizationData =
  [
    ( "hello", "hello" )

  , ( "he llo",      "he llo" ) -- Collapse single space (we don't actually collapse single space)
  , ( "he  llo",     "he llo" ) -- Collapse two spaces into one
  , ( "he   llo",    "he llo" ) -- Collapse three spaces into one

    -- Don't collapse spaces in quotes
  , ( "he 'a a' llo",        "he 'a a' llo" )
  , ( "he 'a   a' llo",      "he 'a   a' llo" )
  , ( "he \"a a\" llo",      "he \"a a\" llo" )
  , ( "he \"a   a\" llo",    "he \"a   a\" llo" )


  -- Replace \r and \n with space inside of quotes
  , ( "he \"l \r  l\" o",    "he \"l    l\" o" )
  , ( "he \"l \n  l\" o",    "he \"l    l\" o" )

  -- Replace multiple \r and \n with space inside of quotes. Don't collapse
  -- spaces in quotes.
  , ( "he \"l \r \n\r  l\" o",    "he \"l       l\" o" )
  , ( "he \"l \n\r \n  l\" o",    "he \"l       l\" o" )

  -- Replace \r and \n with spaces outside of quotes, and also collapse them.
  , ( "he l \r l o",         "he l l o" )
  , ( "he l \n l o",         "he l l o" )

  -- Replace multiple \r and \n with spaces outside of quotes, and also
  -- collapse them.
  , ( "he l \r \n\r l o",         "he l l o" )
  , ( "he l \n\r \n l o",         "he l l o" )
  ]




sanitizationTest :: [(T.Text, T.Text)] -> T.Text
sanitizationTest []     = ""
sanitizationTest (x:xs) = if expected x == sanitizeDoctypeString (toSanitize x)
                          then sanitizationTest xs
                          else (toSanitize x)
  where
    toSanitize = fst
    expected   = snd




{- -------------------------------------------------------------------------- -}




-- TODO: add tests with unrecognizable input strings
-- TODO: add tests with white spaces embedded in input strings
getDoctypeData =
  [
    -- HTML 2.0 - PASS
    ( "<!DOCTYPE HTML PUBLIC -//IETF//DTD HTML",                                          HtmlDoctypeHtml 2.0  )

    -- HTML 3.2 - PASS
  , ( "<!DOCTYPE HTML PUBLIC -//W3C//DTD HTML 3.2",                                       HtmlDoctypeHtml 3.2  )

    -- HTML 4.0 - PASS
  , ( "<!DOCTYPE HTML PUBLIC -//W3C//DTD HTML 4.0",                                       HtmlDoctypeHtml 4.0  )

    -- HTML 4.01 - PASS
  , ( "<!DOCTYPE HTML PUBLIC -//W3C//DTD HTML 4.01 http://www.w3.org/TR/html4/",          HtmlDoctypeHtml 4.01 )

    -- HTML 5 - PASS
  , ( "<!DOCTYPE html>",                                                                  HtmlDoctypeHtml 5.0  )
  , ( "<!DOCTYPE html >",                                                                 HtmlDoctypeHtml 5.0  )
  , ( "<!DOCTYPE html SYSTEM \"about:legacy-compat\">",                                   HtmlDoctypeHtml 5.0  )
  , ( "<!DOCTYPE html SYSTEM 'about:legacy-compat'>",                                     HtmlDoctypeHtml 5.0  )

    -- XHTML 1.0 - PASS
  , ( "<!DOCTYPE HTML PUBLIC -//W3C//DTD XHTML 1.0 http://www.w3.org/TR/xhtml1/DTD/",     HtmlDoctypeXhtml 1.0  )

    -- XHTML 1.1 - PASS
  , ( "<!DOCTYPE HTML PUBLIC -//W3C//DTD XHTML 1.1 http://www.w3.org/TR/xhtml11/DTD/",    HtmlDoctypeXhtml 1.1  )
  ]




getDoctypeTest :: [(T.Text, HtmlDoctype)] -> T.Text
getDoctypeTest []     = ""
getDoctypeTest (x:xs) = if expected x == getDoctypeFromBuffer (input x) emptyDoctype
                        then getDoctypeTest xs
                        else trace (show (getDoctypeFromBuffer (input x) emptyDoctype)) (input x)
  where
    input        = fst
    expected     = snd
    emptyDoctype = HtmlDoctypeNone




{- -------------------------------------------------------------------------- -}




testCases = [
    TestCase(assertEqual "sanitization of doctype"                   "" (sanitizationTest sanitizationData))
  , TestCase(assertEqual "top-level function getting doctype"        "" (getDoctypeTest getDoctypeData))
  ]




testsHtmlDoctype :: IO String
testsHtmlDoctype = do
  counts <- runTestTT (TestList (testCases))
  if (errors counts + failures counts == 0)
    then return ""
    else return "[EE] Hello.Tests.Html.Doctype failed"


