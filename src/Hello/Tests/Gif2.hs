{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}




module Hello.Tests.Gif2
  (
    testsGif2
  )
where




import Test.QuickCheck
import Test.QuickCheck.Instances.Text()

import Hello.Gif
import Hello.Tests.Utils.Gifted
import Hello.Tests.Utils.QuickCheck




{-
This file is using default instance of Arbitrary for Text from
Test.QuickCheck.  The instance is able to generate any string, and sometimes
my code doesn't handle it well.  Thanks to usage of T.E.decodeUtf8With in GIF
parser code I'm avoiding an exception, but I still get this error:

*** Failed! Falsifiable (after 6 tests and 1 shrink):
"\143915"

This failure "only" indicates mismatch between input and output of test
(probably when input is invalid in some way). It doesn't necessarily mean
that there is something wrong with the parser.

TODO: perhaps this indicates that I should not be using Data.Text for
comments. The GIF standard recommends only ASCII characters in the Comment
Extension, so maybe Data.Text is an overkill. But I still need to somehow
handle invalid byte sequences in incoming Extensions.
-}




prop_parseCommentExtension text =
  case parseExtension gifDefault extension of
    Just gif -> text == comment gif
    Nothing  -> False
    where
      extension = buildExtensionComment text



testsGif2 :: IO String
testsGif2 = do
  -- All I had to do to find stdArgs and xWith is to read documentation :)
  -- http://hackage.haskell.org/package/QuickCheck-2.8/docs/Test-QuickCheck.html
  let testArgs = stdArgs { maxSuccess = 400, maxSize = 300 }
  result <- quickCheckWithResult testArgs prop_parseCommentExtension
  -- result <- verboseCheckWithResult testArgs prop_parseCommentExtension

  if qcResultIsSuccess result
    then return ""
    else return "[EE] testsGif2 failed"
