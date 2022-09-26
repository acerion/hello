{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}




module Hello.Tests.Gif
  (
    testsGif
  )
where




import Test.HUnit
import Test.QuickCheck
-- import Test.QuickCheck.Instances.Text -- We will write our own instance
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.String (fromString)

import Gif
import Hello.Tests.Utils.Gifted
import Hello.Tests.Utils.QuickCheck




myGetChar :: Gen Char
myGetChar = elements ['a'..'z']



myArbitraryText :: Gen T.Text
myArbitraryText = fmap fromString (listOf myGetChar)
--myArbitraryText = fromString <$> (listOf myGetChar)


instance Arbitrary T.Text where
    arbitrary = myArbitraryText



parseExtensionComment = [
  -- These three tests succeed because tested function ignores anything shorter than 3 bytes.
    TestCase(assertEqual "comment: empty stream"
             (Just gifDefault)
             (parseExtension gifDefault (BS.pack [])))
  , TestCase(assertEqual "comment: only unexpected byte"
             (Just gifDefault)
             (parseExtension gifDefault (BS.pack [0xbe])))
  , TestCase(assertEqual "comment: only ext intro"
             (Just gifDefault)
             (parseExtension gifDefault (BS.pack [0x21])))

  -- Malformed bytes.
  , TestCase(assertEqual "comment: 3 unexpected bytes"
             Nothing
             (parseExtension gifDefault (BS.pack [0xbe, 0x01, 0x02])))
  , TestCase(assertEqual "comment: 1 good, 2 bad"
             Nothing
             (parseExtension gifDefault (BS.pack [0x21, 0x01, 0x02])))

  -- The shortest possible comment: empty comment. 3 valid bytes should be
  -- consumed. This may be not useful comment, but it appears to be a
  -- well-formed extension.
  , TestCase(assertEqual "comment: empty comment"
             (Just (gifForward gifDefault 3))
             (parseExtension gifDefault (BS.pack [0x21, 0xfe, 0x00])))

  -- The extension array that promises a comment sub-block of size 5, but
  -- fails to deliver those 5 bytes. Since function can't parse full
  -- extension, it consumes zero bytes from the array.
  , TestCase(assertEqual "comment: incomplete comment"
             (Just gifDefault)
             (parseExtension gifDefault (BS.pack [0x21, 0xfe, 0x05])))

  -- This byte stream can be interpreted in several ways:
  --
  -- A:
  -- This is a valid, but incomplete byte stream with two sub-blocks:
  -- first:  size = 0x01, data = [0x65]
  -- second: size = 0x66, data = [starting with 0x00, but missing the rest of bytes]
  --
  -- B:
  -- This is a complete and invalid byte stream, with single letter (0x65)
  -- that should have been followed by Block Terminator, but is instead
  -- followed by invalid byte 0x66.
  --
  -- Nothing in the spec says that the first sub-block must have 0xff bytes,
  -- it looks like smaller sub-blocks are allowed, so I choose the first
  -- interpretation (at least for now). TODO: Manually create a gif image
  -- that has such Comment Extension in it, and see how the parser behaves.
  , TestCase(assertEqual "comment: strange subblock size"
             (Just gifDefault)
             (parseExtension gifDefault (BS.pack [0x21, 0xfe, 0x01, 0x65, 0x66, 0x00])))


  -- Generate Comment Extension from given text, parse it and verify result
  -- of parsing. This is a "manual" test, with manually generated
  -- text. Elsewhere I'm using buildExtensionComment with QuickCheck.
  , TestCase (do
                 let text = ""
                 let extension = buildExtensionComment text
                 case parseExtension gifDefault extension of
                   --Just gif -> assertEqual "empty (just)" "E" (comment gif) -- Uncomment this to trigger failure
                   Just gif -> assertEqual "comment: empty test (just)" text (comment gif)
                   Nothing  -> assertEqual "comment: empty test (nothing)" True False)
  , TestCase (do
                 let text = "a"
                 let extension = buildExtensionComment text
                 case parseExtension gifDefault extension of
                   --Just gif -> assertEqual "single-letter test (just)" (T.toUpper text) (comment gif) -- Uncomment this to trigger failure
                   Just gif -> assertEqual "comment: single-letter test (just)" text (comment gif)
                   Nothing  -> assertEqual "comment: single-letter test (nothing)" True False)
  , TestCase (do
                 let text = "Monday Tuesday Wednesday"
                 let extension = buildExtensionComment text
                 case parseExtension gifDefault extension of
                   --Just gif -> assertEqual "single-letter test (just)" (T.toUpper text) (comment gif) -- Uncomment this to trigger failure
                   Just gif -> assertEqual "comment: string test (just)" text (comment gif)
                   Nothing  -> assertEqual "comment: string test (nothing)" True False)
  ]




parseCommentExtension text =
  case parseExtension gifDefault extension of
    Just gif -> text == comment gif && BS.length extension == consumed gif
    Nothing  -> False
    where
      extension = buildExtensionComment text



testsGif :: IO String
testsGif = do
  -- All I had to do to find stdArgs and xWith is to read documentation :)
  -- http://hackage.haskell.org/package/QuickCheck-2.8/docs/Test-QuickCheck.html
  let testArgs = stdArgs { maxSuccess = 500, maxSize = 400 }
  result <- quickCheckWithResult testArgs parseCommentExtension
  -- result <- verboseCheckWithResult testArgs parseCommentExtension
  let failures1 = if qcResultIsSuccess result
                  then ""
                  else "[EE] testsGif part 1 failed "

  testCounts <- runTestTT (TestList (parseExtensionComment))
  let failures2 = if errors testCounts + failures testCounts == 0
                  then ""
                  else "[EE] testsGif part 2 failed"

  return $ failures1 ++ failures2
