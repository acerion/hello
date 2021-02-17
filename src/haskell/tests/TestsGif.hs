module TestsGif (testsGif) where

import Test.HUnit
import System.Exit
import qualified Data.Text as T
import qualified Data.ByteString as BS

import GIF


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
  ]



testsGif :: IO ()
testsGif = do
  counts <- runTestTT (TestList (parseExtensionComment))
  if (errors counts + failures counts == 0)
    then exitSuccess
    else exitFailure
