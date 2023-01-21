{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}




module Hello.Tests.Html.Entity
  (
    testsHtmlEntity
  )
where




import Test.HUnit
import qualified Data.Text as T

import Hello.Html.Entity




validEntityData :: [(T.Text, Maybe (T.Text, HtmlEntity))]
validEntityData =
  --  input                          Maybe (remainder, html entity)
  [
  -- Correct values of entity names.
    ("&amp;",                        Just ("", HtmlEntity 0x0026))
  , ("&lt;",                         Just ("", HtmlEntity 0x003C))
  , ("&diams;",                      Just ("", HtmlEntity 0x2666))

  -- Correct values of entity names, but without terminating semicolon.
  , ("&nbsp",                        Just ("", HtmlEntity 0x00A0))
  , ("&cent",                        Just ("", HtmlEntity 0x00A2))
  , ("&sup3",                        Just ("", HtmlEntity 0x00B3))

  -- Correct values of entity names, with some text after them. The
  -- additional text should be put into 'remainder' field of parser.
  , ("&Agrave; one",                 Just (" one",   HtmlEntity 0x00C0))
  , ("&clubs;t w o",                 Just ("t w o",  HtmlEntity 0x2663))
  , ("&sum;three ",                  Just ("three ", HtmlEntity 0x2211))

  -- Correct decimal values. With ending semicolon.
  , ("&#1234;",                      Just ("",       HtmlEntity 1234))
  , ("&#3212;hello",                 Just ("hello",  HtmlEntity 3212))
  , ("&#999; world",                 Just (" world", HtmlEntity 999))

  -- Correct decimal values. Without ending semicolon.
  , ("&#7123",                       Just ("",        HtmlEntity 7123))
  , ("&#356 day",                    Just (" day",    HtmlEntity 356))
  , ("&#83 night ",                  Just (" night ", HtmlEntity 83))

    -- Correct hex values. With ending semicolon.
  , ("&#xa34e;",                     Just ("",       HtmlEntity 0xa34e))
  , ("&#xff123;never",               Just ("never",  HtmlEntity 0xff123))
  , ("&#x995;gonna ",                Just ("gonna ", HtmlEntity 0x995))
  , ("&#x123ff; let ",               Just (" let ",  HtmlEntity 0x123ff))

  -- Correct hex values. Without ending semicolon.
  , ("&#x7",                         Just ("",        HtmlEntity 0x7))
  , ("&#x7d left",                   Just (" left",   HtmlEntity 0x7d))
  , ("&#x7d1 right ",                Just (" right ", HtmlEntity 0x7d1))
  ]




-- Well-formed, but unsupported values of entity names. They should be
-- consumed/skipped, and and whatever comes after name should be saved as
-- remainder.
wellFormedEntityData :: [(T.Text, Maybe (T.Text, HtmlEntity))]
wellFormedEntityData =
  --  token                          Maybe (remainder, html entity)
  [
    ("&monday; semispace",           Nothing)  -- Semicolon and space used as separator.
  , ("&tuesday;nospace",             Nothing)  -- Only semicolon used to separate name from following text.
  , ("&wednesday justspace ",        Nothing)  -- Only space used to separeate name from following text.
  ]




-- Verify that we are reaching the special case for w1252 codes.
w1252EntityData :: [(T.Text, Maybe (T.Text, HtmlEntity))]
w1252EntityData =
  --  token                          Maybe (remainder, html entity)
  [
  -- Values before special range.
    ("&#141; 241",                   Just (" 241",  HtmlEntity 141))
  , ("&#142;242",                    Just ("242",   HtmlEntity 142))
  , ("&#143;243",                    Just ("243",   HtmlEntity 143))
  , ("&#144; 244 ",                  Just (" 244 ", HtmlEntity 144))

  -- Special range.
  , ("&#145; 245",                   Just (" 245",  HtmlEntity 0x27))
  , ("&#146;246",                    Just ("246",   HtmlEntity 0x27))
  , ("&#147; 247 ",                  Just (" 247 ", HtmlEntity 0x22))
  , ("&#148;248 ",                   Just ("248 ",  HtmlEntity 0x22))
  , ("&#149; 249",                   Just (" 249",  HtmlEntity 0xb0))
  , ("&#150;",                       Just ("",      HtmlEntity 0x2d))
  , ("&#151; 251",                   Just (" 251",  HtmlEntity 0x2d))

  -- Values after special range.
  , ("&#152; 252",                   Just (" 252", HtmlEntity 152))
  , ("&#153; 253",                   Just (" 253", HtmlEntity 153))
  , ("&#154; 254",                   Just (" 254", HtmlEntity 154))
  ]




invalidEntityData :: [(T.Text, Maybe (T.Text, HtmlEntity))]
invalidEntityData =
  --  token                          Maybe (remainder, html entity)
  [
  -- Decimal values, but without leading '#'.
    ("&1234;",                       Nothing)
  , ("&3212;hello",                  Nothing)
  , ("&999; world",                  Nothing)
  , ("&999; ! ",                     Nothing)

  -- Inorrect hex values.
  , ("&xa34e;",                      Nothing) -- No leading '#'
  , ("&#xff123later",                Nothing)
  , ("$#x0x123",                     Nothing) -- Incorrect "0x" prefix in hex number.
  ]




validEntityTest :: [(T.Text, Maybe (T.Text, HtmlEntity))] -> T.Text
validEntityTest []     = ""
validEntityTest (x:xs) = if isMatch x (htmlEntityToIsoCode . fst $ x)
                         then validEntityTest xs
                         else fst x
  where
    isMatch :: (T.Text, Maybe (T.Text, HtmlEntity)) -> Maybe (T.Text, HtmlEntity) -> Bool
    isMatch expected result = result == snd expected




testCases :: [Test]
testCases = [
    TestCase(assertEqual "valid entity tests"           "" (validEntityTest validEntityData))
  , TestCase(assertEqual "well-formed entity tests"     "" (validEntityTest wellFormedEntityData))
  , TestCase(assertEqual "invalid entity tests"         "" (validEntityTest invalidEntityData))
  , TestCase(assertEqual "w1252 entity tests"           "" (validEntityTest w1252EntityData))
  ]




testsHtmlEntity :: IO String
testsHtmlEntity = do
  testCounts <- runTestTT (TestList testCases)
  if errors testCounts + failures testCounts == 0
    then return ""
    else return "[EE] Tests.Html.Entity failed"

