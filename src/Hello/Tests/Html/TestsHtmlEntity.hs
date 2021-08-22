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

import HtmlEntity




validEntityData =
  --  token                          code              remainder
  [
  -- Correct values of entity names.
    ("&amp;",                        Just 0x0026,      "")
  , ("&lt;",                         Just 0x003C,      "")
  , ("&diams;",                      Just 0x2666,      "")

  -- Correct values of entity names, but without terminating semicolon.
  , ("&nbsp",                        Just 0x00A0,      "")
  , ("&cent",                        Just 0x00A2,      "")
  , ("&sup3",                        Just 0x00B3,      "")

  -- Correct values of entity names, with some text after them. The
  -- additional text should be put into 'remainder' field of parser.
  , ("&Agrave; one",                 Just 0x00C0,      " one")
  , ("&clubs;t w o",                 Just 0x2663,      "t w o")
  , ("&sum;three ",                  Just 0x2211,      "three ")

  -- Correct decimal values. With ending semicolon.
  , ("&#1234;",                      Just 1234,        "")
  , ("&#3212;hello",                 Just 3212,        "hello")
  , ("&#999; world",                 Just 999,         " world")

  -- Correct decimal values. Without ending semicolon.
  , ("&#7123",                       Just 7123,        "")
  , ("&#356 day",                    Just 356,         " day")
  , ("&#83 night ",                  Just 83,          " night ")

    -- Correct hex values. With ending semicolon.
  , ("&#xa34e;",                     Just 0xa34e,      "")
  , ("&#xff123;never",               Just 0xff123,     "never")
  , ("&#x995;gonna ",                Just 0x995,       "gonna ")
  , ("&#x123ff; let ",               Just 0x123ff,     " let ")

  -- Correct hex values. Without ending semicolon.
  , ("&#x7",                         Just 0x7,         "")
  , ("&#x7d left",                   Just 0x7d,        " left")
  , ("&#x7d1 right ",                Just 0x7d1,       " right ")
  ]




-- Well-formed, but unsupported values of entity names. They should be
-- consumed/skipped, and and whatever comes after name should be saved as
-- remainder.
wellFormedEntityData =
  --  token                          code              remainder
  [
    ("&monday; semispace",           Nothing,          " semispace")   -- Semicolon and space used as separator.
  , ("&tuesday;nospace",             Nothing,          "nospace")      -- Only semicolon used to separate name from following text.
  , ("&wednesday justspace ",        Nothing,          " justspace ")  -- Only space used to separeate name from following text.
  ]




-- Verify that we are reaching the special case for w1252 codes.
w1252EntityData =
  --  token                          code              remainder
  [
  -- Values before special range.
    ("&#141; 241",                   Just 141,         " 241")
  , ("&#142;242",                    Just 142,         "242")
  , ("&#143;243",                    Just 143,         "243")
  , ("&#144; 244 ",                  Just 144,         " 244 ")

  -- Special range.
  , ("&#145; 245",                   Just 0x27,        " 245")
  , ("&#146;246",                    Just 0x27,        "246")
  , ("&#147; 247 ",                  Just 0x22,        " 247 ")
  , ("&#148;248 ",                   Just 0x22,        "248 ")
  , ("&#149; 249",                   Just 0xb0,        " 249")
  , ("&#150;",                       Just 0x2d,        "")
  , ("&#151; 251",                   Just 0x2d,        " 251")

  -- Values after special range.
  , ("&#152; 252",                   Just 152,         " 252")
  , ("&#153; 253",                   Just 153,         " 253")
  , ("&#154; 254",                   Just 154,         " 254")
  ]




invalidEntityData =
  --  token                          code              remainder
  [
  -- Decimal values, but without leading '#'.
    ("&1234;",                       Nothing,          "")
  , ("&3212;hello",                  Nothing,          "hello")
  , ("&999; world",                  Nothing,          " world")
  , ("&999; ! ",                     Nothing,          " ! ")

  -- Inorrect hex values.
  , ("&xa34e;",                      Nothing,          "")             -- No leading '#'
  , ("&#xff123later",                Just 0xff123,     "later")        -- Kind-of incorrect, reader should read initial part as valid hex.
  ]




validEntityTest :: [(T.Text, Maybe Int, T.Text)] -> T.Text
validEntityTest []     = ""
validEntityTest (x:xs) = if isMatch x (htmlEntityToIsoCode . inName $ x)
                         then validEntityTest xs
                         else (inName x)
  where
    isMatch :: (T.Text, Maybe Int, T.Text) -> Maybe EntityParser -> Bool
    isMatch x parser = case parser of
                         Just parser' -> inCode x == entityIsoCode parser'
                                         && (inRemainder x) == remainder parser'
                         otherwise    -> False

    inName      (x, _, _) = x
    inCode      (_, y, _) = y
    inRemainder (_, _, z) = z




testCases = [
    TestCase(assertEqual "valid entity tests"           "" (validEntityTest validEntityData))
  , TestCase(assertEqual "well-formed entity tests"     "" (validEntityTest wellFormedEntityData))
  , TestCase(assertEqual "invalid entity tests"         "" (validEntityTest invalidEntityData))
  , TestCase(assertEqual "w1252 entity tests"           "" (validEntityTest w1252EntityData))
  ]




testsHtmlEntity :: IO String
testsHtmlEntity = do
  counts <- runTestTT (TestList (testCases))
  if (errors counts + failures counts == 0)
    then return ""
    else return "[EE] testsHtmlEntity failed"

