module TestsHtmlEntity (testsHtmlEntity
                ) where

import Test.HUnit
import System.Exit
import HtmlEntity
import qualified Data.Text as T




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
  ]




testsHtmlEntity :: IO ()
testsHtmlEntity = do
  counts <- runTestTT (TestList (testCases))
  if (errors counts + failures counts == 0)
    then exitSuccess
    else exitFailure

