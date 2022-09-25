{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}




module Hello.Tests.Chain
  (
    testsChain
  )
where



import qualified Data.Text as T

--import Debug.Trace

import Test.HUnit

import Hello.Chain
import Hello.Utils




type DatumType = T.Text
type LinkType = Char




{- -------------------------------------------------------------------------- -}




-- On success return empty string. On failure return string showing
-- approximately where the problem is.
datumLengthTestFunction :: [(Chain DatumType LinkType, Int)] -> T.Text
datumLengthTestFunction []     = ""
datumLengthTestFunction (x:xs) = if not success
                            then T.pack ("Got: " ++ show output ++ ", Expected: " ++ (show expectedOutput) ++ "; input = " ++ (show input))
                            else datumLengthTestFunction xs
  where
    success = output == expectedOutput
    output = chainDatumLength input

    input          = fst x
    expectedOutput = snd x




datumLengthTestData =
  [
    (Last  "sel1",                                                          1)
  , (Chain "sel1" '+' (Last  "sel2"),                                       2)
  , (Chain "sel1" '+' (Chain "sel2" '>' (Last  "sel3")),                    3)
  , (Chain "sel1" '+' (Chain "sel2" '>' (Chain "sel3" ' ' (Last "sel4"))),  4)
  ]




{- -------------------------------------------------------------------------- -}




-- On success return empty string. On failure return string showing
-- approximately where the problem is.
anyDatumTestFunction :: [(Chain DatumType LinkType, (DatumType -> Bool), Bool)] -> T.Text
anyDatumTestFunction []     = ""
anyDatumTestFunction (x:xs) = if not success
                              then T.pack ("Got: " ++ show output ++ ", Expected: " ++ (show expectedOutput) ++ "; input = " ++ (show input))
                              else anyDatumTestFunction xs
  where
    success = output == expectedOutput
    output  = chainAnyDatum pred input

    input          = triplet1st x
    pred           = triplet2nd x
    expectedOutput = triplet3rd x




anyDatumTestData =
  [
    -- I'm using "isInfixOf" because my version of Data.Text doesn't provide "elem" :(

    (Last  "sel",                                         (\x -> T.isInfixOf "e" x), True)
  , (Last  "sel",                                         (\x -> T.isInfixOf "U" x), False)

  , (Chain "first" '+' (Last  "second"),                  (\x -> T.isInfixOf "i" x), True)
  , (Chain "first" '+' (Last  "second"),                  (\x -> T.isInfixOf "n" x), True)

  , (Chain "first" '+' (Last  "second"),                  (\x -> T.isInfixOf "G" x), False)
  , (Chain "first" '+' (Last  "second"),                  (\x -> T.isInfixOf "Q" x), False)

  , (Chain "ene" '+' (Chain "due" '>' (Last  "like")),    (\x -> T.isInfixOf "n" x), True)
  , (Chain "ene" '+' (Chain "due" '>' (Last  "like")),    (\x -> T.isInfixOf "u" x), True)
  , (Chain "ene" '+' (Chain "due" '>' (Last  "like")),    (\x -> T.isInfixOf "k" x), True)

  , (Chain "ene" '+' (Chain "due" '>' (Last  "like")),    (\x -> T.isInfixOf "A" x), False)
  , (Chain "ene" '+' (Chain "due" '>' (Last  "like")),    (\x -> T.isInfixOf "B" x), False)
  , (Chain "ene" '+' (Chain "due" '>' (Last  "like")),    (\x -> T.isInfixOf "C" x), False)
  ]




{- -------------------------------------------------------------------------- -}




-- On success return empty string. On failure return string showing
-- approximately where the problem is.
getFirstDatumTestFunction :: [(Chain DatumType LinkType, DatumType)] -> T.Text
getFirstDatumTestFunction []     = ""
getFirstDatumTestFunction (x:xs) = if not success
                                   then T.pack ("Got: " ++ show output ++ ", Expected: " ++ (show expectedOutput) ++ "; input = " ++ (show input))
                                   else getFirstDatumTestFunction xs
  where
    success = output == expectedOutput
    output  = chainGetFirstDatum input

    input          = fst x
    expectedOutput = snd x




getFirstDatumTestData =
  [
    (Last  "first",                                                               "first")
  , (Chain "first" '+' (Last  "second"),                                          "first")
  , (Chain "first" '+' (Chain "second" '>' (Last  "third")),                      "first")
  , (Chain "first" '+' (Chain "second" '>' (Chain "third" ' ' (Last "fourth"))),  "first")
  ]




{- -------------------------------------------------------------------------- -}




-- If some error is found, test function returns non-empty string which can
-- help identify a test that failed.
testCases =
  [
    TestCase (do assertEqual "manual tests of chainDatumLength"         "" (datumLengthTestFunction   datumLengthTestData))
  , TestCase (do assertEqual "manual tests of chainAnyDatum"            "" (anyDatumTestFunction      anyDatumTestData))
  , TestCase (do assertEqual "manual tests of chainGetFirstDatum"       "" (getFirstDatumTestFunction getFirstDatumTestData))
  ]




testsChain :: IO String
testsChain = do
  counts <- runTestTT (TestList (testCases))
  if (errors counts + failures counts == 0)
    then return ""
    else return "[EE] Hello.Tests.Chain failed"

