{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}




module Hello.Tests.Utils
  (
    testsUtils
  )
where




import Data.Char
import qualified Data.Text as T
import Test.HUnit
--import Debug.Trace

import Hello.Utils

import qualified Hello.Tests.Utils.Hunit as H.H




{- -------------------------------------------------------------------------- -}




roundIntTestData :: [(Float, Int)]
roundIntTestData =
  [
    (    0.00,    0 )

  , (    0.10,    0 )
  , (    0.51,    1 )
  , (   12.00,   12 )
  , (  122.41,  122 )
  , (  135.72,  136 )
  , (  142.50,  143 ) -- Haskell's 'round' function returns 142

  , (   -0.44,    0 )
  , (   -0.90,   -1 )
  , (   -1.00,   -1 )
  , (   -5.60,   -6 )
  , (   -6.02,   -6 )
  , (  -12.00,  -12 )
  , (  -30.50,  -31 ) -- Haskell's 'round' function returns -30
  , ( -100.00, -100 )
  ]




-- On success return empty string. On failure return string representation of
-- remainder string in a row, for which test failed.
roundIntTest :: [(Float, Int)] -> T.Text
roundIntTest []     = ""
roundIntTest (x:xs) = if expected /= roundInt f
                      then T.pack (show f ++ ": " ++ show (roundInt f) ++ " /= " ++ show expected)
                      else roundIntTest xs
  where
    f        = fst x
    expected = snd x




{- -------------------------------------------------------------------------- -}




composeTestData :: [([String -> String], String, String)]
composeTestData =
  [
    ( [ (\ x -> '1':x), (\ x -> '2':x), (\ x -> '3':x) ]
    , ""
    , "321" )

  , ( [ (\ x -> '1':x), (\ x -> '2':x), (\ x -> '3':x) ]
    , "XYZ"
    , "321XYZ" )

    , ( [ (\ x -> fmap toUpper x), (\ x -> filter (\ c -> isUpper c) x) ]
    , "abc"
    , "ABC" )
  ]




-- On success return empty string. On failure return string representation of
-- remainder string in a row, for which test failed.
composeTestFunction :: [([String -> String], String, String)] -> [T.Text]
composeTestFunction []     = []
composeTestFunction (x:xs) = if expectedOutput /= compose fs input
                             then [errMsg]
                             else composeTestFunction xs
  where
    fs             = triplet1st x
    input          = triplet2nd x
    expectedOutput = triplet3rd x
    errMsg         = T.pack $ "Expected: " ++ show expectedOutput ++ "\nGot: " ++ show (compose fs input)




{- -------------------------------------------------------------------------- -}




testCases :: [Test]
testCases =
  [
    -- If some error is found, test function returns some data (e.g. non-empty
    -- string or test index) which can help identify which test failed.
    TestCase (do assertEqual "manual tests of roundInt" ""        (roundIntTest roundIntTestData))
  , TestCase (do H.H.assertSuccess "manual tests of compose"      (composeTestFunction composeTestData))
  ]




testsUtils :: IO String
testsUtils = do
  testCounts <- runTestTT (TestList testCases)
  if errors testCounts + failures testCounts == 0
    then return ""
    else return "[EE] Hello.Tests.Utils failed"

