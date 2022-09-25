{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}




module Hello.Tests.Css.Parser.QuickCheck
  (
    testsCssParserQuickCheck
  )
where




import qualified Data.List as L

import qualified Test.QuickCheck as QC

--import Debug.Trace

import Hello.Css.Declaration
import Hello.Css.Parser
import Hello.Css.Tokenizer

import Hello.Tests.Css.Parser.Utils
import Hello.Tests.Utils.QuickCheck




{-
Code needed for generating a pair of value and unit. A float is a value of
distance, and string is an unit of the distance.

CSS specifies that for 'margin' property (or other property) at most 4
distances are allowed for top/right/bottom/left margin.

"NN" stands for "non-negative". CSS dictatest that values of 'margin'
property must be non-negative.
-}




data ValuesAndUnitsNN = ValuesAndUnitsNN
  { v :: [Float]
  , u :: [String]
  }
  deriving (Show)




instance QC.Arbitrary ValuesAndUnitsNN where
  arbitrary = do
    v <- QC.vectorOf 4 getValueNN
    u <- QC.vectorOf 4 getUnit
    return $ ValuesAndUnitsNN v u




getValueNN :: QC.Gen Float
getValueNN = do
  value <- QC.choose (0, 20000) :: QC.Gen Int
  let v = (fromIntegral value) / 50 :: Float
  return v




-- Notice that you can generate float values like this, but it's harder to
-- generate "0.0" value with this code, and it's easer to generate value that
-- will be shown in exponent notation.
{-
getValueNN :: QC.Gen Float
getValueNN = do
  value <- QC.choose (0, 50) :: QC.Gen Float
  return value
-}




-- TODO: add more units? Percentages?
getUnit :: QC.Gen String
getUnit = do
  let units :: [String] = ["mm", "px", "em", "ex"]
  idx <- QC.choose (0, (L.length units) - 1) :: QC.Gen Int
  let u = units !! idx
  return u




-- Test success cases of parsing of 'margin' property. The 'margin' property
-- can have 4, 3, 2 or 1 value that can be assigned in different ways to
-- top/right/bottom/left margin.
parse4321trblMarginSuccess :: ValuesAndUnitsNN -> Bool
parse4321trblMarginSuccess ValuesAndUnitsNN { v = values, u = units } = expected == outDeclarations
--parse4321trblMarginSuccess ValuesAndUnitsNN { v = values, u = units } = trace (traceData) (expected == outDeclarations)
  where
    (pat', outDeclarations) = parseSingleDeclaration pat
    pat                     = nextToken2 defaultParser { remainder = input }
    (input, expected)       = buildSuccessRow "margin" [CssPropertyMarginTop, CssPropertyMarginRight, CssPropertyMarginBottom, CssPropertyMarginLeft] CssValueMarginDistance units values

    -- For debugging only
    traceData       = show valuesWithUnits ++ "  " ++ show outDeclarations
    valuesWithUnits = zipWith (\v u -> (show v) ++ u) values units




-- TODO: the parsing code doesn't handle exponent notation of values well.
-- QuickCheck tests can fail like this:
--
-- Failed! Falsified (after 5 tests):
-- ValuesAndUnitsNN {v = [622.59,9.0e-2,853.38,416.31], u = ["mm","ex","em","em"]}
--
-- Verify if exponent values are allowed by CSS (they probably are, but
-- perhaps in slightly different form).testsCssParserQuickCheck :: IO String
testsCssParserQuickCheck = do
  -- All I had to do to find stdArgs and xWith is to read documentation :)
  -- http://hackage.haskell.org/package/QuickCheck-2.8/docs/Test-QuickCheck.html
  let testArgs = QC.stdArgs { QC.maxSuccess = 500, QC.maxSize = 400 }
  result <- QC.quickCheckWithResult testArgs parse4321trblMarginSuccess
  --result <- QC.verboseCheckWithResult testArgs parse4321trblMarginSuccess
  let failures1 = if qcResultIsSuccess result
                  then ""
                  else "[EE] Hello.Css.Parser.QuickCheck for 'margin' has failed "

  return failures1

