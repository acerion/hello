{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}




module Hello.Tests.Css.Parser.Declaration
  (
    testsCssParserDeclaration
  )
where




import qualified Data.Text as T
import Test.HUnit
--import Debug.Trace

import Hello.Css.Declaration
import Hello.Css.Tokenizer
import Hello.Css.Parser.Rule




{- -------------------------------------------------------------------------- -}




data ParsePropertyData = ParsePropertyData
  { inRemainderP  :: T.Text     -- ^ Input remainder to be parsed and turned
                                -- into a set of declarations.
  , expectedP     :: Maybe ((CssParser, CssToken), CssDeclaration) -- ^ Expected result of the tested function
  } deriving (Show, Eq)




parsePropertyTestData :: [ParsePropertyData]
parsePropertyTestData =
  [
    -- Success case: simple property
    ParsePropertyData { inRemainderP = "color: red}"
                      , expectedP    = Just ( nextToken . defaultParserInBlock $ "}"
                                            , defaultDeclaration { property = CssPropertyColor (CssValueColor 0xff0000) }
                                            )
                      }

    -- Success case: without closing brace
  , ParsePropertyData { inRemainderP = "color: red"
                      , expectedP    = Just ( nextToken . defaultParserInBlock $ ""
                                            , defaultDeclaration { property = CssPropertyColor (CssValueColor 0xff0000) }
                                            )
                      }

    -- Success case: simple property, with some spaces
    --
    -- TODO: should the spaces at the beginning and at the end of this string
    -- be parsed by parseProperty or by a function that calls parseProperty?
  , ParsePropertyData { inRemainderP = " \t\t\n color \t   \n : \t \n \t   red \t\n \n   }"
                      , expectedP    = Just ( nextToken . defaultParserInBlock $ "}"
                                            , defaultDeclaration { property = CssPropertyColor (CssValueColor 0xff0000) }
                                            )
                      }

    -- Failure case:
    -- 1. Invalid property name.
  , ParsePropertyData { inRemainderP = "3olor: red}"
                      , expectedP    = Nothing
                      }

    -- Failure case:
    -- 2. Property name is not an existing property name.
    --
    -- Notice that this testcase will fail when this implementation will
    -- start supporting custom CSS properties that start with "--".
  , ParsePropertyData { inRemainderP = "courage: 100%}"
                      , expectedP    = Nothing
                      }

    -- Failure case:
    -- 3. Missing colon after property name
  , ParsePropertyData { inRemainderP = "color red}"
                      , expectedP    = Nothing
                      }

    -- Failure case:
    -- 4. Invalid value of property
  , ParsePropertyData { inRemainderP = "color: circle}"
                      , expectedP    = Nothing
                      }

    -- Failure case:
    -- 5. Missing value
  , ParsePropertyData { inRemainderP = "color: }"
                      , expectedP    = Nothing
                      }
  ]




-- On success return empty string. On failure return string representation of
-- remainder string in a row, for which test failed.
parsePropertyTestFunction :: [ParsePropertyData] -> T.Text
parsePropertyTestFunction []     = ""
parsePropertyTestFunction (x:xs) = if not $ resultsEqual (expectedP x) parsed
                                   then T.pack . show . expectedP $ x -- parsed -- inRemainderP $ x
                                   else parsePropertyTestFunction xs
  where
    -- The tested function parses contents of {} block, so we have to use
    -- here defaultParserInBlock nextToken is used to kick-start a parser.
    pat = nextToken . defaultParserInBlock . inRemainderP $ x
    parsed = parseProperty (pat, defaultDeclaration)

    -- We can't just compare expected with parsed because a parser that we
    -- manually construct and assign to to expectedP has incorrect bufOffset.
    -- We need to compare only parts of the expected parser and resulting
    -- parser.
    resultsEqual Nothing Nothing  = True
    resultsEqual (Just _) Nothing = False
    resultsEqual Nothing (Just _) = False
    resultsEqual (Just ((p1, t1), d1)) (Just ((p2, t2), d2)) = remainder p1 == remainder p2
                                                               && t1 == t2
                                                               && d1 == d2




{- -------------------------------------------------------------------------- -}




testCases = [
  -- If some error is found, test function returns some data (e.g. non-empty
  -- string or test index) which can help identify which test failed.
     TestCase (do
                  assertEqual "manual tests of parseProperty" ""    (parsePropertyTestFunction parsePropertyTestData)
              )
  ]




testsCssParserDeclaration :: IO String
testsCssParserDeclaration = do
  testCounts <- runTestTT (TestList (testCases))
  if (errors testCounts + failures testCounts == 0)
    then return ""
    else return "[EE] Hello.Tests.Css.Parser.Declaration failed"


