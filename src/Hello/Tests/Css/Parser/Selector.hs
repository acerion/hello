{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}




module Hello.Tests.Css.Parser.Selector
  (
    testsCssParserSelector
  )
where




import qualified Data.Text as T
import Test.HUnit
--import Debug.Trace

import Hello.Css.Tokenizer
import Hello.Css.Parser.Selector
import Hello.Css.Selector
import Hello.Html.Tag
import Hello.Utils.Parser

import qualified Hello.Tests.Utils.Hunit as H.H




-- Tests of parsing of compound selectors.
--
-- This array is called "Manual" because these tests were written manually.
-- Perhaps in the future I will write some generator of test data.
parserCompoundSelectorTestData :: [(T.Text, Maybe ((CssParser, CssToken), SelectorWrapper))]
parserCompoundSelectorTestData = [
  -- parser's remainder before/after      expected selector

  -- Recognition of most basic case: no selector in input buffer.
    ( "0",
      Nothing )

  -- Recognition of most basic case: just "id" selector.
  , ( "#some_id",
      Just (((defaultParser "") { bufOffset = 8 }, CssTokEnd)
           , WrapCompound $ CssCompoundSelector { selectorPseudoClass = []
                                                , selectorId = "some_id"
                                                , selectorClass = []
                                                , selectorTagName = CssTypeSelectorUniversal
                                                }
           )
    )


  -- Recognition of most basic case: just "class" selector.
  , ( ".some_class",
      Just (((defaultParser "") { bufOffset = 11 }, CssTokEnd)
           , WrapCompound $ CssCompoundSelector { selectorPseudoClass = []
                                                , selectorId = ""
                                                , selectorClass = ["some_class"]
                                                , selectorTagName = CssTypeSelectorUniversal
                                                }
           )
    )

  -- Recognition of most basic case: just "pseudo class" selector.
  , ( ":link",
      Just (((defaultParser "") { bufOffset = 5 }, CssTokEnd)
           , WrapCompound $ CssCompoundSelector { selectorPseudoClass = ["link"]
                                                , selectorId = ""
                                                , selectorClass = []
                                                , selectorTagName = CssTypeSelectorUniversal
                                                }
           )
    )
  ]




-- On success return empty string. On failure return string representation of
-- remainder string in a row, for which test failed.
parserCompoundSelectorTest :: [(T.Text, Maybe ((CssParser, CssToken), SelectorWrapper))] -> [T.Text]
parserCompoundSelectorTest xs = foldr f [] xs
  where
    f x acc = if expected /= result
              then errMsg:acc
              else acc
      where
        inputRemainder = fst x
        expected       = snd x
        result = runParser parserCompoundSelector (startTokenizer . defaultParser $ inputRemainder)
        errMsg = T.pack ("\n*** input remainder = " ++ show inputRemainder ++ "\n*** expected = " ++ show expected ++ "\n*** result =   " ++ show result)




{- -------------------------------------------------------------------------- -}




-- Tests of parsing of complex selector, most basic cases
--
-- This array is called "Manual" because these tests were written manually.
-- Perhaps in the future I will write some generator of test data.
parserComplexSelectorTestManualDataBasic :: [(T.Text, Maybe ((CssParser, CssToken), [SelectorWrapper]))]
parserComplexSelectorTestManualDataBasic =
  [
    -- Success case: an example from https://www.w3.org/TR/selectors-4/#descendant-combinators
    ( "div * p",             Just (((defaultParser "") { bufOffset = 7 }, CssTokEnd)
                                  , [ WrapCompound defaultCssCompoundSelector { selectorTagName = CssTypeSelector . htmlTagIndex $ "div" }
                                    , WrapCombinator CssCombinatorDescendant
                                    , WrapCompound defaultCssCompoundSelector { selectorTagName = CssTypeSelectorUniversal }
                                    , WrapCombinator CssCombinatorDescendant
                                    , WrapCompound defaultCssCompoundSelector { selectorTagName = CssTypeSelector . htmlTagIndex $ "p" }
                                    ]
                                  )
    )

    -- Success case: recognition of most basic case: just "id" selector.
  , ( "#some_id",            Just (((defaultParser "") { bufOffset = 8 }, CssTokEnd)
                                  , [ WrapCompound defaultCssCompoundSelector { selectorId = "some_id" }
                                    ]
                                  )
    )

    -- Success case: recognition of most basic case: just "id" selector. The
    -- next '#' is treated as belonging to something after the complex
    -- selector, to be parsed by other parser.
  , ( "#some_id #",          Just (((defaultParser "#") { bufOffset = 9, spaceSeparated = True }, CssTokWS)
                                  , [ WrapCompound defaultCssCompoundSelector { selectorId = "some_id" }
                                    ]
                                  )
    )

    -- Failure case: additional '#' character which leads to invalid id
    -- selector, which leads to empty (invalid) complex selector, which leads
    -- to invalid parsing.
  , ( "##some_id",           Nothing
    )
  ]




-- On success return empty string. On failure return string representation of
-- remainder string in a row, for which test failed.
parserComplexSelectorTest :: [(T.Text, Maybe ((CssParser, CssToken), [SelectorWrapper]))] -> T.Text
parserComplexSelectorTest []     = ""
parserComplexSelectorTest (x:xs) = if expectedResult /= result
                                   then T.pack ("remainder before = " ++ T.unpack remainderBefore ++ ", result = " ++ show result)
                                   else parserComplexSelectorTest xs
  where
    remainderBefore  = fst x
    expectedResult   = snd x

    result = case runParser parserComplexSelector (startTokenizer . defaultParser $ remainderBefore) of
               Just (_, [])   -> Nothing
               Just (pat', l) -> Just (pat', l)
               _              -> Nothing




{- -------------------------------------------------------------------------- -}




-- Tests of parsing of list of complex selectors
--
-- This array is called "Manual" because these tests were written manually.
-- Perhaps in the future I will write some generator of test data.
parseListOfSelectorsTestManualData :: [(T.Text, Maybe ((CssParser, CssToken), [CssParsedComplexSelector]))]
parseListOfSelectorsTestManualData =
  [

    -- Success case: one-element list
    ( "body",
      Just (((defaultParser "") { bufOffset = 4 }, CssTokEnd)
           , [ [ WrapCompound defaultCssCompoundSelector { selectorTagName = CssTypeSelector . htmlTagIndex $ "body" }]
             ]
           )
    )

    -- Success case: a list of two non-complicated compound selectors.
  , ( "div, p",
      Just (((defaultParser "") { bufOffset = 6 }, CssTokEnd)
           , [ [ WrapCompound defaultCssCompoundSelector { selectorTagName = CssTypeSelector . htmlTagIndex $ "div" }]
             , [ WrapCompound defaultCssCompoundSelector { selectorTagName = CssTypeSelector . htmlTagIndex $ "p" }]
             ]
           )
    )

    -- Success case: The same as above, but with some spaces.
  ,  ( " div , p ",
       Just (((defaultParser "") { bufOffset = 9, spaceSeparated = True }, CssTokEnd)
            , [ [ WrapCompound defaultCssCompoundSelector { selectorTagName = CssTypeSelector . htmlTagIndex $ "div" }]
              , [ WrapCompound defaultCssCompoundSelector { selectorTagName = CssTypeSelector . htmlTagIndex $ "p" }]
              ]
            )
    )

    -- Success case: compound selectors are a bit more complicated.
  ,  ( " div#some_id , p > h1.some_class h2 ",
       Just (((defaultParser "") { bufOffset = 36, spaceSeparated = True }, CssTokEnd)
            , [ [ WrapCompound defaultCssCompoundSelector { selectorTagName = CssTypeSelector . htmlTagIndex $ "div"
                                                          , selectorId      = "some_id" }]
              , [ WrapCompound defaultCssCompoundSelector { selectorTagName = CssTypeSelector . htmlTagIndex $ "p" }
                , WrapCombinator CssCombinatorChild
                , WrapCompound defaultCssCompoundSelector { selectorTagName = CssTypeSelector . htmlTagIndex $ "h1"
                                                          , selectorClass   = ["some_class"] }
                , WrapCombinator CssCombinatorDescendant
                , WrapCompound defaultCssCompoundSelector { selectorTagName = CssTypeSelector . htmlTagIndex $ "h2" }
                ]
              ]
            )
    )

{- TODO: what to do with this case? Currently the test doesn't pass.
    -- Failure case: empty selector.
  , ( "",
      Nothing
    )
-}
  ]




-- On success return empty string. On failure return string representation of
-- remainder string in a row, for which test failed.
parseListOfSelectorsTest :: [(T.Text, Maybe ((CssParser, CssToken), [CssParsedComplexSelector]))] -> T.Text
parseListOfSelectorsTest []     = ""
parseListOfSelectorsTest (x:xs) = if expectedResult /= result
                                  then T.pack ("remainder before = " ++ T.unpack remainderBefore ++ ", result = " ++ show result)
                                  else parseListOfSelectorsTest xs
  where
    remainderBefore  = fst x
    expectedResult   = snd x
    result = runParser parserSelectorList (startTokenizer . defaultParser $ remainderBefore)




{- -------------------------------------------------------------------------- -}




selectorTestCases :: [Test]
selectorTestCases =
  [
    -- If some error is found, test function returns some data (e.g. non-empty
    -- string or test index) which can help identify which test failed.
    TestCase (do H.H.assertSuccess "manual tests of parserCompoundSelector"               (parserCompoundSelectorTest parserCompoundSelectorTestData))
  , TestCase (do assertEqual "manual tests of parserComplexSelector"                   "" (parserComplexSelectorTest parserComplexSelectorTestManualDataBasic))
  , TestCase (do assertEqual "manual tests of parserSelectorList"                      "" (parseListOfSelectorsTest parseListOfSelectorsTestManualData))
  ]




testsCssParserSelector :: IO String
testsCssParserSelector = do
  testCounts <- runTestTT (TestList selectorTestCases)
  if errors testCounts + failures testCounts == 0
    then return ""
    else return "[EE] Hello.Tests.Css.Parser.Selector failed"

