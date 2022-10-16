{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}




module Hello.Tests.Css.Parser.Rule
  (
    testsCssParserRule
  )
where




import qualified Data.Text as T
import qualified Data.Sequence as S
import Test.HUnit
--import Debug.Trace

import Hello.Chain
import Hello.Css.Declaration
import Hello.Css.Tokenizer
import Hello.Css.Parser.Rule
import Hello.Css.Selector
import Hello.Html.Tag




data ParseStyleRuleData = ParseStyleRuleData
  { remainderIn       :: T.Text     -- ^ Input remainder to be parsed and
                                    -- turned into CSS rule (into rule's
                                    -- ingredients).
  , remainderExpected :: T.Text     -- ^ What should left in remainder after
                                    -- all tokens of a rule are taken
                                    -- (remember that first token from
                                    -- past-rule string will be taken as
                                    -- current token.
  , tokenExpected     :: CssToken   -- ^ Expected value of current token
                                    -- after given remainderIn is parsed.
  , rulePartsExpected :: Maybe ([CssCachedComplexSelector], CssDeclarationSet, CssDeclarationSet) -- ^ Expected output of tested function.
  } deriving (Show, Eq)




-- Test of getTopCompound function
--
-- "top compound selector" is the rightmost compound selector in a rule, and data
-- structure representing it is storing it at the end of a list. This test is
-- meant to ensure that even if we change the data structure, the tested
-- function will always return the right compound selector.
--
-- This array is called "Manual" because these tests were written manually.
-- Perhaps in the future I will write some generator of test data.
parseStyleRuleTestData :: [ParseStyleRuleData]
parseStyleRuleTestData =
  [
    -- Just a single valid rule.
    ParseStyleRuleData { remainderIn       = "body {color: black;background-color: #ffff00; line-height: normal;}"
                       , remainderExpected = ""
                       , tokenExpected     = CssTokEnd
                       , rulePartsExpected = Just ( [defaultComplexSelector { chain = Last defaultCssCompoundSelector{selectorTagName = CssTypeSelector . htmlTagIndex $ "body"} }]
                                                  , defaultCssDeclarationSet { items =
                                                                               S.fromList [ CssDeclaration { property = CssPropertyColor (CssValueColor 0x000000),                          important = False }
                                                                                          , CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0xffff00), important = False }
                                                                                          , CssDeclaration { property = CssPropertyLineHeight CssValueLineHeightNormal,                     important = False }
                                                                                          ]
                                                                             }
                                                  , defaultCssDeclarationSet
                                                  )
                       }

    -- Almost the same valid rule as above, but with minor changes to spaces
    -- and semicolons in input test, and one color changed to make the rule
    -- unique.
  , ParseStyleRuleData { remainderIn       = "body{ color:red ; background-color: #ffff00;line-height: normal}"
                       , remainderExpected = ""
                       , tokenExpected     = CssTokEnd
                       , rulePartsExpected = Just ( [defaultComplexSelector { chain = Last defaultCssCompoundSelector{selectorTagName = CssTypeSelector . htmlTagIndex $ "body"} }]
                                                  , defaultCssDeclarationSet { items =
                                                                               S.fromList [ CssDeclaration { property = CssPropertyColor (CssValueColor 0xff0000),                          important = False }
                                                                                          , CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0xffff00), important = False }
                                                                                          , CssDeclaration { property = CssPropertyLineHeight CssValueLineHeightNormal,                     important = False }
                                                                                          ]
                                                                             }
                                                  , defaultCssDeclarationSet
                                                  )
                       }

    -- Almost the same valid rule as above, but with some string after the
    -- rule. That string will start to be parsed (the token will appear in
    -- tokenExpected), and output remainder will be non-empty.
  , ParseStyleRuleData { remainderIn       = "body{ color:red ; background-color: #ffff00;line-height: normal}h1{color:blue}"
                       , remainderExpected = "{color:blue}"
                       , tokenExpected     = CssTokIdent "h1"
                       , rulePartsExpected = Just ( [defaultComplexSelector { chain = Last defaultCssCompoundSelector{selectorTagName = CssTypeSelector . htmlTagIndex $ "body"} }]
                                                  , defaultCssDeclarationSet { items =
                                                                               S.fromList [ CssDeclaration { property = CssPropertyColor (CssValueColor 0xff0000),                          important = False }
                                                                                          , CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0xffff00), important = False }
                                                                                          , CssDeclaration { property = CssPropertyLineHeight CssValueLineHeightNormal,                     important = False }
                                                                                          ]
                                                                             }
                                                  , defaultCssDeclarationSet
                                                  )
                       }
  ]




-- On success return empty string. On failure return string representation of
-- remainder string in a row, for which test failed.
parseStyleRuleTestFunction :: [ParseStyleRuleData] -> T.Text
parseStyleRuleTestFunction []     = ""
parseStyleRuleTestFunction (x:xs) = if rulePartsExpected x /= ruleParts || tokenExpected x /= token' || remainderExpected x /= remainder parser'
                                    then remainderIn x -- T.pack . show $ ruleParts
                                    else parseStyleRuleTestFunction xs
  where

    parser = defaultParser{remainder = remainderIn x}
    token  = CssTokNone
    ((parser', token'), ruleParts) = parseStyleRule (parser, token)




testCases = [
  -- If some error is found, test function returns some data (e.g. non-empty
  -- string or test index) which can help identify which test failed.
     TestCase (do
                 assertEqual "manual tests of parseStyleRule" "" (parseStyleRuleTestFunction parseStyleRuleTestData))
  ]




testsCssParserRule :: IO String
testsCssParserRule = do
  testCounts <- runTestTT (TestList (testCases))
  if (errors testCounts + failures testCounts == 0)
    then return ""
    else return "[EE] Hello.Tests.Css.Parser.Rule failed"


