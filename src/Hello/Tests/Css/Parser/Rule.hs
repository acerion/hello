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
import Hello.Css.Parser.Declaration
import Hello.Css.Parser.Rule
import Hello.Css.Selector
import Hello.Html.Tag




{- -------------------------------------------------------------------------- -}




data ReadDeclarationsData = ReadDeclarationsData
  { remainderInDs       :: T.Text     -- ^ Input remainder to be parsed and
                                      -- turned into a set of declarations.
  , remainderExpectedDs :: T.Text     -- ^ What should be left in remainder
                                      -- after all tokens of a declarations
                                      -- are taken (remember that first token
                                      -- from past-declarations string will
                                      -- be taken as current token.
  , tokenExpectedDs     :: CssToken   -- ^ Expected value of current token
                                      -- after given remainderIn is parsed.
  , expectedDeclSets   :: CssDeclarationSets -- ^ Expected output of tested function.
  } deriving (Show, Eq)





readDeclarationsTestData :: [ReadDeclarationsData]
readDeclarationsTestData =
  [
    -- Single declaration. No spaces or semicolons at beginning/end of declaration.
    ReadDeclarationsData { remainderInDs       = "color: #abab30}"
                         , remainderExpectedDs = ""
                         , tokenExpectedDs     = CssTokBraceCurlyClose
                         , expectedDeclSets    =
                             ( defaultCssDeclarationSet { items =
                                                            S.fromList [ CssDeclaration { property = CssPropertyColor (CssValueColor 0xabab30), important = False }
                                                                       ]
                                                        }
                             , defaultCssDeclarationSet )
                         }

    -- Single declaration. Spaces before and after declaration.
    -- Tested function can consume these spaces in both places.
  , ReadDeclarationsData { remainderInDs       = "  \t \n\t color: #abab31\n\n\t}"
                         , remainderExpectedDs = ""
                         , tokenExpectedDs     = CssTokBraceCurlyClose
                         , expectedDeclSets    =
                             ( defaultCssDeclarationSet { items =
                                                            S.fromList [ CssDeclaration { property = CssPropertyColor (CssValueColor 0xabab31), important = False }
                                                                       ]
                                                        }
                             , defaultCssDeclarationSet )
                         }

    -- Single declaration. Spaces and semicolons (multiple) at the beginning of declartion.
    -- Tested function can consume spaces + semicolons at front of declaration.
  , ReadDeclarationsData { remainderInDs       = "  ;\t ;\n;\t ;;color: #abab32}"
                         , remainderExpectedDs = ""
                         , tokenExpectedDs     = CssTokBraceCurlyClose
                         , expectedDeclSets    =
                             ( defaultCssDeclarationSet { items =
                                                            S.fromList [ CssDeclaration { property = CssPropertyColor (CssValueColor 0xabab32), important = False }
                                                                       ]
                                                        }
                             , defaultCssDeclarationSet )
                         }

    -- Single declaration. Multiple spaces + semicolons at the end. Tested
    -- function can consume multiple spaces + semicolons between
    -- declarations. The function will recognize them as something standing
    -- before second declaration, but the second declaration will be invalid
    -- and the 2nd declaration will be dropped.
  , ReadDeclarationsData { remainderInDs       = "color: #abab33\n\n\t;;;}"
                         , remainderExpectedDs = ""
                         , tokenExpectedDs     = CssTokBraceCurlyClose
                         , expectedDeclSets    =
                             ( defaultCssDeclarationSet { items =
                                                            S.fromList [ CssDeclaration { property = CssPropertyColor (CssValueColor 0xabab33), important = False }
                                                                       ]
                                                        }
                             , defaultCssDeclarationSet )
                         }

    -- Two declarations. Multiple spaces + semicolons between them. Tested
    -- function can consume multiple spaces + semicolons - the function will
    -- recognize them as something standing before second declaration.
  , ReadDeclarationsData { remainderInDs       = "color: #abab34\n\n\t;;; \t background-color: #0f0f0f}"
                         , remainderExpectedDs = ""
                         , tokenExpectedDs     = CssTokBraceCurlyClose
                         , expectedDeclSets    =
                             ( defaultCssDeclarationSet { items =
                                                            S.fromList [ CssDeclaration { property = CssPropertyColor (CssValueColor 0xabab34),                          important = False }
                                                                       , CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0x0f0f0f), important = False }
                                                                       ]
                                                        }
                             , defaultCssDeclarationSet )
                         }

    -- Three declarations. Multiple spaces + semicolons between them. Tested
    -- function can consume multiple spaces + semicolons - the function will
    -- recognize them as something standing before a declaration.
  , ReadDeclarationsData { remainderInDs       = "color: #abab35\n\n\t;;; \t background-color: #0f0f0f \t\t;; \t \n; ; line-height: normal;}"
                         , remainderExpectedDs = ""
                         , tokenExpectedDs     = CssTokBraceCurlyClose
                         , expectedDeclSets    =
                             ( defaultCssDeclarationSet { items =
                                                            S.fromList [ CssDeclaration { property = CssPropertyColor (CssValueColor 0xabab35),                          important = False }
                                                                       , CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0x0f0f0f), important = False }
                                                                       , CssDeclaration { property = CssPropertyLineHeight CssValueLineHeightNormal,                     important = False }
                                                                       ]
                                                        }
                             , defaultCssDeclarationSet )
                         }

    -- Three declarations. Multiple spaces + semicolons between them. Tested
    -- function can consume multiple spaces + semicolons - the function will
    -- recognize them as something standing before a declaration.
    --
    -- This time with "!important".
  , ReadDeclarationsData { remainderInDs       = "color: #abab36\n\n!important\t;;; \t background-color: #0f0f0f !important\t\t;; \t \n; ; line-height: normal !important;}"
                         , remainderExpectedDs = ""
                         , tokenExpectedDs     = CssTokBraceCurlyClose
                         , expectedDeclSets    =
                             ( defaultCssDeclarationSet
                             , defaultCssDeclarationSet { items =
                                                            S.fromList [ CssDeclaration { property = CssPropertyColor (CssValueColor 0xabab36),                          important = True }
                                                                       , CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0x0f0f0f), important = True }
                                                                       , CssDeclaration { property = CssPropertyLineHeight CssValueLineHeightNormal,                     important = True }
                                                                       ]
                                                        }
                             )
                         }


    -- Failure case
    --
    -- Three declarations. First of them is invalid (invalid property name).
    -- Rest of them gets parsed correctly.
  , ReadDeclarationsData { remainderInDs       = "3color: #abab37\n\n!important\t;;; \t background-color: #0f0f0f !important\t\t;; \t \n; ; line-height: normal !important\t ; \n}"
                         , remainderExpectedDs = ""
                         , tokenExpectedDs     = CssTokBraceCurlyClose
                         , expectedDeclSets    =
                             ( defaultCssDeclarationSet
                             , defaultCssDeclarationSet { items =
                                                            S.fromList [ CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0x0f0f0f), important = True }
                                                                       , CssDeclaration { property = CssPropertyLineHeight CssValueLineHeightNormal,                     important = True }
                                                                       ]
                                                        }
                             )
                         }

    -- Failure case
    --
    -- Three declarations. Second of them is invalid (invalid property name).
    -- Rest of them gets parsed correctly.
  , ReadDeclarationsData { remainderInDs       = "color: #abab38\n\n!important\t;;; \t 4background-color: #0f0f0f !important\t\t;; \t \n; ; line-height: normal !important\t ; \n}"
                         , remainderExpectedDs = ""
                         , tokenExpectedDs     = CssTokBraceCurlyClose
                         , expectedDeclSets    =
                             ( defaultCssDeclarationSet
                             , defaultCssDeclarationSet { items =
                                                            S.fromList [ CssDeclaration { property = CssPropertyColor (CssValueColor 0xabab38),                          important = True }
                                                                       , CssDeclaration { property = CssPropertyLineHeight CssValueLineHeightNormal,                     important = True }
                                                                       ]
                                                        }
                             )
                         }

    -- Failure case
    --
    -- Three declarations. Third of them is invalid (invalid property name).
    -- Rest of them gets parsed correctly.
  , ReadDeclarationsData { remainderInDs       = "color: #abab39\n\n!important\t;;; \t background-color: #0f0f0f !important\t\t;; \t \n; ; 5line-height: normal !important\t ; \n}"
                         , remainderExpectedDs = ""
                         , tokenExpectedDs     = CssTokBraceCurlyClose
                         , expectedDeclSets    =
                             ( defaultCssDeclarationSet
                             , defaultCssDeclarationSet { items =
                                                            S.fromList [ CssDeclaration { property = CssPropertyColor (CssValueColor 0xabab39),                          important = True }
                                                                       , CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0x0f0f0f), important = True }
                                                                       ]
                                                        }
                             )
                         }


    -- Failure case
    --
    -- Three declarations. All three of them are invalid (invalid property
    -- names).
  , ReadDeclarationsData { remainderInDs       = "1color: #abab40\n\n!important\t;;; \t 2background-color: #0f0f0f !important\t\t;; \t \n; ; 3line-height: normal !important\t ; \n}"
                         , remainderExpectedDs = ""
                         , tokenExpectedDs     = CssTokBraceCurlyClose
                         , expectedDeclSets    =
                             ( defaultCssDeclarationSet
                             , defaultCssDeclarationSet
                             )
                         }
  ]




-- On success return empty string. On failure return string representation of
-- remainder string in a row, for which test failed.
readDeclarationsTestFunction :: [ReadDeclarationsData] -> T.Text
readDeclarationsTestFunction []     = ""
readDeclarationsTestFunction (x:xs) = if expectedDeclSets x /= parsedDeclSets || tokenExpectedDs x /= token' || remainderExpectedDs x /= remainder parser'
                                      then T.pack . show . remainderInDs $ x
                                      else readDeclarationsTestFunction xs
  where
    -- The tested function parses contents of {} block, so we have to use here defaultParserInBlock
    -- nextToken is used to kick-start a parser.
    pat = nextToken . defaultParserInBlock . remainderInDs $ x
    ((parser', token'), parsedDeclSets) = readDeclarations (pat, (defaultCssDeclarationSet, defaultCssDeclarationSet))




{- -------------------------------------------------------------------------- -}




data ParseStyleRuleData = ParseStyleRuleData
  { remainderIn       :: T.Text     -- ^ Input remainder to be parsed and
                                    -- turned into CSS rule (into rule's
                                    -- ingredients).
  , remainderExpected :: T.Text     -- ^ What should be left in remainder
                                    -- after all tokens of a rule are taken
                                    -- (remember that first token from
                                    -- past-rule string will be taken as
                                    -- current token.
  , tokenExpected     :: CssToken   -- ^ Expected value of current token
                                    -- after given remainderIn is parsed.
  , parsedStyleRuleExpected :: Maybe CssParsedStyleRule -- ^ Expected output of tested function.
  } deriving (Show, Eq)




-- Testcases of parseStyleRule function
--
-- This array is called "Manual" because these tests were written manually.
-- Perhaps in the future I will write some generator of test data.
parseStyleRuleTestData :: [ParseStyleRuleData]
parseStyleRuleTestData =
  [
    -- Just a single valid rule with single property.
    ParseStyleRuleData { remainderIn       = "body {color: #003412;background-color: #ffff00; line-height: normal;}"
                       , remainderExpected = ""
                       , tokenExpected     = CssTokEnd
                       , parsedStyleRuleExpected =
                           Just CssParsedStyleRule
                           { prelude = [defaultComplexSelector { chain = Last defaultCssCompoundSelector{selectorTagName = CssTypeSelector . htmlTagIndex $ "body"} }]
                           , content = ( defaultCssDeclarationSet { items =
                                                                      S.fromList [ CssDeclaration { property = CssPropertyColor (CssValueColor 0x003412),                          important = False }
                                                                                 , CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0xffff00), important = False }
                                                                                 , CssDeclaration { property = CssPropertyLineHeight CssValueLineHeightNormal,                     important = False }
                                                                                 ]
                                                                  }
                                       , defaultCssDeclarationSet )
                           }
                       }

    -- Just a single valid rule with more than one property.
  , ParseStyleRuleData { remainderIn       = "body {color: #34128c;background-color: #ffff00; line-height: normal;}"
                       , remainderExpected = ""
                       , tokenExpected     = CssTokEnd
                       , parsedStyleRuleExpected =
                           Just CssParsedStyleRule
                           { prelude = [defaultComplexSelector { chain = Last defaultCssCompoundSelector{selectorTagName = CssTypeSelector . htmlTagIndex $ "body"} }]
                           , content = ( defaultCssDeclarationSet { items =
                                                                      S.fromList [ CssDeclaration { property = CssPropertyColor (CssValueColor 0x34128c),                          important = False }
                                                                                 , CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0xffff00), important = False }
                                                                                 , CssDeclaration { property = CssPropertyLineHeight CssValueLineHeightNormal,                     important = False }
                                                                                 ]
                                                                  }
                                       , defaultCssDeclarationSet )
                           }
                       }

    -- Almost the same valid rule as above, but with minor changes to spaces
    -- and semicolons in input test, and one color changed to make the rule
    -- unique.
  , ParseStyleRuleData { remainderIn       = "body{ color:red ; background-color: #ffff00;line-height: normal}"
                       , remainderExpected = ""
                       , tokenExpected     = CssTokEnd
                       , parsedStyleRuleExpected =
                           Just CssParsedStyleRule
                           { prelude = [defaultComplexSelector { chain = Last defaultCssCompoundSelector{selectorTagName = CssTypeSelector . htmlTagIndex $ "body"} }]
                           , content = ( defaultCssDeclarationSet { items =
                                                                      S.fromList [ CssDeclaration { property = CssPropertyColor (CssValueColor 0xff0000),                          important = False }
                                                                                 , CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0xffff00), important = False }
                                                                                 , CssDeclaration { property = CssPropertyLineHeight CssValueLineHeightNormal,                     important = False }
                                                                                 ]
                                                                  }
                                       , defaultCssDeclarationSet )
                           }
                       }

    -- Almost the same valid rule as above, but with another rule after
    -- parsed rule. That next rule will start to be parsed (the token will
    -- appear in tokenExpected), and output remainder will be non-empty.
  , ParseStyleRuleData { remainderIn       = "body{ color:#4c4c4c ; background-color: #ffff00;line-height: normal}h1{color:blue}"
                       , remainderExpected = "{color:blue}"
                       , tokenExpected     = CssTokIdent "h1"
                       , parsedStyleRuleExpected =
                           Just CssParsedStyleRule
                           { prelude = [defaultComplexSelector { chain = Last defaultCssCompoundSelector{selectorTagName = CssTypeSelector . htmlTagIndex $ "body"} }]
                           , content = ( defaultCssDeclarationSet { items =
                                                                      S.fromList [ CssDeclaration { property = CssPropertyColor (CssValueColor 0x4c4c4c),                          important = False }
                                                                                 , CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0xffff00), important = False }
                                                                                 , CssDeclaration { property = CssPropertyLineHeight CssValueLineHeightNormal,                     important = False }
                                                                                 ]
                                                                  }
                                       , defaultCssDeclarationSet )
                           }
                       }

    -- The same rules as above, but with changes to spaces and semicolons. The changes result in input that is still valid.
  , ParseStyleRuleData { remainderIn       = " body { color : #22aa22 ; background-color:#ffff00;line-height: normal ; } h1{color:blue}"
                       , remainderExpected = "{color:blue}"
                       , tokenExpected     = CssTokIdent "h1"
                       , parsedStyleRuleExpected =
                           Just CssParsedStyleRule
                           { prelude = [defaultComplexSelector { chain = Last defaultCssCompoundSelector{selectorTagName = CssTypeSelector . htmlTagIndex $ "body"} }]
                           , content = ( defaultCssDeclarationSet { items =
                                                                      S.fromList [ CssDeclaration { property = CssPropertyColor (CssValueColor 0x22aa22),                          important = False }
                                                                                 , CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0xffff00), important = False }
                                                                                 , CssDeclaration { property = CssPropertyLineHeight CssValueLineHeightNormal,                     important = False }
                                                                                 ]
                                                                  }
                                       , defaultCssDeclarationSet )
                           }
                       }

    -- The same rules as above, but one of properties is marked with
    -- !important.
  , ParseStyleRuleData { remainderIn       = " body { color : #ffdd33 !important; background-color:#ffff00;line-height: normal ; } h1{color:blue}"
                       , remainderExpected = "{color:blue}"
                       , tokenExpected     = CssTokIdent "h1"
                       , parsedStyleRuleExpected =
                           Just CssParsedStyleRule
                           { prelude = [defaultComplexSelector { chain = Last defaultCssCompoundSelector{selectorTagName = CssTypeSelector . htmlTagIndex $ "body"} }]
                           , content = ( defaultCssDeclarationSet { items =
                                                                      S.fromList [ CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0xffff00), important = False }
                                                                                 , CssDeclaration { property = CssPropertyLineHeight CssValueLineHeightNormal,                     important = False }
                                                                                 ]
                                                                  }
                                       , defaultCssDeclarationSet { items =
                                                                      S.fromList [ CssDeclaration { property = CssPropertyColor (CssValueColor 0xffdd33),                          important = True }
                                                                                 ]
                                                                  } )
                           }
                       }

    -- The same rules as above, but one of properties is marked with
    -- !important, and there are more valid changes to spaces and semicolons
  , ParseStyleRuleData { remainderIn       = " body { color : #2a3b3d   !important   ; background-color :     #ffff00    ;     line-height: normal  } h1{color:blue}"
                       , remainderExpected = "{color:blue}"
                       , tokenExpected     = CssTokIdent "h1"
                       , parsedStyleRuleExpected =
                           Just CssParsedStyleRule
                           { prelude = [defaultComplexSelector { chain = Last defaultCssCompoundSelector{selectorTagName = CssTypeSelector . htmlTagIndex $ "body"} }]
                           , content = ( defaultCssDeclarationSet { items =
                                                                      S.fromList [ CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0xffff00), important = False }
                                                                                 , CssDeclaration { property = CssPropertyLineHeight CssValueLineHeightNormal,                     important = False }
                                                                                 ]
                                                                  }
                                       , defaultCssDeclarationSet { items =
                                                                      S.fromList [ CssDeclaration { property = CssPropertyColor (CssValueColor 0x2a3b3d),                          important = True }
                                                                                 ]
                                                                  } )
                           }
                       }

    -- The same rules as above, but !important is mis-spelled, so that
    -- property is invalid and is rejected. The rest of properties are still
    -- parsed and accepted.
  , ParseStyleRuleData { remainderIn       = " body { color : #098765 !import_ant; background-color:#ffff00;line-height: normal ; } h1{color:blue}"
                       , remainderExpected = "{color:blue}"
                       , tokenExpected     = CssTokIdent "h1"
                       , parsedStyleRuleExpected =
                           Just CssParsedStyleRule
                           { prelude = [defaultComplexSelector { chain = Last defaultCssCompoundSelector{selectorTagName = CssTypeSelector . htmlTagIndex $ "body"} }]
                           , content = ( defaultCssDeclarationSet { items =
                                                                      S.fromList [ CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0xffff00), important = False }
                                                                                 , CssDeclaration { property = CssPropertyLineHeight CssValueLineHeightNormal,                     important = False }
                                                                                 ]
                                                                  }
                                       , defaultCssDeclarationSet )
                           }
                       }

    -- Failure case: missing opening brace. The invalid rule will be skipped.
  , ParseStyleRuleData { remainderIn       = "body color:#386ccc ; background-color: #ffff00;line-height: normal}h1{color:blue}"
                       , remainderExpected = "{color:blue}"
                       , tokenExpected     = CssTokIdent "h1"
                       , parsedStyleRuleExpected = Nothing
                       }

    -- Failure case: invalid compound selector. The invalid rule will be
    -- skipped.
    --
    -- https://www.w3.org/TR/css-syntax-3/#style-rules: "The prelude of the
    -- qualified rule is parsed as a <selector-list>. If this returns
    -- failure, the entire style rule is invalid."
  , ParseStyleRuleData { remainderIn       = "33 {color:#aca3de ; background-color: #ffff00;line-height: normal} h1{color: rgb(0, 255, 0)} h2{color: #001122}"
                       , remainderExpected = "{color: rgb(0, 255, 0)} h2{color: #001122}"
                       , tokenExpected     = CssTokIdent "h1"
                       , parsedStyleRuleExpected = Nothing
                       }

    -- Failure case: Invalid selector (due to two combinators next to each
    -- other). The invalid rule will be skipped.
  , ParseStyleRuleData { remainderIn       = "body + > h1 {color:#aca001 ; background-color: #55ff00;line-height: normal} h2{color: rgb(11, 22, 33)} h3{color: #043256}"
                       , remainderExpected = "{color: rgb(11, 22, 33)} h3{color: #043256}"
                       , tokenExpected     = CssTokIdent "h2"
                       , parsedStyleRuleExpected = Nothing
                       }

    -- Failure case: single rule with single property, but the property is
    -- invalid. Right now the expected result is that parser will output a
    -- rule, but rule's content (set of declarations) will be empty.
  , ParseStyleRuleData { remainderIn       = "body {44: black;}"
                       , remainderExpected = ""
                       , tokenExpected     = CssTokEnd
                       , parsedStyleRuleExpected =
                           Just CssParsedStyleRule
                           { prelude = [defaultComplexSelector { chain = Last defaultCssCompoundSelector{selectorTagName = CssTypeSelector . htmlTagIndex $ "body"} }]
                           , content = ( defaultCssDeclarationSet
                                       , defaultCssDeclarationSet )
                           }
                       }

    -- Failure case: single rule with single property, but the rule is
    -- invalid (because of invalid selector). Entire rule is skipped.
  , ParseStyleRuleData { remainderIn       = "78c {color: black;}"
                       , remainderExpected = ""
                       , tokenExpected     = CssTokEnd
                       , parsedStyleRuleExpected = Nothing
                       }
  ]




-- On success return empty string. On failure return string representation of
-- remainder string in a row, for which test failed.
parseStyleRuleTestFunction :: [ParseStyleRuleData] -> T.Text
parseStyleRuleTestFunction []     = ""
parseStyleRuleTestFunction (x:xs) = if parsedStyleRuleExpected x /= parsedStyleRule || tokenExpected x /= token' || remainderExpected x /= remainder parser'
                                    then remainderIn x -- T.pack . show $ parsedStyleRule
                                    else parseStyleRuleTestFunction xs
  where

    parser = defaultParser . remainderIn $ x
    ((parser', token'), parsedStyleRule) = parseStyleRule (nextToken parser)




testCases :: [Test]
testCases = [
  -- If some error is found, test function returns some data (e.g. non-empty
  -- string or test index) which can help identify which test failed.
     TestCase (do
                  assertEqual "manual tests of readDeclarations" "" (readDeclarationsTestFunction readDeclarationsTestData)
                  assertEqual "manual tests of parseStyleRule" ""   (parseStyleRuleTestFunction parseStyleRuleTestData))
  ]




testsCssParserRule :: IO String
testsCssParserRule = do
  testCounts <- runTestTT (TestList testCases)
  if errors testCounts + failures testCounts == 0
    then return ""
    else return "[EE] Hello.Tests.Css.Parser.Rule failed"


