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
import Hello.Css.Distance
import Hello.Css.Tokenizer
import Hello.Css.Parser.Declaration




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




{-
Test a function that parses a CSS declaration: a property name + property
value. Tested function can return multiple CSS declarations for property
names such as "background" or "border".
-}




parseSingleDeclarationTestData =
  [
    ( "background: red",
      Just CssDeclaration { property = CssPropertyBackground
                            $ initialValueBackground { backgroundColor = CssValueBackgroundColorColor 0xff0000 }
                          , important = False }
    )
  -- The same as above, but with important flag set.
  , ( "background: red !important",
      Just CssDeclaration { property = CssPropertyBackground
                            $ initialValueBackground { backgroundColor = CssValueBackgroundColorColor 0xff0000 }
                          , important = True }
    )

  , ( "background: rgb(0, 255, 0) repeat-x",
      Just CssDeclaration { property = CssPropertyBackground
                            $ initialValueBackground { backgroundColor       = CssValueBackgroundColorColor 0x00ff00
                                                     , backgroundRepeatStyle = CssValueBackgroundRepeatRepeatX
                                                     }
                          , important = False }
    )
  -- The same as above, but values are in different order.
  , ( "background: repeat-x rgb(0, 255, 0)",
      Just CssDeclaration { property = CssPropertyBackground
                            $ initialValueBackground { backgroundColor       = CssValueBackgroundColorColor 0x00ff00
                                                     , backgroundRepeatStyle = CssValueBackgroundRepeatRepeatX
                                                     }
                          , important = False }
    )

  , ( "background: repeat-y scroll rgb(0, 255, 255)",
      Just CssDeclaration { property = CssPropertyBackground
                            $ initialValueBackground { backgroundColor       = CssValueBackgroundColorColor 0x00ffff
                                                     , backgroundRepeatStyle = CssValueBackgroundRepeatRepeatY
                                                     , backgroundAttachment  = CssValueBackgroundAttachmentScroll
                                                     }
                          , important = False }
    )




  , ( "background-attachment: scroll",              Just CssDeclaration { property = CssPropertyBackgroundAttachment CssValueBackgroundAttachmentScroll,       important = False } )
  , ( "background-attachment: scroll !important",   Just CssDeclaration { property = CssPropertyBackgroundAttachment CssValueBackgroundAttachmentScroll,       important = True  } )
  , ( "background-attachment: fixed",               Just CssDeclaration { property = CssPropertyBackgroundAttachment CssValueBackgroundAttachmentFixed,        important = False } )
  , ( "background-attachment: fixed !important",    Just CssDeclaration { property = CssPropertyBackgroundAttachment CssValueBackgroundAttachmentFixed,        important = True  } )

  -- Testing for parsing of bad css: invalid property name.
  , ( "background-atachment: scroll",               Nothing)
  -- Testing for parsing of bad css: invalid value.
  , ( "background-attachment: italic",              Nothing)
  -- Testing for parsing of bad css: misspelled "important "word. TODO: check how parser should behave here according to spec.
  , ( "background-attachment: fixed important",     Nothing)




  , ( "background-color: inherit",                   Just CssDeclaration { property = CssPropertyBackgroundColor CssValueBackgroundColorInherit,             important = False } )
  , ( "background-color: inherit !important",        Just CssDeclaration { property = CssPropertyBackgroundColor CssValueBackgroundColorInherit,             important = True  } )
  , ( "background-color: blue",                      Just CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0x0000ff),    important = False } )
  , ( "background-color: blue !important",           Just CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0x0000ff),    important = True  } )
  , ( "background-color: blue !important;",          Just CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0x0000ff),    important = True  } )
  , ( "background-color: #00ff00;",                  Just CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0x00ff00),    important = False } )
  , ( "background-color: #00ff00 !important",        Just CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0x00ff00),    important = True  } )
  , ( "background-color: #00ff00 !important;",       Just CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0x00ff00),    important = True  } )
  , ( "background-color: rgb(0, 0, 255)",            Just CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0x0000ff),    important = False } )
  , ( "background-color: rgb(0, 0, 255) !important", Just CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0x0000ff),    important = True  } )

  -- Testing for parsing of bad css: invalid property name.
  , ( "background-colo: blue",                       Nothing)
  -- Testing for parsing of bad css: invalid value.
  , ( "background-color: square",                    Nothing)
  , ( "background-color: 0x00ff00",                  Nothing) -- Invalid format of HEX value
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "background-color: rgb(255, 0, 0) important",  Nothing)




  -- TODO: decide what should be the value of CssValueBackgroundImageUri.
  -- Should it be just a verbatim stream, or some information about tokens that build the URI.
  -- TODO: write more tests
  , ( "background-image: url(\"background.png\")",     Just CssDeclaration { property = CssPropertyBackgroundImage (
                                                                               CssValueBackgroundImageUri "[CssTokStr \"background.png\",CssTokParenClose]"),         important = False } )




  -- Support for background position in hello is almost non-existent, so this
  -- test set is very, very, very basic.
  , ( "background-position: left top",              Just CssDeclaration { property = CssPropertyBackgroundPosition (CssValueBackgroundPositionXY 0 0),    important = False } )
  , ( "background-position: left top !important",   Just CssDeclaration { property = CssPropertyBackgroundPosition (CssValueBackgroundPositionXY 0 0),    important = True  } )

  -- Testing for parsing of bad css: invalid property name.
  , ( "backgroundposition: left top",               Nothing)
  -- Testing for parsing of bad css: invalid value.
  --, ( "background-position: italic",                Nothing)
  --, ( "background-position: left-top",              Nothing)
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "background-position: left top important",    Nothing)




  , ( "background-repeat: repeat",                  Just CssDeclaration { property = CssPropertyBackgroundRepeat CssValueBackgroundRepeatRepeat,         important = False } )
  , ( "background-repeat: repeat-x !important",     Just CssDeclaration { property = CssPropertyBackgroundRepeat CssValueBackgroundRepeatRepeatX,        important = True  } )
  , ( "background-repeat: repeat-y",                Just CssDeclaration { property = CssPropertyBackgroundRepeat CssValueBackgroundRepeatRepeatY,        important = False } )
  , ( "background-repeat: no-repeat !important",    Just CssDeclaration { property = CssPropertyBackgroundRepeat CssValueBackgroundRepeatNoRepeat,       important = True  } )

  -- Testing for parsing of bad css: invalid property name.
  , ( "background_repeat: repeat",                  Nothing)
  -- Testing for parsing of bad css: invalid value.
  , ( "background-repeat: #00ff00",                 Nothing)
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "background-repeat: no-repeat !importan",     Nothing)




  , ( "border: 10px inset #00fff1",
      Just CssDeclaration { property = CssPropertyBorder $ CssValueBorderTRBL
                                       { borderTRBLWidth = CssValueBorderWidthDistance (CssDistanceAbsPx 10.0)
                                       , borderTRBLStyle = CssValueBorderStyleInset
                                       , borderTRBLColor = CssValueBorderColor 0x00fff1
                                       }
                          , important = False
                          }
    )
  , ( "border: 10px inset #00fff1 !important",
      Just CssDeclaration { property = CssPropertyBorder $ CssValueBorderTRBL
                                       { borderTRBLWidth = CssValueBorderWidthDistance (CssDistanceAbsPx 10.0)
                                       , borderTRBLStyle = CssValueBorderStyleInset
                                       , borderTRBLColor = CssValueBorderColor 0x00fff1
                                       }
                          , important = True
                          }
    )

  , ( "border: 17px red",
      Just CssDeclaration { property = CssPropertyBorder $ CssValueBorderTRBL
                                       { borderTRBLWidth = CssValueBorderWidthDistance (CssDistanceAbsPx 17.0)
                                       , borderTRBLStyle = defaultBorderTRBLStyle
                                       , borderTRBLColor = CssValueBorderColor 0xff0000
                                       }
                          , important = False
                          }
    )

  , ( "border: outset rgb(0, 255, 0) 29em",
      Just CssDeclaration { property = CssPropertyBorder $ CssValueBorderTRBL
                                       { borderTRBLWidth = CssValueBorderWidthDistance (CssDistanceRelEm 29.0)
                                       , borderTRBLStyle = CssValueBorderStyleOutset
                                       , borderTRBLColor = CssValueBorderColor 0x00ff00
                                       }
                          , important = False
                          }
    )
  , ( "border: outset rgb(0, 255, 0) 29em !important",
      Just CssDeclaration { property = CssPropertyBorder $ CssValueBorderTRBL
                                       { borderTRBLWidth = CssValueBorderWidthDistance (CssDistanceRelEm 29.0)
                                       , borderTRBLStyle = CssValueBorderStyleOutset
                                       , borderTRBLColor = CssValueBorderColor 0x00ff00
                                       }
                          , important = True
                          }
    )

  , ( "border: dotted",
      Just CssDeclaration { property = CssPropertyBorder $ CssValueBorderTRBL
                                       { borderTRBLWidth = defaultBorderTRBLWidth
                                       , borderTRBLStyle = CssValueBorderStyleDotted
                                       , borderTRBLColor = defaultBorderTRBLColor
                                       }
                          , important = False
                          }
    )
  , ( "border: dotted !important",
      Just CssDeclaration { property = CssPropertyBorder $ CssValueBorderTRBL
                                       { borderTRBLWidth = defaultBorderTRBLWidth
                                       , borderTRBLStyle = CssValueBorderStyleDotted
                                       , borderTRBLColor = defaultBorderTRBLColor
                                       }
                          , important = True
                          }
    )

  -- Invalid case: no value, just "!important".
  , ( "border: !important",
      Nothing)
  -- Invalid case: nothing after the property name.
  , ( "border:",
      Nothing)
  -- Invalid case: just a space after the property name.
  , ( "border:",
      Nothing)
  -- Invalid case: invalid value.
  , ( "border: elephant",
      Nothing)




  , ( "border-collapse: separate",               Just CssDeclaration { property = CssPropertyBorderCollapse CssValueBorderCollapseSeparate,        important = False } )
  , ( "border-collapse: separate !important",    Just CssDeclaration { property = CssPropertyBorderCollapse CssValueBorderCollapseSeparate,        important = True  } )
  , ( "border-collapse: collapse",               Just CssDeclaration { property = CssPropertyBorderCollapse CssValueBorderCollapseCollapse,        important = False } )
  , ( "border-collapse: collapse !important",    Just CssDeclaration { property = CssPropertyBorderCollapse CssValueBorderCollapseCollapse,        important = True  } )

  -- Testing for parsing of bad css: invalid property name.
  , ( "border-colapse: block",                   Nothing)
  -- Testing for parsing of bad css: invalid value.
  , ( "border-collapse: #00ff00",                Nothing)
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "border-collapse: separate !importan",     Nothing)




  , ( "border-spacing:  1.5px !important",            Just CssDeclaration { property = CssPropertyBorderSpacing (CssValueBorderSpacingDistance (CssDistanceAbsPx  1.5)),  important = True  } )
  , ( "border-spacing:  2.0mm",                       Just CssDeclaration { property = CssPropertyBorderSpacing (CssValueBorderSpacingDistance (CssDistanceAbsMm  2.0)),  important = False } )
  , ( "border-spacing: 13.5em",                       Just CssDeclaration { property = CssPropertyBorderSpacing (CssValueBorderSpacingDistance (CssDistanceRelEm 13.5)),  important = False } )
  , ( "border-spacing: 44.0ex !important",            Just CssDeclaration { property = CssPropertyBorderSpacing (CssValueBorderSpacingDistance (CssDistanceRelEx 44.0)),  important = True  } )

  -- Testing for parsing of bad css: invalid property name.
  , ( "border_spacing: 52.0mm",                       Nothing)
  -- Testing for parsing of bad css: invalid property value.
  , ( "border-spacing: latin",                        Nothing)
    -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "border-spacing: 74.0ex !importan",             Nothing)



  , ( "border-top: 5mm solid red",            Just CssDeclaration { property = CssPropertyBorderTop CssValueBorderTRBL
                                                                    { borderTRBLWidth = CssValueBorderWidthDistance (CssDistanceAbsMm 5)
                                                                    , borderTRBLStyle = CssValueBorderStyleSolid
                                                                    , borderTRBLColor = CssValueBorderColor 0xff0000
                                                                    }
                                                                  , important = False
                                                                  }
    )
    -- This time with changed order of values (they can be unordered).
  , ( "border-top: solid red 5mm",            Just CssDeclaration { property = CssPropertyBorderTop CssValueBorderTRBL
                                                                    { borderTRBLWidth = CssValueBorderWidthDistance (CssDistanceAbsMm 5)
                                                                    , borderTRBLStyle = CssValueBorderStyleSolid
                                                                    , borderTRBLColor = CssValueBorderColor 0xff0000
                                                                    }
                                                                  , important = False
                                                                  }
    )
    -- This time with one of values omitted (default value should be used).
  , ( "border-top: solid red",                  Just CssDeclaration { property = CssPropertyBorderTop CssValueBorderTRBL
                                                                      { borderTRBLWidth = defaultBorderTRBLWidth
                                                                      , borderTRBLStyle = CssValueBorderStyleSolid
                                                                      , borderTRBLColor = CssValueBorderColor 0xff0000
                                                                      }
                                                                    , important = False
                                                                    }
    )

  , ( "border-right: 2mm dotted orange",      Just CssDeclaration { property = CssPropertyBorderRight CssValueBorderTRBL
                                                                    { borderTRBLWidth = CssValueBorderWidthDistance (CssDistanceAbsMm 2)
                                                                    , borderTRBLStyle = CssValueBorderStyleDotted
                                                                    , borderTRBLColor = CssValueBorderColor 0xffa500
                                                                    }
                                                                  , important = False
                                                                  }
    )

  , ( "border-bottom: 17px inherit rgb(255, 0, 255)",    Just CssDeclaration { property = CssPropertyBorderBottom CssValueBorderTRBL
                                                                               { borderTRBLWidth = CssValueBorderWidthDistance (CssDistanceAbsPx 17)
                                                                               , borderTRBLStyle = CssValueBorderStyleInherit
                                                                               , borderTRBLColor = CssValueBorderColor 0xff00ff
                                                                               }
                                                                             , important = False
                                                                             }
    )

  , ( "border-left: 20em none inherit",      Just CssDeclaration { property = CssPropertyBorderLeft CssValueBorderTRBL
                                                                   { borderTRBLWidth = CssValueBorderWidthDistance (CssDistanceRelEm 20)
                                                                   , borderTRBLStyle = CssValueBorderStyleNone
                                                                   , borderTRBLColor = CssValueBorderColorInherit
                                                                   }
                                                                 , important = False }
    )

  -- Invalid value; TODO: this test fails
  -- , ( "border-top: 5mm computer dashed",      Nothing)






    -- Single value of border-color is provided: top-righ-bottom-left.
  , ( "border-color: rgb(0, 0, 15)",
      Just CssDeclaration { property = CssPropertyBorderColor $ CssValueBorderColor'
                                       { borderColorTop    = CssValueBorderColor 0x00000f
                                       , borderColorRight  = CssValueBorderColor 0x00000f
                                       , borderColorBottom = CssValueBorderColor 0x00000f
                                       , borderColorLeft   = CssValueBorderColor 0x00000f
                                       }
                          , important = False }
    )
    -- Two values of border-color are provided: top-bottom / right-left.
  , ( "border-color: rgb(0, 15, 0) #ff0000",
      Just CssDeclaration { property = CssPropertyBorderColor $ CssValueBorderColor'
                                       { borderColorTop    = CssValueBorderColor 0x000f00
                                       , borderColorRight  = CssValueBorderColor 0xff0000
                                       , borderColorBottom = CssValueBorderColor 0x000f00
                                       , borderColorLeft   = CssValueBorderColor 0xff0000
                                       }
                          , important = False }
    )
    -- Three values of border-color are provided:  top / right-left / bottom..
  , ( "border-color: rgb(0, 15, 0) #ff0000 blue",
      Just CssDeclaration { property = CssPropertyBorderColor $ CssValueBorderColor'
                                       { borderColorTop    = CssValueBorderColor 0x000f00
                                       , borderColorRight  = CssValueBorderColor 0xff0000
                                       , borderColorBottom = CssValueBorderColor 0x0000ff
                                       , borderColorLeft   = CssValueBorderColor 0xff0000
                                       }
                          , important = False }
      )
    -- Four values of border-width are provided: top / right / bottom / left
  , ( "border-color: rgb(0, 15, 0) rgb(1, 15, 0) #012345 burlywood",
      Just CssDeclaration { property = CssPropertyBorderColor $ CssValueBorderColor'
                                       { borderColorTop    = CssValueBorderColor 0x000f00
                                       , borderColorRight  = CssValueBorderColor 0x010f00
                                       , borderColorBottom = CssValueBorderColor 0x012345
                                       , borderColorLeft   = CssValueBorderColor 0xdeb887
                                       }
                          , important = False }
    )
  -- TODO: add tests of failure cases for border-color.




  -- Single value of border-width is provided: top-righ-bottom-left.
  , ( "border-style: double",
      Just CssDeclaration { property = CssPropertyBorderStyle $ CssValueBorderStyle'
                                       { borderStyleTop    = CssValueBorderStyleDouble
                                       , borderStyleRight  = CssValueBorderStyleDouble
                                       , borderStyleBottom = CssValueBorderStyleDouble
                                       , borderStyleLeft   = CssValueBorderStyleDouble
                                       }
                          , important = False }
    )
  -- Two values of border-width are provided: top-bottom / right-left.
  , ( "border-style: none hidden",
      Just CssDeclaration { property = CssPropertyBorderStyle $ CssValueBorderStyle'
                                       { borderStyleTop    = CssValueBorderStyleNone
                                       , borderStyleRight  = CssValueBorderStyleHidden
                                       , borderStyleBottom = CssValueBorderStyleNone
                                       , borderStyleLeft   = CssValueBorderStyleHidden
                                       }
                          , important = False }
    )
  -- Three values of border-width are provided: top / right-left / bottom.
  , ( "border-style: none inherit groove",
      Just CssDeclaration { property = CssPropertyBorderStyle $ CssValueBorderStyle'
                                       { borderStyleTop    = CssValueBorderStyleNone
                                       , borderStyleRight  = CssValueBorderStyleInherit
                                       , borderStyleBottom = CssValueBorderStyleGroove
                                       , borderStyleLeft   = CssValueBorderStyleInherit
                                       }
                          , important = False }
    )
  -- Four values of border-width are provided: top / right / bottom / left
  , ( "border-style: outset inherit hidden none",
      Just CssDeclaration { property = CssPropertyBorderStyle $ CssValueBorderStyle'
                                       { borderStyleTop    = CssValueBorderStyleOutset
                                       , borderStyleRight  = CssValueBorderStyleInherit
                                       , borderStyleBottom = CssValueBorderStyleHidden
                                       , borderStyleLeft   = CssValueBorderStyleNone
                                       }
                          , important = False }
    )
  -- TODO: add tests of failure cases for border-style.




  -- Single value of border-width is provided: top-righ-bottom-left.
  , ( "border-width: 99px",
      Just CssDeclaration { property = CssPropertyBorderWidth $ CssValueBorderWidth'
                                       { borderWidthTop    = CssValueBorderWidthDistance (CssDistanceAbsPx 99)
                                       , borderWidthRight  = CssValueBorderWidthDistance (CssDistanceAbsPx 99)
                                       , borderWidthBottom = CssValueBorderWidthDistance (CssDistanceAbsPx 99)
                                       , borderWidthLeft   = CssValueBorderWidthDistance (CssDistanceAbsPx 99)
                                       }
                          , important = False }
    )
  -- Two values of border-width are provided: top-bottom / right-left.
  , ( "border-width: 88px 77mm",
      Just CssDeclaration { property = CssPropertyBorderWidth $ CssValueBorderWidth'
                                       { borderWidthTop    = CssValueBorderWidthDistance (CssDistanceAbsPx 88)
                                       , borderWidthRight  = CssValueBorderWidthDistance (CssDistanceAbsMm 77)
                                       , borderWidthBottom = CssValueBorderWidthDistance (CssDistanceAbsPx 88)
                                       , borderWidthLeft   = CssValueBorderWidthDistance (CssDistanceAbsMm 77)
                                       }
                          , important = False }
    )
  -- Three values of border-width are provided: top / right-left / bottom.
  , ( "border-width: 66px 55em 44px",
      Just CssDeclaration { property = CssPropertyBorderWidth $ CssValueBorderWidth'
                                       { borderWidthTop    = CssValueBorderWidthDistance (CssDistanceAbsPx 66)
                                       , borderWidthRight  = CssValueBorderWidthDistance (CssDistanceRelEm 55)
                                       , borderWidthBottom = CssValueBorderWidthDistance (CssDistanceAbsPx 44)
                                       , borderWidthLeft   = CssValueBorderWidthDistance (CssDistanceRelEm 55)
                                       }
                          , important = False }
    )
  -- Four values of border-width are provided: top / right / bottom / left
  --
  -- TODO: the parser doesn't handle a value of property that includes
  -- "auto". Replace "thin" with "auto" and see what happens: the parser will
  -- tell you that it sees only three values of distance.
  , ( "border-width: 21px 32em 43ex thin",
      Just CssDeclaration { property = CssPropertyBorderWidth $ CssValueBorderWidth'
                                       { borderWidthTop    = CssValueBorderWidthDistance (CssDistanceAbsPx 21)
                                       , borderWidthRight  = CssValueBorderWidthDistance (CssDistanceRelEm 32)
                                       , borderWidthBottom = CssValueBorderWidthDistance (CssDistanceRelEx 43)
                                       , borderWidthLeft   = CssValueBorderWidthThin -- CssValueBorderWidthDistance CssDistanceAuto)
                                       }
                          , important = False }
    )
  -- TODO: add tests of failure cases for border-width.




  , ( "border-top-color: inherit",                        Just CssDeclaration { property = CssPropertyBorderTopColor   CssValueBorderColorInherit,       important = False } )
  , ( "border-top-color: transparent",                    Just CssDeclaration { property = CssPropertyBorderTopColor   CssValueBorderColorTransparent,   important = False } )
  , ( "border-top-color: red",                            Just CssDeclaration { property = CssPropertyBorderTopColor   $ CssValueBorderColor 0xff0000,   important = False } )
  , ( "border-top-color: #0000ff !important",             Just CssDeclaration { property = CssPropertyBorderTopColor   $ CssValueBorderColor 0x0000ff,   important = True  } )
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "border-top-color: #0000ff !iportant",              Nothing)

  , ( "border-right-color: inherit",                      Just CssDeclaration { property = CssPropertyBorderRightColor CssValueBorderColorInherit,       important = False } )
  , ( "border-right-color: transparent",                  Just CssDeclaration { property = CssPropertyBorderRightColor CssValueBorderColorTransparent,   important = False } )
  , ( "border-right-color: lime !important",              Just CssDeclaration { property = CssPropertyBorderRightColor $ CssValueBorderColor 0x00ff00,   important = True  } )
  , ( "border-right-color: rgb(255, 0, 0) !important",    Just CssDeclaration { property = CssPropertyBorderRightColor $ CssValueBorderColor 0xff0000,   important = True  } )
   -- Testing for parsing of bad css: space after function name.
  , ( "border-right-color: rgb (255, 0, 0) !important",   Nothing)

  , ( "border-bottom-color: inherit !important",          Just CssDeclaration { property = CssPropertyBorderBottomColor CssValueBorderColorInherit,      important = True  } )
  , ( "border-bottom-color: transparent",                 Just CssDeclaration { property = CssPropertyBorderBottomColor CssValueBorderColorTransparent,  important = False } )
  , ( "border-bottom-color: pink",                        Just CssDeclaration { property = CssPropertyBorderBottomColor $ CssValueBorderColor 0xffc0cb,  important = False } )
  , ( "border-bottom-color: rgb(0, 255, 0) !important",   Just CssDeclaration { property = CssPropertyBorderBottomColor $ CssValueBorderColor 0x00ff00,  important = True  } )
    -- Testing for parsing of bad css: typo in property name.
  , ( "border-bottom_color: rgb(0, 255, 0) !important",   Nothing)

  , ( "border-left-color: inherit",                       Just CssDeclaration { property = CssPropertyBorderLeftColor CssValueBorderColorInherit,        important = False } )
  , ( "border-left-color: transparent !important",        Just CssDeclaration { property = CssPropertyBorderLeftColor CssValueBorderColorTransparent,    important = True  } )
  , ( "border-left-color: purple",                        Just CssDeclaration { property = CssPropertyBorderLeftColor $ CssValueBorderColor 0x800080,    important = False } )
  , ( "border-left-color: rgb(0, 0, 255) !important",     Just CssDeclaration { property = CssPropertyBorderLeftColor $ CssValueBorderColor 0x0000ff,    important = True  } )
    -- Testing for parsing of bad css: invalid value name.
  , ( "border-left-color: purpe",                         Nothing)




  , ( "border-top-style: none !important",       Just CssDeclaration { property = CssPropertyBorderTopStyle CssValueBorderStyleNone,          important = True  } )
  , ( "border-top-style: hidden",                Just CssDeclaration { property = CssPropertyBorderTopStyle CssValueBorderStyleHidden,        important = False } )
  , ( "border-top-style: dotted !important",     Just CssDeclaration { property = CssPropertyBorderTopStyle CssValueBorderStyleDotted,        important = True  } )
  , ( "border-top-style: dashed",                Just CssDeclaration { property = CssPropertyBorderTopStyle CssValueBorderStyleDashed,        important = False } )
  , ( "border-top-style: solid !important",      Just CssDeclaration { property = CssPropertyBorderTopStyle CssValueBorderStyleSolid,         important = True  } )
  , ( "border-top-style: double",                Just CssDeclaration { property = CssPropertyBorderTopStyle CssValueBorderStyleDouble,        important = False } )
  , ( "border-top-style: groove !important",     Just CssDeclaration { property = CssPropertyBorderTopStyle CssValueBorderStyleGroove,        important = True  } )
  , ( "border-top-style: ridge",                 Just CssDeclaration { property = CssPropertyBorderTopStyle CssValueBorderStyleRidge,         important = False } )
  , ( "border-top-style: inset !important",      Just CssDeclaration { property = CssPropertyBorderTopStyle CssValueBorderStyleInset,         important = True  } )
  , ( "border-top-style: outset",                Just CssDeclaration { property = CssPropertyBorderTopStyle CssValueBorderStyleOutset,        important = False } )
  , ( "border-top-style: inherit !important",    Just CssDeclaration { property = CssPropertyBorderTopStyle CssValueBorderStyleInherit,       important = True  } )
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "border-top-style: inherit !mportant",     Nothing)

  , ( "border-right-style: none",                Just CssDeclaration { property = CssPropertyBorderRightStyle CssValueBorderStyleNone,        important = False } )
  , ( "border-right-style: hidden !important",   Just CssDeclaration { property = CssPropertyBorderRightStyle CssValueBorderStyleHidden,      important = True  } )
  , ( "border-right-style: dotted",              Just CssDeclaration { property = CssPropertyBorderRightStyle CssValueBorderStyleDotted,      important = False } )
  , ( "border-right-style: dashed !important",   Just CssDeclaration { property = CssPropertyBorderRightStyle CssValueBorderStyleDashed,      important = True  } )
  , ( "border-right-style: solid",               Just CssDeclaration { property = CssPropertyBorderRightStyle CssValueBorderStyleSolid,       important = False } )
  , ( "border-right-style: double !important",   Just CssDeclaration { property = CssPropertyBorderRightStyle CssValueBorderStyleDouble,      important = True  } )
  , ( "border-right-style: groove",              Just CssDeclaration { property = CssPropertyBorderRightStyle CssValueBorderStyleGroove,      important = False } )
  , ( "border-right-style: ridge !important",    Just CssDeclaration { property = CssPropertyBorderRightStyle CssValueBorderStyleRidge,       important = True  } )
  , ( "border-right-style: inset",               Just CssDeclaration { property = CssPropertyBorderRightStyle CssValueBorderStyleInset,       important = False } )
  , ( "border-right-style: outset !important",   Just CssDeclaration { property = CssPropertyBorderRightStyle CssValueBorderStyleOutset,      important = True  } )
  , ( "border-right-style: inherit",             Just CssDeclaration { property = CssPropertyBorderRightStyle CssValueBorderStyleInherit,     important = False } )
  -- Testing for parsing of bad css: invalid value.
  , ( "border-right-style: blue",                Nothing)

  , ( "border-bottom-style: none !important",    Just CssDeclaration { property = CssPropertyBorderBottomStyle CssValueBorderStyleNone,       important = True  } )
  , ( "border-bottom-style: hidden",             Just CssDeclaration { property = CssPropertyBorderBottomStyle CssValueBorderStyleHidden,     important = False } )
  , ( "border-bottom-style: dotted !important",  Just CssDeclaration { property = CssPropertyBorderBottomStyle CssValueBorderStyleDotted,     important = True  } )
  , ( "border-bottom-style: dashed",             Just CssDeclaration { property = CssPropertyBorderBottomStyle CssValueBorderStyleDashed,     important = False } )
  , ( "border-bottom-style: solid !important",   Just CssDeclaration { property = CssPropertyBorderBottomStyle CssValueBorderStyleSolid,      important = True  } )
  , ( "border-bottom-style: double",             Just CssDeclaration { property = CssPropertyBorderBottomStyle CssValueBorderStyleDouble,     important = False } )
  , ( "border-bottom-style: groove !important",  Just CssDeclaration { property = CssPropertyBorderBottomStyle CssValueBorderStyleGroove,     important = True  } )
  , ( "border-bottom-style: ridge",              Just CssDeclaration { property = CssPropertyBorderBottomStyle CssValueBorderStyleRidge,      important = False } )
  , ( "border-bottom-style: inset !important",   Just CssDeclaration { property = CssPropertyBorderBottomStyle CssValueBorderStyleInset,      important = True  } )
  , ( "border-bottom-style: outset",             Just CssDeclaration { property = CssPropertyBorderBottomStyle CssValueBorderStyleOutset,     important = False } )
  , ( "border-bottom-style: inherit !important", Just CssDeclaration { property = CssPropertyBorderBottomStyle CssValueBorderStyleInherit,    important = True  } )
  -- Testing for parsing of bad css: typo in property name.
  , ( "order-bottom-style: inherit !important",  Nothing)

  , ( "border-left-style: none",                 Just CssDeclaration { property = CssPropertyBorderLeftStyle CssValueBorderStyleNone,         important = False } )
  , ( "border-left-style: hidden !important",    Just CssDeclaration { property = CssPropertyBorderLeftStyle CssValueBorderStyleHidden,       important = True  } )
  , ( "border-left-style: dotted",               Just CssDeclaration { property = CssPropertyBorderLeftStyle CssValueBorderStyleDotted,       important = False } )
  , ( "border-left-style: dashed !important",    Just CssDeclaration { property = CssPropertyBorderLeftStyle CssValueBorderStyleDashed,       important = True  } )
  , ( "border-left-style: solid",                Just CssDeclaration { property = CssPropertyBorderLeftStyle CssValueBorderStyleSolid,        important = False } )
  , ( "border-left-style: double !important",    Just CssDeclaration { property = CssPropertyBorderLeftStyle CssValueBorderStyleDouble,       important = True  } )
  , ( "border-left-style: groove",               Just CssDeclaration { property = CssPropertyBorderLeftStyle CssValueBorderStyleGroove,       important = False } )
  , ( "border-left-style: ridge !important",     Just CssDeclaration { property = CssPropertyBorderLeftStyle CssValueBorderStyleRidge,        important = True  } )
  , ( "border-left-style: inset",                Just CssDeclaration { property = CssPropertyBorderLeftStyle CssValueBorderStyleInset,        important = False } )
  , ( "border-left-style: outset !important",    Just CssDeclaration { property = CssPropertyBorderLeftStyle CssValueBorderStyleOutset,       important = True  } )
  , ( "border-left-style: inherit",              Just CssDeclaration { property = CssPropertyBorderLeftStyle CssValueBorderStyleInherit,      important = False } )
  -- Testing for parsing of bad css: invalid value name.
  , ( "border-left-style: inheri !important",     Nothing)




  , ( "border-top-width: inherit",                        Just CssDeclaration { property = CssPropertyBorderTopWidth CssValueBorderWidthInherit,                               important = False } )
  , ( "border-top-width: 1.0px",                          Just CssDeclaration { property = CssPropertyBorderTopWidth (CssValueBorderWidthDistance (CssDistanceAbsPx 1.0)),     important = False } )
  , ( "border-top-width: 2.0mm !important",               Just CssDeclaration { property = CssPropertyBorderTopWidth (CssValueBorderWidthDistance (CssDistanceAbsMm 2.0)),     important = True  } )
  -- Testing for parsing of bad css: invalid value.
  , ( "border-top-width: I.0px",                          Nothing)

  , ( "border-right-width: inherit",                      Just CssDeclaration { property = CssPropertyBorderRightWidth CssValueBorderWidthInherit,                             important = False } )
  , ( "border-right-width: 1.5px !important",             Just CssDeclaration { property = CssPropertyBorderRightWidth (CssValueBorderWidthDistance (CssDistanceAbsPx 1.5)),   important = True  } )
  , ( "border-right-width: 2.0mm",                        Just CssDeclaration { property = CssPropertyBorderRightWidth (CssValueBorderWidthDistance (CssDistanceAbsMm 2.0)),   important = False } )
  -- Testing for parsing of bad css: invalid property name.
  , ( "border-rigth-width: 2.0mm",                        Nothing)

  , ( "border-bottom-width: inherit !important",          Just CssDeclaration { property = CssPropertyBorderBottomWidth CssValueBorderWidthInherit,                            important = True  } )
  , ( "border-bottom-width: 1.0em",                       Just CssDeclaration { property = CssPropertyBorderBottomWidth (CssValueBorderWidthDistance (CssDistanceRelEm 1.0)),  important = False } )
  , ( "border-bottom-width: 2.0ex !important",            Just CssDeclaration { property = CssPropertyBorderBottomWidth (CssValueBorderWidthDistance (CssDistanceRelEx 2.0)),  important = True  } )
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "border-bottom-width: 2.0ex !importan",             Nothing)

  , ( "border-left-width: inherit",                       Just CssDeclaration { property = CssPropertyBorderLeftWidth CssValueBorderWidthInherit,                              important = False } )
  , ( "border-left-width: 1.0em",                         Just CssDeclaration { property = CssPropertyBorderLeftWidth (CssValueBorderWidthDistance (CssDistanceRelEm 1.0)),    important = False } )
  , ( "border-left-width: 2.0ex !important",              Just CssDeclaration { property = CssPropertyBorderLeftWidth (CssValueBorderWidthDistance (CssDistanceRelEx 2.0)),    important = True  } )
  -- Testing for parsing of bad css: invalid value.
  , ( "border-left-width: anherit",                       Nothing)




  , ( "color: inherit",                          Just CssDeclaration { property = CssPropertyColor CssValueColorInherit,        important = False } )
  , ( "color: inherit !important",               Just CssDeclaration { property = CssPropertyColor CssValueColorInherit,        important = True  } )
  , ( "color: red",                              Just CssDeclaration { property = CssPropertyColor (CssValueColor 0xff0000),    important = False } )
  , ( "color: lime !important",                  Just CssDeclaration { property = CssPropertyColor (CssValueColor 0x00ff00),    important = True  } ) -- Yes, "lime" not "green".
  , ( "color: blue !important;",                 Just CssDeclaration { property = CssPropertyColor (CssValueColor 0x0000ff),    important = True  } )
  , ( "color: #abcdef;",                         Just CssDeclaration { property = CssPropertyColor (CssValueColor 0xabcdef),    important = False } )




    -- For now only quoted strings are supported (with single or double quotes).
  , ( "content: \"\"",                           Just CssDeclaration { property = CssPropertyContent (CssValueContent ""),        important = False } )
  , ( "content: \"\" !important",                Just CssDeclaration { property = CssPropertyContent (CssValueContent ""),        important = True } )
  , ( "content: \"bullet\"",                     Just CssDeclaration { property = CssPropertyContent (CssValueContent "bullet"),  important = False } )
  , ( "content: \"bullet\" !important",          Just CssDeclaration { property = CssPropertyContent (CssValueContent "bullet"),  important = True } )
  , ( "content: \"train\"",                      Just CssDeclaration { property = CssPropertyContent (CssValueContent "train"),   important = False } )
  , ( "content: \"train\" !important",           Just CssDeclaration { property = CssPropertyContent (CssValueContent "train"),   important = True } )
  , ( "content: ''",                             Just CssDeclaration { property = CssPropertyContent (CssValueContent ""),        important = False } )
  , ( "content: '' !important",                  Just CssDeclaration { property = CssPropertyContent (CssValueContent ""),        important = True } )
  , ( "content: 'bus'",                          Just CssDeclaration { property = CssPropertyContent (CssValueContent "bus"),     important = False } )
  , ( "content: 'bus' !important",               Just CssDeclaration { property = CssPropertyContent (CssValueContent "bus"),     important = True } )
  , ( "content: 'car'",                          Just CssDeclaration { property = CssPropertyContent (CssValueContent "car"),     important = False } )
  , ( "content: 'car' !important",               Just CssDeclaration { property = CssPropertyContent (CssValueContent "car"),     important = True } )
    -- Testing for parsing of bad css: invalid property name.
  , ( "contet: \"bullet\"",                      Nothing)
    -- Testing for parsing of bad css: invalid value.
  , ( "content: train",                          Nothing)
    -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "content: \"bullet\" !improtant",          Nothing)
  , ( "content: 'train' !improtant",             Nothing)




  , ( "cursor: crosshair",            Just CssDeclaration { property = CssPropertyCursor CssValueCursorCrosshair,   important = False } )
  , ( "cursor: default !important",   Just CssDeclaration { property = CssPropertyCursor CssValueCursorDefault,     important = True  } )
  , ( "cursor: pointer",              Just CssDeclaration { property = CssPropertyCursor CssValueCursorPointer,     important = False } )
  , ( "cursor: move !important",      Just CssDeclaration { property = CssPropertyCursor CssValueCursorMove,        important = True  } )
  , ( "cursor: e-resize",             Just CssDeclaration { property = CssPropertyCursor CssValueCursorEResize,     important = False } )
  , ( "cursor: ne-resize !important", Just CssDeclaration { property = CssPropertyCursor CssValueCursorNeResize,    important = True  } )
  , ( "cursor: nw-resize",            Just CssDeclaration { property = CssPropertyCursor CssValueCursorNwResize,    important = False } )
  , ( "cursor: n-resize !important",  Just CssDeclaration { property = CssPropertyCursor CssValueCursorNResize,     important = True  } )
  , ( "cursor: se-resize",            Just CssDeclaration { property = CssPropertyCursor CssValueCursorSeResize,    important = False } )
  , ( "cursor: sw-resize !important", Just CssDeclaration { property = CssPropertyCursor CssValueCursorSwResize,    important = True  } )
  , ( "cursor: s-resize",             Just CssDeclaration { property = CssPropertyCursor CssValueCursorSResize,     important = False } )
  , ( "cursor: w-resize !important",  Just CssDeclaration { property = CssPropertyCursor CssValueCursorWResize,     important = True  } )
  , ( "cursor: text",                 Just CssDeclaration { property = CssPropertyCursor CssValueCursorText,        important = False } )
  , ( "cursor: wait !important",      Just CssDeclaration { property = CssPropertyCursor CssValueCursorWait,        important = True  } )
  , ( "cursor: help",                 Just CssDeclaration { property = CssPropertyCursor CssValueCursorHelp,        important = False } )
  -- Testing for parsing of bad css: invalid property name.
  , ( "cursr: crosshair",             Nothing)
  -- Testing for parsing of bad css: invalid value.
  , ( "cursor: ponter",               Nothing)
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "cursor: help !improtant",      Nothing)





  , ( "display: block",                         Just CssDeclaration { property = CssPropertyDisplay CssValueDisplayBlock,              important = False } )
  , ( "display: inline !important",             Just CssDeclaration { property = CssPropertyDisplay CssValueDisplayInline,             important = True  } )
  , ( "display: inline-block",                  Just CssDeclaration { property = CssPropertyDisplay CssValueDisplayInlineBlock,        important = False } )
  , ( "display: list-item !important",          Just CssDeclaration { property = CssPropertyDisplay CssValueDisplayListItem,           important = True  } )
  , ( "display: none",                          Just CssDeclaration { property = CssPropertyDisplay CssValueDisplayNone,               important = False } )
  , ( "display: table !important",              Just CssDeclaration { property = CssPropertyDisplay CssValueDisplayTable,              important = True  } )
  , ( "display: table-row-group",               Just CssDeclaration { property = CssPropertyDisplay CssValueDisplayTableRowGroup,      important = False } )
  , ( "display: table-header-group !important", Just CssDeclaration { property = CssPropertyDisplay CssValueDisplayTableHeaderGroup,   important = True  } )
  , ( "display: table-footer-group",            Just CssDeclaration { property = CssPropertyDisplay CssValueDisplayTableFooterGroup,   important = False } )
  , ( "display: table-row !important",          Just CssDeclaration { property = CssPropertyDisplay CssValueDisplayTableRow,           important = True  } )
  , ( "display: table-cell",                    Just CssDeclaration { property = CssPropertyDisplay CssValueDisplayTableCell,          important = False } )
  -- Testing for parsing of bad css: invalid property name.
  , ( "dsiplay: block",                         Nothing)
  -- Testing for parsing of bad css: invalid value.
  , ( "display: rgb(0, 100, 200)",              Nothing)
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "display: table !improtant",              Nothing)




    -- CSS2.2: [ [ <'font-style'> || <'font-variant'> || <'font-weight'> ]? <'font-size'> [ / <'line-height'> ]? <'font-family'> ]
    --         | caption | icon | menu | message-box | small-caption | status-bar | inherit
  , ("font: italic small-caps 8px serif",       Just CssDeclaration { property = CssPropertyFont
                                                                      $ CssValueFont [ CssPropertyFontStyle CssValueFontStyleItalic
                                                                                     , CssPropertyFontVariant CssValueFontVariantSmallCaps
                                                                                     , CssPropertyFontSize $ CssValueFontSizeDistance $ CssDistanceAbsPx 8.0
                                                                                     , CssPropertyFontFamily $ CssValueFontFamilyList ["serif"]
                                                                                     ]
                                                                    , important = False
                                                                    }
    )

    -- Absolute required minimum: font-size and font-family
  , ("font: 8px monospace",                     Just CssDeclaration { property = CssPropertyFont
                                                                      $ CssValueFont [ CssPropertyFontSize $ CssValueFontSizeDistance $ CssDistanceAbsPx 8.0
                                                                                     , CssPropertyFontFamily $ CssValueFontFamilyList ["monospace"]
                                                                                     ]
                                                                    , important = False
                                                                    }
    )

    -- Invalid input: font-family is missing
  , ("font: small-caps 8px",                    Nothing)


  -- TODO: because tokensAsValueStringList is too eager and consumes
  -- space-separated idents as if they were a part of a list, this test
  -- fails. "monospace inherit" is treated as two-items list. The
  -- tokensAsValueStringList function should treat a list of *comma*
  -- separated list of idents as a list.
{-
    -- Invalid input: absolute required minimum (font-size and font-family), but followed by 'inherit' that is exclusive with size/family.
  , ("font: 8px monospace inherit",             Nothing)
-}



    -- TODO: "!important" keyword is not parsed correctly, fix it.
    -- TODO: rules for font-family are compilcated, but we don't support them well, fix it.
  , ("font-family: monospace",                  Just CssDeclaration { property = CssPropertyFontFamily $ CssValueFontFamilyList ["monospace"],             important = False } )
  , ("font-family: \"Comic Sans\", serif",      Just CssDeclaration { property = CssPropertyFontFamily $ CssValueFontFamilyList ["Comic Sans", "serif"],   important = False } )
  , ("font-family: 'My Font', cursive",         Just CssDeclaration { property = CssPropertyFontFamily $ CssValueFontFamilyList ["My Font", "cursive"],    important = False } )
  -- Testing for parsing of bad css: invalid property name.
  , ( "foht-family: monospace",                 Nothing)




  , ("font-size: xx-small",            Just CssDeclaration { property = CssPropertyFontSize CssValueFontSizeXXSmall,   important = False } )
  , ("font-size: x-small !important",  Just CssDeclaration { property = CssPropertyFontSize CssValueFontSizeXSmall,    important = True  } )
  , ("font-size: small",               Just CssDeclaration { property = CssPropertyFontSize CssValueFontSizeSmall,     important = False } )
  , ("font-size: medium !important",   Just CssDeclaration { property = CssPropertyFontSize CssValueFontSizeMedium,    important = True  } )
  , ("font-size: large",               Just CssDeclaration { property = CssPropertyFontSize CssValueFontSizeLarge,     important = False } )
  , ("font-size: x-large !important",  Just CssDeclaration { property = CssPropertyFontSize CssValueFontSizeXLarge,    important = True  } )
  , ("font-size: xx-large",            Just CssDeclaration { property = CssPropertyFontSize CssValueFontSizeXXLarge,   important = False } )
  , ("font-size: larger !important",   Just CssDeclaration { property = CssPropertyFontSize CssValueFontSizeLarger,    important = True  } )
  , ("font-size: smaller",             Just CssDeclaration { property = CssPropertyFontSize CssValueFontSizeSmaller,   important = False } )
  -- Testing for parsing of bad css: invalid property name.
  , ( "font-site: small",              Nothing)
  -- Testing for parsing of bad css: invalid value.
  , ( "font-size: square",             Nothing)
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "font-size: large important",    Nothing)




  , ("font-style: normal !important",    Just CssDeclaration { property = CssPropertyFontStyle CssValueFontStyleNormal,   important = True  } )
  , ("font-style: italic",               Just CssDeclaration { property = CssPropertyFontStyle CssValueFontStyleItalic,   important = False } )
  , ("font-style: oblique !important",   Just CssDeclaration { property = CssPropertyFontStyle CssValueFontStyleOblique,  important = True  } )
    -- Testing for parsing of bad css: invalid property name.
  , ( "font-syle: normal",               Nothing)
  -- Testing for parsing of bad css: invalid value.
  , ( "font-style: obligue",             Nothing)
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "font-style: normal !!important",  Nothing)




  , ("font-variant: normal !important",    Just CssDeclaration { property = CssPropertyFontVariant CssValueFontVariantNormal,      important = True  } )
  , ("font-variant: small-caps",           Just CssDeclaration { property = CssPropertyFontVariant CssValueFontVariantSmallCaps,   important = False } )
    -- Testing for parsing of bad css: invalid property name.
  , ( "font-wariant: normal",              Nothing)
  -- Testing for parsing of bad css: invalid value.
  , ( "font-variant: xx-large",            Nothing)
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "font-variant: normal !_mportant",   Nothing)




  , ("font-weight: normal",             Just CssDeclaration { property = CssPropertyFontWeight CssValueFontWeightNormal,     important = False } )
  , ("font-weight: bold !important",    Just CssDeclaration { property = CssPropertyFontWeight CssValueFontWeightBold,       important = True  } )
  , ("font-weight: bolder",             Just CssDeclaration { property = CssPropertyFontWeight CssValueFontWeightBolder,     important = False } )
  , ("font-weight: lighter !important", Just CssDeclaration { property = CssPropertyFontWeight CssValueFontWeightLighter,    important = True  } )
  , ("font-weight: 100 !important",     Just CssDeclaration { property = CssPropertyFontWeight $ CssValueFontWeightInt 100,  important = True  } )
  , ("font-weight: 900",                Just CssDeclaration { property = CssPropertyFontWeight $ CssValueFontWeightInt 900,  important = False } )

    -- Testing for parsing of bad css: invalid property name.
  , ("font-weigth: bold",               Nothing)
    -- Testing for parsing of bad css: invalid value.
  , ("font-weight: light",              Nothing)
  , ("font-weight: 1200",               Nothing)
    -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "font-weight: normal !_mportant", Nothing)




  , ("height: auto",                     Just CssDeclaration { property = CssPropertyHeight . CssValueHeightDistance $ CssDistanceAuto,                    important = False } )
  , ("height: auto !important",          Just CssDeclaration { property = CssPropertyHeight . CssValueHeightDistance $ CssDistanceAuto,                    important = True  } )
  , ("height:   1px",                    Just CssDeclaration { property = CssPropertyHeight (CssValueHeightDistance (CssDistanceAbsPx   1.0)),             important = False } )
  , ("height:   1px !important",         Just CssDeclaration { property = CssPropertyHeight (CssValueHeightDistance (CssDistanceAbsPx   1.0)),             important = True  } )
  , ("height:  22.22mm",                 Just CssDeclaration { property = CssPropertyHeight (CssValueHeightDistance (CssDistanceAbsMm  22.22)),            important = False } )
  , ("height:  22.22mm !important",      Just CssDeclaration { property = CssPropertyHeight (CssValueHeightDistance (CssDistanceAbsMm  22.22)),            important = True  } )
  , ("height:  33.3em",                  Just CssDeclaration { property = CssPropertyHeight (CssValueHeightDistance (CssDistanceRelEm  33.3)),             important = False } )
  , ("height:  33.3em !important",       Just CssDeclaration { property = CssPropertyHeight (CssValueHeightDistance (CssDistanceRelEm  33.3)),             important = True  } )
  , ("height: 444.44ex",                 Just CssDeclaration { property = CssPropertyHeight (CssValueHeightDistance (CssDistanceRelEx 444.44)),            important = False } )
  , ("height: 444.44ex !important",      Just CssDeclaration { property = CssPropertyHeight (CssValueHeightDistance (CssDistanceRelEx 444.44)),            important = True  } )

    -- Testing for parsing of bad css: invalid property name.
  , ("heigth:  77.7em",                  Nothing)
    -- Testing for parsing of bad css: invalid value.
  , ("height:  left",                    Nothing)
    -- TODO: per CSS2.2 negative values are invalid. Fix this case in parser.
  , ("height: -500.0mm",                 Just CssDeclaration { property = CssPropertyHeight (CssValueHeightDistance (CssDistanceAbsMm (-500.00))),         important = False } )
    -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ("height:  22.22mm !importat",       Nothing)




  , ("letter-spacing: normal",             Just CssDeclaration { property = CssPropertyLetterSpacing CssValueLetterSpacingNormal,                                 important = False } )
  , ("letter-spacing: normal !important",  Just CssDeclaration { property = CssPropertyLetterSpacing CssValueLetterSpacingNormal,                                 important = True  } )
  , ("letter-spacing: 10px",               Just CssDeclaration { property = CssPropertyLetterSpacing (CssValueLetterSpacingDistance (CssDistanceAbsPx 10.0)),     important = False } )
  , ("letter-spacing: 10px !important",    Just CssDeclaration { property = CssPropertyLetterSpacing (CssValueLetterSpacingDistance (CssDistanceAbsPx 10.0)),     important = True  } )
  , ("letter-spacing: -10px",              Just CssDeclaration { property = CssPropertyLetterSpacing (CssValueLetterSpacingDistance (CssDistanceAbsPx (-10.0))),  important = False } )
  , ("letter-spacing: -10px !important",   Just CssDeclaration { property = CssPropertyLetterSpacing (CssValueLetterSpacingDistance (CssDistanceAbsPx (-10.0))),  important = True  } )
  , ("letter-spacing: 5em",                Just CssDeclaration { property = CssPropertyLetterSpacing (CssValueLetterSpacingDistance (CssDistanceRelEm 5.0)),      important = False } )
  , ("letter-spacing: 5em !important",     Just CssDeclaration { property = CssPropertyLetterSpacing (CssValueLetterSpacingDistance (CssDistanceRelEm 5.0)),      important = True  } )

  -- Testing for parsing of bad css: invalid property name.
  , ("leter-spacing: normal",              Nothing)
  -- Testing for parsing of bad css: invalid value.
  , ("letter-spacing: bold",               Nothing)
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ("letter-spacing: normal !_omportant",  Nothing)




  , ("line-height: normal",             Just CssDeclaration { property = CssPropertyLineHeight CssValueLineHeightNormal,                                 important = False } )
  , ("line-height: normal !important",  Just CssDeclaration { property = CssPropertyLineHeight CssValueLineHeightNormal,                                 important = True  } )
  , ("line-height: 10px",               Just CssDeclaration { property = CssPropertyLineHeight (CssValueLineHeightDistance (CssDistanceAbsPx 10.0)),     important = False } )
  , ("line-height: 10px !important",    Just CssDeclaration { property = CssPropertyLineHeight (CssValueLineHeightDistance (CssDistanceAbsPx 10.0)),     important = True  } )
  , ("line-height: -10px",              Just CssDeclaration { property = CssPropertyLineHeight (CssValueLineHeightDistance (CssDistanceAbsPx (-10.0))),  important = False } )
  , ("line-height: -10px !important",   Just CssDeclaration { property = CssPropertyLineHeight (CssValueLineHeightDistance (CssDistanceAbsPx (-10.0))),  important = True  } )
  , ("line-height: 5em",                Just CssDeclaration { property = CssPropertyLineHeight (CssValueLineHeightDistance (CssDistanceRelEm 5.0)),      important = False } )
  , ("line-height: 5em !important",     Just CssDeclaration { property = CssPropertyLineHeight (CssValueLineHeightDistance (CssDistanceRelEm 5.0)),      important = True  } )

  -- Testing for parsing of bad css: invalid property name.
  , ("line-heigth: normal",             Nothing)
  -- Testing for parsing of bad css: invalid value.
  , ("line-height: bold",               Nothing)
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ("line-height: normal important",   Nothing)




    -- Notice that list-style-image is not supported by the parser and is not tested here.

{- -- This test item is disabled until a proper support for single "none" value is implemented.
  , ( "list-style: none",                    Just CssDeclaration { property = CssPropertyListStyle $ CssValueListStyle
                                                                { listStyleType     = initialValueListStyleType
                                                                , listStylePosition = initialValueListStylePosition
                                                                , listStyleImage    = initialValueListStyleImage
                                                                }
                                                              , important = False
                                                              }
                                             )
-}
  , ( "list-style: disc inside",             Just CssDeclaration { property = CssPropertyListStyle $ CssValueListStyle
                                                                   { listStyleType     = CssValueListStyleTypeDisc
                                                                   , listStylePosition = CssValueListStylePositionInside
                                                                   , listStyleImage    = initialValueListStyleImage
                                                                   }
                                                                 , important = False
                                                                 }
    )
  , ( "list-style: outside",                 Just CssDeclaration { property = CssPropertyListStyle $ CssValueListStyle
                                                                   { listStyleType     = initialValueListStyleType
                                                                   , listStylePosition = CssValueListStylePositionOutside
                                                                   , listStyleImage    = initialValueListStyleImage
                                                                   }
                                                                 , important = False
                                                                 }
    )
  , ( "list-style: upper-roman outside",     Just CssDeclaration { property = CssPropertyListStyle $ CssValueListStyle
                                                                   { listStyleType     = CssValueListStyleTypeUpperRoman
                                                                   , listStylePosition = CssValueListStylePositionOutside
                                                                   , listStyleImage    = initialValueListStyleImage
                                                                   }
                                                                 , important = False
                                                                 }
    )
    -- Same as above, but with flipped css value tokens.
  , ( "list-style: outside upper-roman",     Just CssDeclaration { property = CssPropertyListStyle $ CssValueListStyle
                                                                   { listStyleType     = CssValueListStyleTypeUpperRoman
                                                                   , listStylePosition = CssValueListStylePositionOutside
                                                                   , listStyleImage    = initialValueListStyleImage
                                                                   }
                                                                 , important = False
                                                                 }
    )



  , ( "list-style-position: inside",                    Just CssDeclaration { property = CssPropertyListStylePosition CssValueListStylePositionInside,   important = False } )
  , ( "list-style-position: inside !important",         Just CssDeclaration { property = CssPropertyListStylePosition CssValueListStylePositionInside,   important = True } )
  , ( "list-style-position: outside",                   Just CssDeclaration { property = CssPropertyListStylePosition CssValueListStylePositionOutside,  important = False } )
  , ( "list-style-position: outside !important",        Just CssDeclaration { property = CssPropertyListStylePosition CssValueListStylePositionOutside,  important = True } )
  -- Testing for parsing of bad css: invalid value.
  , ( "list-style-position: outide !important",         Nothing)




  , ( "list-style-type: disc !important",                 Just CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeDisc,                important = True  } )
  , ( "list-style-type: circle",                          Just CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeCircle,              important = False } )
  , ( "list-style-type: square !important",               Just CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeSquare,              important = True  } )
  , ( "list-style-type: decimal",                         Just CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeDecimal,             important = False } )
  , ( "list-style-type: decimal-leading-zero !important", Just CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeDecimalLeadingZero,  important = True  } )
  , ( "list-style-type: lower-roman",                     Just CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeLowerRoman,          important = False } )
  , ( "list-style-type: upper-roman !important",          Just CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeUpperRoman,          important = True  } )
  , ( "list-style-type: lower-greek",                     Just CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeLowerGreek,          important = False } )
  , ( "list-style-type: lower-alpha !important",          Just CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeLowerAlpha,          important = True  } )
  , ( "list-style-type: lower-latin",                     Just CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeLowerLatin,          important = False } )
  , ( "list-style-type: upper-alpha !important",          Just CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeUpperAlpha,          important = True  } )
  , ( "list-style-type: upper-latin",                     Just CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeUpperLatin,          important = False } )
  , ( "list-style-type: hebrew !important",               Just CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeHebrew,              important = True  } )
  , ( "list-style-type: armenian",                        Just CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeArmenian,            important = False } )
  , ( "list-style-type: georgian !important",             Just CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeGeorgian,            important = True  } )
  , ( "list-style-type: cjk-ideographic",                 Just CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeCjkIdeographic,      important = False } )
  , ( "list-style-type: hiragana !important",             Just CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeHiragana,            important = True  } )
  , ( "list-style-type: katakana",                        Just CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeKatakana,            important = False } )
  , ( "list-style-type: hiragana-iroha !important",       Just CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeHiraganaIroha,       important = True  } )
  , ( "list-style-type: katakana-iroha",                  Just CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeKatakanaIroha,       important = False } )
  , ( "list-style-type: none !important",                 Just CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeNone,                important = True  } )
  -- Testing for parsing of bad css: invalid property name.
  , ( "list-styletype: upper-latin",                      Nothing)
  -- Testing for parsing of bad css: invalid value.
  , ( "list-style-type: lower-ronan",                     Nothing)
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "list-style-type: none !improtant",                 Nothing)




    -- All four values provided: top, right, bottom, left.
  , ("margin: 10.1px 20.2mm 30.3em 40.4ex",  Just CssDeclaration { property = CssPropertyMargin CssValueMargin
                                                                   { marginTop    = CssValueMarginXDistance (CssDistanceAbsPx 10.1)
                                                                   , marginRight  = CssValueMarginXDistance (CssDistanceAbsMm 20.2)
                                                                   , marginBottom = CssValueMarginXDistance (CssDistanceRelEm 30.3)
                                                                   , marginLeft   = CssValueMarginXDistance (CssDistanceRelEx 40.4)
                                                                   }
                                                                 , important = False }
    )
    -- All four values provided: top, right, bottom, left, but with varying
    -- spaces between values.
  , ("margin:  10.1px   20.2mm  \t 30.3em \t\n 40.4ex",  Just CssDeclaration { property = CssPropertyMargin CssValueMargin
                                                                               { marginTop    = CssValueMarginXDistance (CssDistanceAbsPx 10.1)
                                                                               , marginRight  = CssValueMarginXDistance (CssDistanceAbsMm 20.2)
                                                                               , marginBottom = CssValueMarginXDistance (CssDistanceRelEm 30.3)
                                                                               , marginLeft   = CssValueMarginXDistance (CssDistanceRelEx 40.4)
                                                                               }
                                                                             , important = False }
    )
    -- Three values are provided: top, right-left, bottom.
  , ("margin: 11px 22mm 33.3em",             Just CssDeclaration { property = CssPropertyMargin CssValueMargin
                                                                   { marginTop    = CssValueMarginXDistance (CssDistanceAbsPx 11.0)
                                                                   , marginRight  = CssValueMarginXDistance (CssDistanceAbsMm 22.0)
                                                                   , marginBottom = CssValueMarginXDistance (CssDistanceRelEm 33.3)
                                                                   , marginLeft   = CssValueMarginXDistance (CssDistanceAbsMm 22.0)
                                                                   }
                                                                 , important = False }
    )
    -- Two values are provided: top-bottom, right-left.
  , ("margin: 100px 200mm",                  Just CssDeclaration { property = CssPropertyMargin CssValueMargin
                                                                   { marginTop    = CssValueMarginXDistance (CssDistanceAbsPx 100.0)
                                                                   , marginRight  = CssValueMarginXDistance (CssDistanceAbsMm 200.0)
                                                                   , marginBottom = CssValueMarginXDistance (CssDistanceAbsPx 100.0)
                                                                   , marginLeft   = CssValueMarginXDistance (CssDistanceAbsMm 200.0)
                                                                   }
                                                                 , important = False }
    )
    -- One value is provided: top-right-bottom-left.
  , ("margin: 38.01em",                      Just CssDeclaration { property = CssPropertyMargin CssValueMargin
                                                                   { marginTop    = CssValueMarginXDistance (CssDistanceRelEm 38.01)
                                                                   , marginRight  = CssValueMarginXDistance (CssDistanceRelEm 38.01)
                                                                   , marginBottom = CssValueMarginXDistance (CssDistanceRelEm 38.01)
                                                                   , marginLeft   = CssValueMarginXDistance (CssDistanceRelEm 38.01)
                                                                   }
                                                                 ,  important = False }
    )
  -- Failure case: five values provided (while at most 4 expected).
  , ("margin: 10.1px 20.2mm 30.3em 40.4ex 50.5mm",  Nothing)
  -- Failure cases: zero values provided (while at least 1 expected).
  , ("margin: ",   Nothing)
  , ("margin: ;",  Nothing)
  , ("margin: }",  Nothing)




  , ( "margin-top: auto",                      Just CssDeclaration { property = CssPropertyMarginTop . CssValueMarginXDistance $ CssDistanceAuto,              important = False } )
  , ( "margin-top: auto !important",           Just CssDeclaration { property = CssPropertyMarginTop . CssValueMarginXDistance $ CssDistanceAuto,              important = True  } )
  , ( "margin-top:  1px",                      Just CssDeclaration { property = CssPropertyMarginTop (CssValueMarginXDistance (CssDistanceAbsPx  1.0)),        important = False } )
  , ( "margin-top:  2.2mm !important",         Just CssDeclaration { property = CssPropertyMarginTop (CssValueMarginXDistance (CssDistanceAbsMm  2.2)),        important = True  } )
  , ( "margin-top:  3.0em",                    Just CssDeclaration { property = CssPropertyMarginTop (CssValueMarginXDistance (CssDistanceRelEm  3.0)),        important = False } )
  , ( "margin-top: 93.0ex",                    Just CssDeclaration { property = CssPropertyMarginTop (CssValueMarginXDistance (CssDistanceRelEx 93.0)),        important = False } )
    -- Testing for parsing of bad css: invalid property name.
  , ( "margin-to: 11.0px",                     Nothing)
    -- Testing for parsing of bad css: invalid value.
  , ( "margin-top: red",                       Nothing)
    -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "margin-top: 26.6px !inportant",         Nothing)




  , ( "margin-right: auto",                    Just CssDeclaration { property = CssPropertyMarginRight . CssValueMarginXDistance $ CssDistanceAuto,            important = False } )
  , ( "margin-right: auto !important",         Just CssDeclaration { property = CssPropertyMarginRight . CssValueMarginXDistance $ CssDistanceAuto,            important = True  } )
  , ( "margin-right: 111px",                   Just CssDeclaration { property = CssPropertyMarginRight (CssValueMarginXDistance (CssDistanceAbsPx 111.0)),     important = False } )
  , ( "margin-right: 222mm !important",        Just CssDeclaration { property = CssPropertyMarginRight (CssValueMarginXDistance (CssDistanceAbsMm 222.0)),     important = True  } )
  , ( "margin-right: 333.0em",                 Just CssDeclaration { property = CssPropertyMarginRight (CssValueMarginXDistance (CssDistanceRelEm 333.0)),     important = False } )
  , ( "margin-right: 444.0ex !important",      Just CssDeclaration { property = CssPropertyMarginRight (CssValueMarginXDistance (CssDistanceRelEx 444.0)),     important = True  } )
    -- Testing for parsing of bad css: invalid property name.
  , ( "margin-rigth: 11.0px",                  Nothing)
    -- Testing for parsing of bad css: invalid value.
  , ( "margin-right: italic",                  Nothing)
    -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "margin-right: 33.6px !inportant",       Nothing)




  , ( "margin-bottom: auto",                   Just CssDeclaration { property = CssPropertyMarginBottom . CssValueMarginXDistance $ CssDistanceAuto,           important = False } )
  , ( "margin-bottom: auto !important",        Just CssDeclaration { property = CssPropertyMarginBottom . CssValueMarginXDistance $ CssDistanceAuto,           important = True  } )
  , ( "margin-bottom: 1.110px",                Just CssDeclaration { property = CssPropertyMarginBottom (CssValueMarginXDistance (CssDistanceAbsPx 1.11)),     important = False } )
  , ( "margin-bottom: 2.220mm !important",     Just CssDeclaration { property = CssPropertyMarginBottom (CssValueMarginXDistance (CssDistanceAbsMm 2.22)),     important = True  } )
  , ( "margin-bottom: 3.330em",                Just CssDeclaration { property = CssPropertyMarginBottom (CssValueMarginXDistance (CssDistanceRelEm 3.33)),     important = False } )
  , ( "margin-bottom: 4.440ex !important",     Just CssDeclaration { property = CssPropertyMarginBottom (CssValueMarginXDistance (CssDistanceRelEx 4.44)),     important = True  } )
    -- Testing for parsing of bad css: invalid property name.
  , ( "margin-botom: 11.0px",                  Nothing)
    -- Testing for parsing of bad css: invalid value.
  , ( "margin-bottom: none",                   Nothing)
    -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "margin-bottom: 33.6px !inportant",      Nothing)




  , ( "margin-left: auto",                     Just CssDeclaration { property = CssPropertyMarginLeft . CssValueMarginXDistance $ CssDistanceAuto,             important = False } )
  , ( "margin-left: auto !important",          Just CssDeclaration { property = CssPropertyMarginLeft . CssValueMarginXDistance $ CssDistanceAuto,             important = True  } )
  , ( "margin-left: 1.110px !important",       Just CssDeclaration { property = CssPropertyMarginLeft (CssValueMarginXDistance (CssDistanceAbsPx 1.11)),       important = True  } )
  , ( "margin-left: 2.220mm",                  Just CssDeclaration { property = CssPropertyMarginLeft (CssValueMarginXDistance (CssDistanceAbsMm 2.22)),       important = False } )
  , ( "margin-left: 3.330em !important",       Just CssDeclaration { property = CssPropertyMarginLeft (CssValueMarginXDistance (CssDistanceRelEm 3.33)),       important = True  } )
  , ( "margin-left: 4.440ex",                  Just CssDeclaration { property = CssPropertyMarginLeft (CssValueMarginXDistance (CssDistanceRelEx 4.44)),       important = False } )
    -- Testing for parsing of bad css: invalid property name.
  , ( "margin_left: 11.0px",                   Nothing)
    -- Testing for parsing of bad css: invalid value.
  , ( "margin-left: latin",                    Nothing)
    -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "margin-left: 33.6px !inportant",        Nothing)






      -- All four values provided: top, right, bottom, left.
  , ("padding: 10.1px 20.2mm 30.3em 40.4ex",  Just CssDeclaration { property = CssPropertyPadding CssValuePadding
                                                                    { paddingTop    = CssValuePaddingX (CssDistanceAbsPx 10.1)
                                                                    , paddingRight  = CssValuePaddingX (CssDistanceAbsMm 20.2)
                                                                    , paddingBottom = CssValuePaddingX (CssDistanceRelEm 30.3)
                                                                    , paddingLeft   = CssValuePaddingX (CssDistanceRelEx 40.4)
                                                                    }
                                                                  , important = False
                                                                  }
    )
    -- All four values provided: top, right, bottom, left, but with varying
    -- spaces between values.
  , ("padding:  10.1px   20.2mm  \t 30.3em \t\n 40.4ex",  Just CssDeclaration { property = CssPropertyPadding CssValuePadding
                                                                                { paddingTop    = CssValuePaddingX (CssDistanceAbsPx 10.1)
                                                                                , paddingRight  = CssValuePaddingX (CssDistanceAbsMm 20.2)
                                                                                , paddingBottom = CssValuePaddingX (CssDistanceRelEm 30.3)
                                                                                , paddingLeft   = CssValuePaddingX (CssDistanceRelEx 40.4)
                                                                                }
                                                                              , important = False
                                                                              }
    )
    -- Three values are provided: top, right-left, bottom.
  , ("padding: 11px 22mm 33.3em",             Just CssDeclaration { property = CssPropertyPadding CssValuePadding
                                                                    { paddingTop    = CssValuePaddingX (CssDistanceAbsPx 11.0)
                                                                    , paddingRight  = CssValuePaddingX (CssDistanceAbsMm 22.0)
                                                                    , paddingBottom = CssValuePaddingX (CssDistanceRelEm 33.3)
                                                                    , paddingLeft   = CssValuePaddingX (CssDistanceAbsMm 22.0)
                                                                    }
                                                                  , important = False
                                                                  }
    )
    -- Two values are provided: top-bottom, right-left.
  , ("padding: 100px 200mm",                  Just CssDeclaration { property = CssPropertyPadding CssValuePadding
                                                                    { paddingTop    = CssValuePaddingX (CssDistanceAbsPx 100.0)
                                                                    , paddingRight  = CssValuePaddingX (CssDistanceAbsMm 200.0)
                                                                    , paddingBottom = CssValuePaddingX (CssDistanceAbsPx 100.0)
                                                                    , paddingLeft   = CssValuePaddingX (CssDistanceAbsMm 200.0)
                                                                    }
                                                                  , important = False
                                                                  }
    )
    -- One value is provided: top-right-bottom-left.
  , ("padding: 38.01em",                      Just CssDeclaration { property = CssPropertyPadding CssValuePadding
                                                                    { paddingTop    = CssValuePaddingX (CssDistanceRelEm 38.01)
                                                                    , paddingRight  = CssValuePaddingX (CssDistanceRelEm 38.01)
                                                                    , paddingBottom = CssValuePaddingX (CssDistanceRelEm 38.01)
                                                                    , paddingLeft   = CssValuePaddingX (CssDistanceRelEm 38.01)
                                                                    }
                                                                  , important = False
                                                                  }
    )
  -- Failure case: five values provided (while at most 4 expected).
  , ("padding: 10.1px 20.2mm 30.3em 40.4ex 50.5mm",  Nothing)
  -- Failure cases: zero values provided (while at least 1 expected).
  , ("padding: ",   Nothing)
  , ("padding: ;",  Nothing)
  , ("padding: }",  Nothing)






  , ( "padding-top: 1.0px",                   Just CssDeclaration { property = CssPropertyPaddingTop (CssValuePaddingX (CssDistanceAbsPx 1.0)),       important = False } )
  , ( "padding-top: 2.3mm !important",        Just CssDeclaration { property = CssPropertyPaddingTop (CssValuePaddingX (CssDistanceAbsMm 2.3)),       important = True  } )
  , ( "padding-top: 4.5em",                   Just CssDeclaration { property = CssPropertyPaddingTop (CssValuePaddingX (CssDistanceRelEm 4.5)),       important = False } )
  -- Testing for parsing of bad css: invalid property name.
  , ( "padding-to: 1.0px",                    Nothing)
  -- Testing for parsing of bad css: invalid value.
  , ( "padding-top: red",                     Nothing)
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "padding-top: 6.6px !inportant",        Nothing)




  , ( "padding-right: 1.0px",                 Just CssDeclaration { property = CssPropertyPaddingRight (CssValuePaddingX (CssDistanceAbsPx 1.0)),     important = False } )
  , ( "padding-right: 2.3mm !important",      Just CssDeclaration { property = CssPropertyPaddingRight (CssValuePaddingX (CssDistanceAbsMm 2.3)),     important = True  } )
  , ( "padding-right: 4.5em",                 Just CssDeclaration { property = CssPropertyPaddingRight (CssValuePaddingX (CssDistanceRelEm 4.5)),     important = False } )
  -- Testing for parsing of bad css: invalid property name.
  , ( "padding-rig: 1.0px",                   Nothing)
  -- Testing for parsing of bad css: invalid value.
  , ( "padding-right: red",                   Nothing)
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "padding-right: 6.6px !inportant",      Nothing)




  , ( "padding-bottom: 1.0px",                Just CssDeclaration { property = CssPropertyPaddingBottom (CssValuePaddingX (CssDistanceAbsPx 1.0)),    important = False } )
  , ( "padding-bottom: 2.3mm !important",     Just CssDeclaration { property = CssPropertyPaddingBottom (CssValuePaddingX (CssDistanceAbsMm 2.3)),    important = True  } )
  , ( "padding-bottom: 4.5em",                Just CssDeclaration { property = CssPropertyPaddingBottom (CssValuePaddingX (CssDistanceRelEm 4.5)),    important = False } )
  -- Testing for parsing of bad css: invalid property name.
  , ( "padding-rig: 1.0px",                   Nothing)
  -- Testing for parsing of bad css: invalid value.
  , ( "padding-bottom: red",                  Nothing)
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "padding-bottom: 6.6px !inportant",     Nothing)




  , ( "padding-left: 1.0px",                  Just CssDeclaration { property = CssPropertyPaddingLeft (CssValuePaddingX (CssDistanceAbsPx 1.0)),      important = False } )
  , ( "padding-left: 2.3mm !important",       Just CssDeclaration { property = CssPropertyPaddingLeft (CssValuePaddingX (CssDistanceAbsMm 2.3)),      important = True  } )
  , ( "padding-left: 4.5em",                  Just CssDeclaration { property = CssPropertyPaddingLeft (CssValuePaddingX (CssDistanceRelEm 4.5)),      important = False } )
  -- Testing for parsing of bad css: invalid property name.
  , ( "padding-rig: 1.0px",                   Nothing)
  -- Testing for parsing of bad css: invalid value.
  , ( "padding-left: red",                    Nothing)
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "padding-left: 6.6px !inportant",       Nothing)




  , ( "text-indent:  1.1px !important",            Just CssDeclaration { property = CssPropertyTextIndent (CssValueTextIndentDistance (CssDistanceAbsPx     1.1)),   important = True  } )
  , ( "text-indent:  2.2mm",                       Just CssDeclaration { property = CssPropertyTextIndent (CssValueTextIndentDistance (CssDistanceAbsMm     2.2)),   important = False } )
  , ( "text-indent: 13.3em",                       Just CssDeclaration { property = CssPropertyTextIndent (CssValueTextIndentDistance (CssDistanceRelEm    13.3)),   important = False } )
  , ( "text-indent: 44.4ex !important",            Just CssDeclaration { property = CssPropertyTextIndent (CssValueTextIndentDistance (CssDistanceRelEx    44.4)),   important = True  } )
  -- From a real web page :)
  , ( "text-indent: -700em",                       Just CssDeclaration { property = CssPropertyTextIndent (CssValueTextIndentDistance (CssDistanceRelEm (-700.0))),  important = False } )

  -- Testing for parsing of bad css: invalid property name.
  , ( "test-indent: 55.5mm",                       Nothing)
  -- Testing for parsing of bad css: invalid property value.
  , ( "text-indent: justify",                      Nothing)
    -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "text-indent: 77.7ex !importan",             Nothing)




  , ("text-align: left !important",       Just CssDeclaration { property = CssPropertyTextAlign CssValueTextAlignLeft,     important = True  } )
  , ("text-align: right",                 Just CssDeclaration { property = CssPropertyTextAlign CssValueTextAlignRight,    important = False } )
  , ("text-align: center !important",     Just CssDeclaration { property = CssPropertyTextAlign CssValueTextAlignCenter,   important = True  } )
  , ("text-align: justify",               Just CssDeclaration { property = CssPropertyTextAlign CssValueTextAlignJustify,  important = False } )
  -- Testing for parsing of bad css: invalid property name.
  , ("test-align: left",                  Nothing)
  -- Testing for parsing of bad css: invalid value.
  , ("text-align: italic",                Nothing)
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ("text-align: left !!important",      Nothing)




  , ("text-transform: none",                  Just CssDeclaration { property = CssPropertyTextTransform CssValueTextTransformNone,        important = False } )
  , ("text-transform: capitalize !important", Just CssDeclaration { property = CssPropertyTextTransform CssValueTextTransformCapitalize,  important = True  } )
  , ("text-transform: uppercase",             Just CssDeclaration { property = CssPropertyTextTransform CssValueTextTransformUppercase,   important = False } )
  , ("text-transform: lowercase !important",  Just CssDeclaration { property = CssPropertyTextTransform CssValueTextTransformLowercase,   important = True  } )
  -- Testing for parsing of bad css: invalid property name.
  , ("test-transform: none",                  Nothing)
  -- Testing for parsing of bad css: invalid value.
  , ("text-transform: 1.0px",                 Nothing)
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ("text-transform: uppercase _important",  Nothing)




  -- First some simple cases, where only one value appears in input.
  , ("text-decoration: underline !important",     Just CssDeclaration { property = CssPropertyTextDecoration [CssValueTextDecorationUnderline],    important = True  } )
  , ("text-decoration: overline",                 Just CssDeclaration { property = CssPropertyTextDecoration [CssValueTextDecorationOverline],     important = False } )
  , ("text-decoration: line-through !important",  Just CssDeclaration { property = CssPropertyTextDecoration [CssValueTextDecorationLineThrough],  important = True  } )
  , ("text-decoration: blink",                    Just CssDeclaration { property = CssPropertyTextDecoration [CssValueTextDecorationBlink],        important = False } )

  -- Now few valid values. Notice that in different test cases the values appear in in different order.
  , ("text-decoration: underline overline line-through blink", Just CssDeclaration { property = CssPropertyTextDecoration
                                                                                                [ CssValueTextDecorationUnderline
                                                                                                , CssValueTextDecorationOverline
                                                                                                , CssValueTextDecorationLineThrough
                                                                                                , CssValueTextDecorationBlink
                                                                                                ],
                                                                                     important = False } )
  , ("text-decoration: blink overline underline line-through", Just CssDeclaration { property = CssPropertyTextDecoration
                                                                                                [ CssValueTextDecorationBlink
                                                                                                , CssValueTextDecorationOverline
                                                                                                , CssValueTextDecorationUnderline
                                                                                                , CssValueTextDecorationLineThrough
                                                                                                ],
                                                                                     important = False } )
  , ("text-decoration: overline line-through",                 Just CssDeclaration { property = CssPropertyTextDecoration
                                                                                                [ CssValueTextDecorationOverline
                                                                                                , CssValueTextDecorationLineThrough
                                                                                                ],
                                                                                     important = False } )
  , ("text-decoration: blink line-through !important",         Just CssDeclaration { property = CssPropertyTextDecoration
                                                                                                [ CssValueTextDecorationBlink
                                                                                                , CssValueTextDecorationLineThrough
                                                                                                ],
                                                                                     important = True } )

  -- Testing for parsing of bad css: invalid property name.
  , ("test-decoration: overline",                     Nothing)
  -- Testing for parsing of bad css: invalid value. Notice that a single invalid value token invalidates entire property.
  , ("text-decoration: blue",                         Nothing)
  , ("text-decoration: underline blue",               Nothing)
  , ("text-decoration: 1.0px overline",               Nothing)
  , ("text-decoration: underline italic blink",       Nothing)
  , ("text-decoration: underline overline brink",     Nothing)
  -- Testing for parsing of bad css: misspelled "important" word.
  --
  -- Notice that in this case the "_important" word is treated as one of
  -- possible decorations. The word is not a valid decoration, so entrie
  -- declaration is rejected.
  , ("text-decoration: underline _important",         Nothing)




  , ( "vertical-align: top !important",       Just CssDeclaration { property = CssPropertyVerticalAlign CssValueVerticalAlignTop,        important = True  } )
  , ( "vertical-align: bottom",               Just CssDeclaration { property = CssPropertyVerticalAlign CssValueVerticalAlignBottom,     important = False } )
  , ( "vertical-align: middle !important",    Just CssDeclaration { property = CssPropertyVerticalAlign CssValueVerticalAlignMiddle,     important = True  } )
  , ( "vertical-align: baseline",             Just CssDeclaration { property = CssPropertyVerticalAlign CssValueVerticalAlignBaseline,   important = False } )
  , ( "vertical-align: sub !important",       Just CssDeclaration { property = CssPropertyVerticalAlign CssValueVerticalAlignSub,        important = True  } )
  , ( "vertical-align: super",                Just CssDeclaration { property = CssPropertyVerticalAlign CssValueVerticalAlignSuper,      important = False } )
  , ( "vertical-align: text-top !important",  Just CssDeclaration { property = CssPropertyVerticalAlign CssValueVerticalAlignTextTop,    important = True  } )
  , ( "vertical-align: text-bottom",          Just CssDeclaration { property = CssPropertyVerticalAlign CssValueVerticalAlignTextBottom, important = False } )
  -- Testing for parsing of bad css: invalid property name.
  , ( "ertical-align: pre",                   Nothing)
  -- Testing for parsing of bad css: invalid value.
  , ( "vertical-align: suber",                Nothing)
  -- Testing for parsing of bad css: incorrect value of "important" keyword. TODO: check how parser should behave here according to spec.
  , ( "vertical-align: top !!important",      Nothing)




  , ("width: auto",                     Just CssDeclaration { property = CssPropertyWidth . CssValueWidthDistance $ CssDistanceAuto,                    important = False } )
  , ("width: auto !important",          Just CssDeclaration { property = CssPropertyWidth . CssValueWidthDistance $ CssDistanceAuto,                    important = True  } )
  , ("width:   1px",                    Just CssDeclaration { property = CssPropertyWidth (CssValueWidthDistance (CssDistanceAbsPx   1.0)),             important = False } )
  , ("width:   1px !important",         Just CssDeclaration { property = CssPropertyWidth (CssValueWidthDistance (CssDistanceAbsPx   1.0)),             important = True  } )
  , ("width:  22.22mm",                 Just CssDeclaration { property = CssPropertyWidth (CssValueWidthDistance (CssDistanceAbsMm  22.22)),            important = False } )
  , ("width:  22.22mm !important",      Just CssDeclaration { property = CssPropertyWidth (CssValueWidthDistance (CssDistanceAbsMm  22.22)),            important = True  } )
  , ("width:  33.3em",                  Just CssDeclaration { property = CssPropertyWidth (CssValueWidthDistance (CssDistanceRelEm  33.3)),             important = False } )
  , ("width:  33.3em !important",       Just CssDeclaration { property = CssPropertyWidth (CssValueWidthDistance (CssDistanceRelEm  33.3)),             important = True  } )
  , ("width: 444.44ex",                 Just CssDeclaration { property = CssPropertyWidth (CssValueWidthDistance (CssDistanceRelEx 444.44)),            important = False } )
  , ("width: 444.44ex !important",      Just CssDeclaration { property = CssPropertyWidth (CssValueWidthDistance (CssDistanceRelEx 444.44)),            important = True  } )

    -- Testing for parsing of bad css: invalid property name.
  , ("widht:  77.7em",                  Nothing)
    -- Testing for parsing of bad css: invalid value.
  , ("width:  left",                    Nothing)
    -- TODO: per CSS2.2 negative values are invalid. Fix this case in parser.
  , ("width: -500.0mm",                 Just CssDeclaration { property = CssPropertyWidth (CssValueWidthDistance (CssDistanceAbsMm (-500.00))),         important = False } )
    -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ("width:  22.22mm !importat",       Nothing)




  , ( "white-space: normal !important",     Just CssDeclaration { property = CssPropertyWhitespace CssValueWhitespaceNormal,   important = True  } )
  , ( "white-space: pre",                   Just CssDeclaration { property = CssPropertyWhitespace CssValueWhitespacePre,      important = False } )
  , ( "white-space: nowrap !important",     Just CssDeclaration { property = CssPropertyWhitespace CssValueWhitespaceNoWrap,   important = True  } )
  , ( "white-space: pre-wrap",              Just CssDeclaration { property = CssPropertyWhitespace CssValueWhitespacePreWrap,  important = False } )
  , ( "white-space: pre-line  !important",  Just CssDeclaration { property = CssPropertyWhitespace CssValueWhitespacePreLine,  important = True  } )
  -- Testing for parsing of bad css: invalid property name.
  , ( "white-spac: pre",                    Nothing)
  -- Testing for parsing of bad css: invalid value.
  , ( "white-space: prewrap",               Nothing)
  -- Testing for parsing of bad css: incorrect value of "important" keyword. TODO: check how parser should behave here according to spec.
  , ( "white-space: pre important",         Nothing)




  , ( "word-spacing: normal !important",    Just CssDeclaration { property = CssPropertyWordSpacing CssValueWordSpacingNormal,                              important = True  } )
  , ( "word-spacing: 1.0px",                Just CssDeclaration { property = CssPropertyWordSpacing (CssValueWordSpacingDistance (CssDistanceAbsPx 1.0)),   important = False } )
  , ( "word-spacing: 2.5mm !important",     Just CssDeclaration { property = CssPropertyWordSpacing (CssValueWordSpacingDistance (CssDistanceAbsMm 2.5)),   important = True  } )
  , ( "word-spacing: 3.6em",                Just CssDeclaration { property = CssPropertyWordSpacing (CssValueWordSpacingDistance (CssDistanceRelEm 3.6)),   important = False } )
  , ( "word-spacing: 4.7ex !important",     Just CssDeclaration { property = CssPropertyWordSpacing (CssValueWordSpacingDistance (CssDistanceRelEx 4.7)),   important = True  } )
  -- Testing for parsing of bad css: invalid property name.
  , ( "words-pacing: normal !important",    Nothing)
  -- Testing for parsing of bad css: invalid value. TODO: shouldn't "1" be considered a valid value?
  , ( "word-spacing: 1;0xz",                Nothing)
  -- Testing for parsing of bad css: incorrect value of "important" keyword. TODO: check how parser should behave here according to spec.
  , ( "word-spacing: normal !importan",     Nothing)
  ]




-- On success return empty string. On failure return string showing
-- approximately where the problem is.
parseSingleDeclarationTest :: [(T.Text, Maybe CssDeclaration)] -> T.Text
parseSingleDeclarationTest []     = ""
parseSingleDeclarationTest (x:xs) = if expectedDeclarations /= declarations
                                    then T.pack ("Got: " ++ show declarations ++ ", Expected: " ++ show expectedDeclarations)
                                    else parseSingleDeclarationTest xs
  where
    remd                 = fst x
    expectedDeclarations = snd x
    ((_parser', _token'), declarations) = parseSingleDeclaration pat

    -- This tests parses a declaration. Declaration is inside of {} block.
    -- Therefore construct a parser that has recognized that it is inside a
    -- block.
    pat = nextToken . defaultParserInBlock $ remd




{- -------------------------------------------------------------------------- -}




testCases = [
  -- If some error is found, test function returns some data (e.g. non-empty
  -- string or test index) which can help identify which test failed.
     TestCase (do
                  assertEqual "manual tests of parseProperty" ""          (parsePropertyTestFunction parsePropertyTestData)
                  assertEqual "manual tests of parseSingleDeclaration" "" (parseSingleDeclarationTest parseSingleDeclarationTestData)
              )
  ]




testsCssParserDeclaration :: IO String
testsCssParserDeclaration = do
  testCounts <- runTestTT (TestList (testCases))
  if (errors testCounts + failures testCounts == 0)
    then return ""
    else return "[EE] Hello.Tests.Css.Parser.Declaration failed"


