{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}




module Hello.Tests.Css.Parser
  (
    testsCssParser
  )
where




import qualified Data.Text as T

import Test.HUnit
--import Debug.Trace

import Hello.Css.Declaration
import Hello.Css.Distance
import Hello.Css.Parser.Rule
import Hello.Css.Tokenizer




{- -------------------------------------------------------------------------- -}




{-
Test a function that parses a CSS declaration: a property name + property
value. Tested function can return multiple CSS declarations for property
names such as "background" or "border".
-}




parseDeclarationTestData =
  [
    ( "background-attachment: scroll",              [CssDeclaration { property = CssPropertyBackgroundAttachment CssValueBackgroundAttachmentScroll,       important = False } ])
  , ( "background-attachment: scroll !important",   [CssDeclaration { property = CssPropertyBackgroundAttachment CssValueBackgroundAttachmentScroll,       important = True  } ])
  , ( "background-attachment: fixed",               [CssDeclaration { property = CssPropertyBackgroundAttachment CssValueBackgroundAttachmentFixed,        important = False } ])
  , ( "background-attachment: fixed !important",    [CssDeclaration { property = CssPropertyBackgroundAttachment CssValueBackgroundAttachmentFixed,        important = True  } ])

  -- Testing for parsing of bad css: invalid property name.
  , ( "background-atachment: scroll",               [])
  -- Testing for parsing of bad css: invalid value.
  , ( "background-attachment: italic",              [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "background-attachment: fixed important",     [CssDeclaration { property = CssPropertyBackgroundAttachment CssValueBackgroundAttachmentFixed,        important = False  } ])




  , ( "background-color: inherit",                   [CssDeclaration { property = CssPropertyBackgroundColor CssValueBackgroundColorInherit,             important = False } ])
  , ( "background-color: inherit !important",        [CssDeclaration { property = CssPropertyBackgroundColor CssValueBackgroundColorInherit,             important = True  } ])
  , ( "background-color: blue",                      [CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0x0000ff),    important = False } ])
  , ( "background-color: blue !important",           [CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0x0000ff),    important = True  } ])
  , ( "background-color: blue !important;",          [CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0x0000ff),    important = True  } ])
  , ( "background-color: #00ff00;",                  [CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0x00ff00),    important = False } ])
  , ( "background-color: #00ff00 !important",        [CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0x00ff00),    important = True  } ])
  , ( "background-color: #00ff00 !important;",       [CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0x00ff00),    important = True  } ])
  , ( "background-color: rgb(0, 0, 255)",            [CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0x0000ff),    important = False } ])
  , ( "background-color: rgb(0, 0, 255) !important", [CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0x0000ff),    important = True  } ])

  -- Testing for parsing of bad css: invalid property name.
  , ( "background-colo: blue",                       [])
  -- Testing for parsing of bad css: invalid value.
  , ( "background-color: square",                    [])
  , ( "background-color: 0x00ff00",                  []) -- Invalid format of HEX value
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "background-color: rgb(255, 0, 0) important",  [CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 0xff0000),        important = False  } ])




  -- TODO: decide what should be the value of CssValueBackgroundImageUri.
  -- Should it be just a verbatim stream, or some information about tokens that build the URI.
  -- TODO: write more tests
  , ( "background-image: url(\"background.png\")",     [CssDeclaration { property = CssPropertyBackgroundImage (
                                                                           CssValueBackgroundImageUri "[CssTokStr \"background.png\",CssTokParenClose]"),         important = False } ])




  -- Support for background position in hello is almost non-existent, so this
  -- test set is very, very, very basic.
  , ( "background-position: left top",              [CssDeclaration { property = CssPropertyBackgroundPosition (CssValueBackgroundPositionXY 0 0),    important = False } ])
  , ( "background-position: left top !important",   [CssDeclaration { property = CssPropertyBackgroundPosition (CssValueBackgroundPositionXY 0 0),    important = True  } ])

  -- Testing for parsing of bad css: invalid property name.
  , ( "backgroundposition: left top",               [])
  -- Testing for parsing of bad css: invalid value.
  --, ( "background-position: italic",                [])
  --, ( "background-position: left-top",              [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "background-position: left top important",    [CssDeclaration { property = CssPropertyBackgroundPosition (CssValueBackgroundPositionXY 0 0),    important = False  } ])




  , ( "background-repeat: repeat",                  [CssDeclaration { property = CssPropertyBackgroundRepeat CssValueBackgroundRepeatRepeat,         important = False } ])
  , ( "background-repeat: repeat-x !important",     [CssDeclaration { property = CssPropertyBackgroundRepeat CssValueBackgroundRepeatRepeatX,        important = True  } ])
  , ( "background-repeat: repeat-y",                [CssDeclaration { property = CssPropertyBackgroundRepeat CssValueBackgroundRepeatRepeatY,        important = False } ])
  , ( "background-repeat: no-repeat !important",    [CssDeclaration { property = CssPropertyBackgroundRepeat CssValueBackgroundRepeatNoRepeat,       important = True  } ])

  -- Testing for parsing of bad css: invalid property name.
  , ( "background_repeat: repeat",                  [])
  -- Testing for parsing of bad css: invalid value.
  , ( "background-repeat: #00ff00",                 [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "background-repeat: no-repeat !importan",     [CssDeclaration { property = CssPropertyBackgroundRepeat CssValueBackgroundRepeatNoRepeat,       important = False  } ])




  , ( "border: 10px inset #00fff1",
      [ CssDeclaration { property = CssPropertyBorder $ CssValueBorderTRBL
                                    { borderTRBLWidth = CssValueBorderWidthDistance (CssDistanceAbsPx 10.0)
                                    , borderTRBLStyle = CssValueBorderStyleInset
                                    , borderTRBLColor = CssValueBorderColor 0x00fff1
                                    }
                       , important = False
                       }
      ])
  , ( "border: 10px inset #00fff1 !important",
      [ CssDeclaration { property = CssPropertyBorder $ CssValueBorderTRBL
                                    { borderTRBLWidth = CssValueBorderWidthDistance (CssDistanceAbsPx 10.0)
                                    , borderTRBLStyle = CssValueBorderStyleInset
                                    , borderTRBLColor = CssValueBorderColor 0x00fff1
                                    }
                       , important = True
                       }
      ])

  , ( "border: 17px red",
      [ CssDeclaration { property = CssPropertyBorder $ CssValueBorderTRBL
                                    { borderTRBLWidth = CssValueBorderWidthDistance (CssDistanceAbsPx 17.0)
                                    , borderTRBLStyle = defaultBorderTRBLStyle
                                    , borderTRBLColor = CssValueBorderColor 0xff0000
                                    }
                       , important = False
                       }
      ])

  , ( "border: outset rgb(0, 255, 0) 29em",
      [ CssDeclaration { property = CssPropertyBorder $ CssValueBorderTRBL
                                    { borderTRBLWidth = CssValueBorderWidthDistance (CssDistanceRelEm 29.0)
                                    , borderTRBLStyle = CssValueBorderStyleOutset
                                    , borderTRBLColor = CssValueBorderColor 0x00ff00
                                    }
                       , important = False
                       }
      ])
  , ( "border: outset rgb(0, 255, 0) 29em !important",
      [ CssDeclaration { property = CssPropertyBorder $ CssValueBorderTRBL
                                    { borderTRBLWidth = CssValueBorderWidthDistance (CssDistanceRelEm 29.0)
                                    , borderTRBLStyle = CssValueBorderStyleOutset
                                    , borderTRBLColor = CssValueBorderColor 0x00ff00
                                    }
                       , important = True
                       }
      ])

  , ( "border: dotted",
      [ CssDeclaration { property = CssPropertyBorder $ CssValueBorderTRBL
                                    { borderTRBLWidth = defaultBorderTRBLWidth
                                    , borderTRBLStyle = CssValueBorderStyleDotted
                                    , borderTRBLColor = defaultBorderTRBLColor
                                    }
                       , important = False
                       }
      ])
  , ( "border: dotted !important",
      [ CssDeclaration { property = CssPropertyBorder $ CssValueBorderTRBL
                                    { borderTRBLWidth = defaultBorderTRBLWidth
                                    , borderTRBLStyle = CssValueBorderStyleDotted
                                    , borderTRBLColor = defaultBorderTRBLColor
                                    }
                       , important = True
                       }
      ])

  -- Invalid case: no value, just "!important".
  , ( "border: !important",
      [])
  -- Invalid case: nothing after the property name.
  , ( "border:",
      [])
  -- Invalid case: just a space after the property name.
  , ( "border:",
      [])
  -- Invalid case: invalid value.
  , ( "border: elephant",
      [])




  , ( "border-collapse: separate",               [CssDeclaration { property = CssPropertyBorderCollapse CssValueBorderCollapseSeparate,        important = False } ])
  , ( "border-collapse: separate !important",    [CssDeclaration { property = CssPropertyBorderCollapse CssValueBorderCollapseSeparate,        important = True  } ])
  , ( "border-collapse: collapse",               [CssDeclaration { property = CssPropertyBorderCollapse CssValueBorderCollapseCollapse,        important = False } ])
  , ( "border-collapse: collapse !important",    [CssDeclaration { property = CssPropertyBorderCollapse CssValueBorderCollapseCollapse,        important = True  } ])

  -- Testing for parsing of bad css: invalid property name.
  , ( "border-colapse: block",                   [])
  -- Testing for parsing of bad css: invalid value.
  , ( "border-collapse: #00ff00",                [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "border-collapse: separate !importan",     [CssDeclaration { property = CssPropertyBorderCollapse CssValueBorderCollapseSeparate,        important = False  } ])




  , ( "border-spacing:  1.5px !important",            [CssDeclaration { property = CssPropertyBorderSpacing (CssValueBorderSpacingDistance (CssDistanceAbsPx  1.5)),  important = True  } ])
  , ( "border-spacing:  2.0mm",                       [CssDeclaration { property = CssPropertyBorderSpacing (CssValueBorderSpacingDistance (CssDistanceAbsMm  2.0)),  important = False } ])
  , ( "border-spacing: 13.5em",                       [CssDeclaration { property = CssPropertyBorderSpacing (CssValueBorderSpacingDistance (CssDistanceRelEm 13.5)),  important = False } ])
  , ( "border-spacing: 44.0ex !important",            [CssDeclaration { property = CssPropertyBorderSpacing (CssValueBorderSpacingDistance (CssDistanceRelEx 44.0)),  important = True  } ])

  -- Testing for parsing of bad css: invalid property name.
  , ( "border_spacing: 52.0mm",                       [])
  -- Testing for parsing of bad css: invalid property value.
  , ( "border-spacing: latin",                        [])
    -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "border-spacing: 74.0ex !importan",             [CssDeclaration { property = CssPropertyBorderSpacing (CssValueBorderSpacingDistance (CssDistanceRelEx 74.0)),  important = False  } ])



  , ( "border-top: 5mm solid red",            [ CssDeclaration { property = CssPropertyBorderTop CssValueBorderTRBL
                                                                 { borderTRBLWidth = CssValueBorderWidthDistance (CssDistanceAbsMm 5)
                                                                 , borderTRBLStyle = CssValueBorderStyleSolid
                                                                 , borderTRBLColor = CssValueBorderColor 0xff0000
                                                                 }
                                                               , important = False
                                                               }
                                              ])
    -- This time with changed order of values (they can be unordered).
  , ( "border-top: solid red 5mm",            [ CssDeclaration { property = CssPropertyBorderTop CssValueBorderTRBL
                                                                 { borderTRBLWidth = CssValueBorderWidthDistance (CssDistanceAbsMm 5)
                                                                 , borderTRBLStyle = CssValueBorderStyleSolid
                                                                 , borderTRBLColor = CssValueBorderColor 0xff0000
                                                                 }
                                                               , important = False
                                                               }
                                              ])
    -- This time with one of values omitted (default value should be used).
  , ( "border-top: solid red",                  [ CssDeclaration { property = CssPropertyBorderTop CssValueBorderTRBL
                                                                 { borderTRBLWidth = defaultBorderTRBLWidth
                                                                 , borderTRBLStyle = CssValueBorderStyleSolid
                                                                 , borderTRBLColor = CssValueBorderColor 0xff0000
                                                                 }
                                                               , important = False
                                                               }
                                              ])

  , ( "border-right: 2mm dotted orange",      [ CssDeclaration { property = CssPropertyBorderRight CssValueBorderTRBL
                                                                 { borderTRBLWidth = CssValueBorderWidthDistance (CssDistanceAbsMm 2)
                                                                 , borderTRBLStyle = CssValueBorderStyleDotted
                                                                 , borderTRBLColor = CssValueBorderColor 0xffa500
                                                                 }
                                                               , important = False
                                                               }
                                              ])

  , ( "border-bottom: 17px inherit rgb(255, 0, 255)",    [ CssDeclaration { property = CssPropertyBorderBottom CssValueBorderTRBL
                                                                            { borderTRBLWidth = CssValueBorderWidthDistance (CssDistanceAbsPx 17)
                                                                            , borderTRBLStyle = CssValueBorderStyleInherit
                                                                            , borderTRBLColor = CssValueBorderColor 0xff00ff
                                                                            }
                                                                          , important = False
                                                                          }
                                                         ])

  , ( "border-left: 20em none inherit",      [ CssDeclaration { property = CssPropertyBorderLeft CssValueBorderTRBL
                                                                { borderTRBLWidth = CssValueBorderWidthDistance (CssDistanceRelEm 20)
                                                                , borderTRBLStyle = CssValueBorderStyleNone
                                                                , borderTRBLColor = CssValueBorderColorInherit
                                                                }
                                                              , important = False }
                                             ])

  -- Invalid value; TODO: this test fails
  -- , ( "border-top: 5mm computer dashed",      [])






    -- Single value of border-color is provided: top-righ-bottom-left.
  , ( "border-color: rgb(0, 0, 15)",
      [ CssDeclaration { property = CssPropertyBorderColor $ CssValueBorderColor'
                                    { borderColorTop    = CssValueBorderColor 0x00000f
                                    , borderColorRight  = CssValueBorderColor 0x00000f
                                    , borderColorBottom = CssValueBorderColor 0x00000f
                                    , borderColorLeft   = CssValueBorderColor 0x00000f
                                    }
                       , important = False }
      ])
  -- Two values of border-color are provided: top-bottom / right-left.
  , ( "border-color: rgb(0, 15, 0) #ff0000",
      [ CssDeclaration { property = CssPropertyBorderColor $ CssValueBorderColor'
                                    { borderColorTop    = CssValueBorderColor 0x000f00
                                    , borderColorRight  = CssValueBorderColor 0xff0000
                                    , borderColorBottom = CssValueBorderColor 0x000f00
                                    , borderColorLeft   = CssValueBorderColor 0xff0000
                                    }
                       , important = False }
      ])
  -- Three values of border-color are provided:  top / right-left / bottom..
  , ( "border-color: rgb(0, 15, 0) #ff0000 blue",
      [ CssDeclaration { property = CssPropertyBorderColor $ CssValueBorderColor'
                                    { borderColorTop    = CssValueBorderColor 0x000f00
                                    , borderColorRight  = CssValueBorderColor 0xff0000
                                    , borderColorBottom = CssValueBorderColor 0x0000ff
                                    , borderColorLeft   = CssValueBorderColor 0xff0000
                                    }
                       , important = False }
      ])
    -- Four values of border-width are provided: top / right / bottom / left
  , ( "border-color: rgb(0, 15, 0) rgb(1, 15, 0) #012345 burlywood",
      [ CssDeclaration { property = CssPropertyBorderColor $ CssValueBorderColor'
                                    { borderColorTop    = CssValueBorderColor 0x000f00
                                    , borderColorRight  = CssValueBorderColor 0x010f00
                                    , borderColorBottom = CssValueBorderColor 0x012345
                                    , borderColorLeft   = CssValueBorderColor 0xdeb887
                                    }
                       , important = False }
      ])
  -- TODO: add tests of failure cases for border-color.




  -- Single value of border-width is provided: top-righ-bottom-left.
  , ( "border-style: double",
      [ CssDeclaration { property = CssPropertyBorderStyle $ CssValueBorderStyle'
                                    { borderStyleTop    = CssValueBorderStyleDouble
                                    , borderStyleRight  = CssValueBorderStyleDouble
                                    , borderStyleBottom = CssValueBorderStyleDouble
                                    , borderStyleLeft   = CssValueBorderStyleDouble
                                    }
                       , important = False }
      ])
  -- Two values of border-width are provided: top-bottom / right-left.
  , ( "border-style: none hidden",
      [ CssDeclaration { property = CssPropertyBorderStyle $ CssValueBorderStyle'
                                    { borderStyleTop    = CssValueBorderStyleNone
                                    , borderStyleRight  = CssValueBorderStyleHidden
                                    , borderStyleBottom = CssValueBorderStyleNone
                                    , borderStyleLeft   = CssValueBorderStyleHidden
                                    }
                       , important = False }
      ])
  -- Three values of border-width are provided: top / right-left / bottom.
  , ( "border-style: none inherit groove",
      [ CssDeclaration { property = CssPropertyBorderStyle $ CssValueBorderStyle'
                                    { borderStyleTop    = CssValueBorderStyleNone
                                    , borderStyleRight  = CssValueBorderStyleInherit
                                    , borderStyleBottom = CssValueBorderStyleGroove
                                    , borderStyleLeft   = CssValueBorderStyleInherit
                                    }
                       , important = False }
      ])
  -- Four values of border-width are provided: top / right / bottom / left
  , ( "border-style: outset inherit hidden none",
      [ CssDeclaration { property = CssPropertyBorderStyle $ CssValueBorderStyle'
                                    { borderStyleTop    = CssValueBorderStyleOutset
                                    , borderStyleRight  = CssValueBorderStyleInherit
                                    , borderStyleBottom = CssValueBorderStyleHidden
                                    , borderStyleLeft   = CssValueBorderStyleNone
                                    }
                       , important = False }
      ])
  -- TODO: add tests of failure cases for border-style.




  -- Single value of border-width is provided: top-righ-bottom-left.
  , ( "border-width: 99px",
      [ CssDeclaration { property = CssPropertyBorderWidth $ CssValueBorderWidth'
                                    { borderWidthTop    = CssValueBorderWidthDistance (CssDistanceAbsPx 99)
                                    , borderWidthRight  = CssValueBorderWidthDistance (CssDistanceAbsPx 99)
                                    , borderWidthBottom = CssValueBorderWidthDistance (CssDistanceAbsPx 99)
                                    , borderWidthLeft   = CssValueBorderWidthDistance (CssDistanceAbsPx 99)
                                    }
                       , important = False }
      ])
  -- Two values of border-width are provided: top-bottom / right-left.
  , ( "border-width: 88px 77mm",
      [ CssDeclaration { property = CssPropertyBorderWidth $ CssValueBorderWidth'
                                    { borderWidthTop    = CssValueBorderWidthDistance (CssDistanceAbsPx 88)
                                    , borderWidthRight  = CssValueBorderWidthDistance (CssDistanceAbsMm 77)
                                    , borderWidthBottom = CssValueBorderWidthDistance (CssDistanceAbsPx 88)
                                    , borderWidthLeft   = CssValueBorderWidthDistance (CssDistanceAbsMm 77)
                                    }
                       , important = False }
      ])
  -- Three values of border-width are provided: top / right-left / bottom.
  , ( "border-width: 66px 55em 44px",
      [ CssDeclaration { property = CssPropertyBorderWidth $ CssValueBorderWidth'
                                    { borderWidthTop    = CssValueBorderWidthDistance (CssDistanceAbsPx 66)
                                    , borderWidthRight  = CssValueBorderWidthDistance (CssDistanceRelEm 55)
                                    , borderWidthBottom = CssValueBorderWidthDistance (CssDistanceAbsPx 44)
                                    , borderWidthLeft   = CssValueBorderWidthDistance (CssDistanceRelEm 55)
                                    }
                       , important = False }
      ])
  -- Four values of border-width are provided: top / right / bottom / left
  --
  -- TODO: the parser doesn't handle a value of property that includes
  -- "auto". Replace "thin" with "auto" and see what happens: the parser will
  -- tell you that it sees only three values of distance.
  , ( "border-width: 21px 32em 43ex thin",
      [ CssDeclaration { property = CssPropertyBorderWidth $ CssValueBorderWidth'
                                    { borderWidthTop    = CssValueBorderWidthDistance (CssDistanceAbsPx 21)
                                    , borderWidthRight  = CssValueBorderWidthDistance (CssDistanceRelEm 32)
                                    , borderWidthBottom = CssValueBorderWidthDistance (CssDistanceRelEx 43)
                                    , borderWidthLeft   = CssValueBorderWidthThin -- CssValueBorderWidthDistance CssDistanceAuto)
                                    }
                       , important = False }
      ])
  -- TODO: add tests of failure cases for border-width.




  , ( "border-top-color: inherit",                        [CssDeclaration { property = CssPropertyBorderTopColor   CssValueBorderColorInherit,       important = False } ])
  , ( "border-top-color: transparent",                    [CssDeclaration { property = CssPropertyBorderTopColor   CssValueBorderColorTransparent,   important = False } ])
  , ( "border-top-color: red",                            [CssDeclaration { property = CssPropertyBorderTopColor   $ CssValueBorderColor 0xff0000,   important = False } ])
  , ( "border-top-color: #0000ff !important",             [CssDeclaration { property = CssPropertyBorderTopColor   $ CssValueBorderColor 0x0000ff,   important = True  } ])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "border-top-color: #0000ff !iportant",              [CssDeclaration { property = CssPropertyBorderTopColor   $ CssValueBorderColor 0x0000ff,   important = False } ])

  , ( "border-right-color: inherit",                      [CssDeclaration { property = CssPropertyBorderRightColor CssValueBorderColorInherit,       important = False } ])
  , ( "border-right-color: transparent",                  [CssDeclaration { property = CssPropertyBorderRightColor CssValueBorderColorTransparent,   important = False } ])
  , ( "border-right-color: lime !important",              [CssDeclaration { property = CssPropertyBorderRightColor $ CssValueBorderColor 0x00ff00,   important = True  } ])
  , ( "border-right-color: rgb(255, 0, 0) !important",    [CssDeclaration { property = CssPropertyBorderRightColor $ CssValueBorderColor 0xff0000,   important = True  } ])
   -- Testing for parsing of bad css: space after function name.
  , ( "border-right-color: rgb (255, 0, 0) !important",   [])

  , ( "border-bottom-color: inherit !important",          [CssDeclaration { property = CssPropertyBorderBottomColor CssValueBorderColorInherit,      important = True  } ])
  , ( "border-bottom-color: transparent",                 [CssDeclaration { property = CssPropertyBorderBottomColor CssValueBorderColorTransparent,  important = False } ])
  , ( "border-bottom-color: pink",                        [CssDeclaration { property = CssPropertyBorderBottomColor $ CssValueBorderColor 0xffc0cb,  important = False } ])
  , ( "border-bottom-color: rgb(0, 255, 0) !important",   [CssDeclaration { property = CssPropertyBorderBottomColor $ CssValueBorderColor 0x00ff00,  important = True  } ])
    -- Testing for parsing of bad css: typo in property name.
  , ( "border-bottom_color: rgb(0, 255, 0) !important",   [])

  , ( "border-left-color: inherit",                       [CssDeclaration { property = CssPropertyBorderLeftColor CssValueBorderColorInherit,        important = False } ])
  , ( "border-left-color: transparent !important",        [CssDeclaration { property = CssPropertyBorderLeftColor CssValueBorderColorTransparent,    important = True  } ])
  , ( "border-left-color: purple",                        [CssDeclaration { property = CssPropertyBorderLeftColor $ CssValueBorderColor 0x800080,    important = False } ])
  , ( "border-left-color: rgb(0, 0, 255) !important",     [CssDeclaration { property = CssPropertyBorderLeftColor $ CssValueBorderColor 0x0000ff,    important = True  } ])
    -- Testing for parsing of bad css: invalid value name.
  , ( "border-left-color: purpe",                         [])




  , ( "border-top-style: none !important",       [CssDeclaration { property = CssPropertyBorderTopStyle CssValueBorderStyleNone,          important = True  } ])
  , ( "border-top-style: hidden",                [CssDeclaration { property = CssPropertyBorderTopStyle CssValueBorderStyleHidden,        important = False } ])
  , ( "border-top-style: dotted !important",     [CssDeclaration { property = CssPropertyBorderTopStyle CssValueBorderStyleDotted,        important = True  } ])
  , ( "border-top-style: dashed",                [CssDeclaration { property = CssPropertyBorderTopStyle CssValueBorderStyleDashed,        important = False } ])
  , ( "border-top-style: solid !important",      [CssDeclaration { property = CssPropertyBorderTopStyle CssValueBorderStyleSolid,         important = True  } ])
  , ( "border-top-style: double",                [CssDeclaration { property = CssPropertyBorderTopStyle CssValueBorderStyleDouble,        important = False } ])
  , ( "border-top-style: groove !important",     [CssDeclaration { property = CssPropertyBorderTopStyle CssValueBorderStyleGroove,        important = True  } ])
  , ( "border-top-style: ridge",                 [CssDeclaration { property = CssPropertyBorderTopStyle CssValueBorderStyleRidge,         important = False } ])
  , ( "border-top-style: inset !important",      [CssDeclaration { property = CssPropertyBorderTopStyle CssValueBorderStyleInset,         important = True  } ])
  , ( "border-top-style: outset",                [CssDeclaration { property = CssPropertyBorderTopStyle CssValueBorderStyleOutset,        important = False } ])
  , ( "border-top-style: inherit !important",    [CssDeclaration { property = CssPropertyBorderTopStyle CssValueBorderStyleInherit,       important = True  } ])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "border-top-style: inherit !mportant",     [CssDeclaration { property = CssPropertyBorderTopStyle CssValueBorderStyleInherit,       important = False } ])

  , ( "border-right-style: none",                [CssDeclaration { property = CssPropertyBorderRightStyle CssValueBorderStyleNone,        important = False } ])
  , ( "border-right-style: hidden !important",   [CssDeclaration { property = CssPropertyBorderRightStyle CssValueBorderStyleHidden,      important = True  } ])
  , ( "border-right-style: dotted",              [CssDeclaration { property = CssPropertyBorderRightStyle CssValueBorderStyleDotted,      important = False } ])
  , ( "border-right-style: dashed !important",   [CssDeclaration { property = CssPropertyBorderRightStyle CssValueBorderStyleDashed,      important = True  } ])
  , ( "border-right-style: solid",               [CssDeclaration { property = CssPropertyBorderRightStyle CssValueBorderStyleSolid,       important = False } ])
  , ( "border-right-style: double !important",   [CssDeclaration { property = CssPropertyBorderRightStyle CssValueBorderStyleDouble,      important = True  } ])
  , ( "border-right-style: groove",              [CssDeclaration { property = CssPropertyBorderRightStyle CssValueBorderStyleGroove,      important = False } ])
  , ( "border-right-style: ridge !important",    [CssDeclaration { property = CssPropertyBorderRightStyle CssValueBorderStyleRidge,       important = True  } ])
  , ( "border-right-style: inset",               [CssDeclaration { property = CssPropertyBorderRightStyle CssValueBorderStyleInset,       important = False } ])
  , ( "border-right-style: outset !important",   [CssDeclaration { property = CssPropertyBorderRightStyle CssValueBorderStyleOutset,      important = True  } ])
  , ( "border-right-style: inherit",             [CssDeclaration { property = CssPropertyBorderRightStyle CssValueBorderStyleInherit,     important = False } ])
  -- Testing for parsing of bad css: invalid value.
  , ( "border-right-style: blue",                [])

  , ( "border-bottom-style: none !important",    [CssDeclaration { property = CssPropertyBorderBottomStyle CssValueBorderStyleNone,       important = True  } ])
  , ( "border-bottom-style: hidden",             [CssDeclaration { property = CssPropertyBorderBottomStyle CssValueBorderStyleHidden,     important = False } ])
  , ( "border-bottom-style: dotted !important",  [CssDeclaration { property = CssPropertyBorderBottomStyle CssValueBorderStyleDotted,     important = True  } ])
  , ( "border-bottom-style: dashed",             [CssDeclaration { property = CssPropertyBorderBottomStyle CssValueBorderStyleDashed,     important = False } ])
  , ( "border-bottom-style: solid !important",   [CssDeclaration { property = CssPropertyBorderBottomStyle CssValueBorderStyleSolid,      important = True  } ])
  , ( "border-bottom-style: double",             [CssDeclaration { property = CssPropertyBorderBottomStyle CssValueBorderStyleDouble,     important = False } ])
  , ( "border-bottom-style: groove !important",  [CssDeclaration { property = CssPropertyBorderBottomStyle CssValueBorderStyleGroove,     important = True  } ])
  , ( "border-bottom-style: ridge",              [CssDeclaration { property = CssPropertyBorderBottomStyle CssValueBorderStyleRidge,      important = False } ])
  , ( "border-bottom-style: inset !important",   [CssDeclaration { property = CssPropertyBorderBottomStyle CssValueBorderStyleInset,      important = True  } ])
  , ( "border-bottom-style: outset",             [CssDeclaration { property = CssPropertyBorderBottomStyle CssValueBorderStyleOutset,     important = False } ])
  , ( "border-bottom-style: inherit !important", [CssDeclaration { property = CssPropertyBorderBottomStyle CssValueBorderStyleInherit,    important = True  } ])
  -- Testing for parsing of bad css: typo in property name.
  , ( "order-bottom-style: inherit !important",  [])

  , ( "border-left-style: none",                 [CssDeclaration { property = CssPropertyBorderLeftStyle CssValueBorderStyleNone,         important = False } ])
  , ( "border-left-style: hidden !important",    [CssDeclaration { property = CssPropertyBorderLeftStyle CssValueBorderStyleHidden,       important = True  } ])
  , ( "border-left-style: dotted",               [CssDeclaration { property = CssPropertyBorderLeftStyle CssValueBorderStyleDotted,       important = False } ])
  , ( "border-left-style: dashed !important",    [CssDeclaration { property = CssPropertyBorderLeftStyle CssValueBorderStyleDashed,       important = True  } ])
  , ( "border-left-style: solid",                [CssDeclaration { property = CssPropertyBorderLeftStyle CssValueBorderStyleSolid,        important = False } ])
  , ( "border-left-style: double !important",    [CssDeclaration { property = CssPropertyBorderLeftStyle CssValueBorderStyleDouble,       important = True  } ])
  , ( "border-left-style: groove",               [CssDeclaration { property = CssPropertyBorderLeftStyle CssValueBorderStyleGroove,       important = False } ])
  , ( "border-left-style: ridge !important",     [CssDeclaration { property = CssPropertyBorderLeftStyle CssValueBorderStyleRidge,        important = True  } ])
  , ( "border-left-style: inset",                [CssDeclaration { property = CssPropertyBorderLeftStyle CssValueBorderStyleInset,        important = False } ])
  , ( "border-left-style: outset !important",    [CssDeclaration { property = CssPropertyBorderLeftStyle CssValueBorderStyleOutset,       important = True  } ])
  , ( "border-left-style: inherit",              [CssDeclaration { property = CssPropertyBorderLeftStyle CssValueBorderStyleInherit,      important = False } ])
  -- Testing for parsing of bad css: invalid value name.
  , ( "border-left-style: inheri !important",     [])




  , ( "border-top-width: inherit",                        [CssDeclaration { property = CssPropertyBorderTopWidth CssValueBorderWidthInherit,                               important = False } ])
  , ( "border-top-width: 1.0px",                          [CssDeclaration { property = CssPropertyBorderTopWidth (CssValueBorderWidthDistance (CssDistanceAbsPx 1.0)),     important = False } ])
  , ( "border-top-width: 2.0mm !important",               [CssDeclaration { property = CssPropertyBorderTopWidth (CssValueBorderWidthDistance (CssDistanceAbsMm 2.0)),     important = True  } ])
  -- Testing for parsing of bad css: invalid value.
  , ( "border-top-width: I.0px",                          [])

  , ( "border-right-width: inherit",                      [CssDeclaration { property = CssPropertyBorderRightWidth CssValueBorderWidthInherit,                             important = False } ])
  , ( "border-right-width: 1.5px !important",             [CssDeclaration { property = CssPropertyBorderRightWidth (CssValueBorderWidthDistance (CssDistanceAbsPx 1.5)),   important = True  } ])
  , ( "border-right-width: 2.0mm",                        [CssDeclaration { property = CssPropertyBorderRightWidth (CssValueBorderWidthDistance (CssDistanceAbsMm 2.0)),   important = False } ])
  -- Testing for parsing of bad css: invalid property name.
  , ( "border-rigth-width: 2.0mm",                        [])

  , ( "border-bottom-width: inherit !important",          [CssDeclaration { property = CssPropertyBorderBottomWidth CssValueBorderWidthInherit,                            important = True  } ])
  , ( "border-bottom-width: 1.0em",                       [CssDeclaration { property = CssPropertyBorderBottomWidth (CssValueBorderWidthDistance (CssDistanceRelEm 1.0)),  important = False } ])
  , ( "border-bottom-width: 2.0ex !important",            [CssDeclaration { property = CssPropertyBorderBottomWidth (CssValueBorderWidthDistance (CssDistanceRelEx 2.0)),  important = True  } ])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "border-bottom-width: 2.0ex !importan",             [CssDeclaration { property = CssPropertyBorderBottomWidth (CssValueBorderWidthDistance (CssDistanceRelEx 2.0)),  important = False  } ])

  , ( "border-left-width: inherit",                       [CssDeclaration { property = CssPropertyBorderLeftWidth CssValueBorderWidthInherit,                              important = False } ])
  , ( "border-left-width: 1.0em",                         [CssDeclaration { property = CssPropertyBorderLeftWidth (CssValueBorderWidthDistance (CssDistanceRelEm 1.0)),    important = False } ])
  , ( "border-left-width: 2.0ex !important",              [CssDeclaration { property = CssPropertyBorderLeftWidth (CssValueBorderWidthDistance (CssDistanceRelEx 2.0)),    important = True  } ])
  -- Testing for parsing of bad css: invalid value.
  , ( "border-left-width: anherit",                       [])




  , ( "color: inherit",                          [CssDeclaration { property = CssPropertyColor CssValueColorInherit,        important = False } ])
  , ( "color: inherit !important",               [CssDeclaration { property = CssPropertyColor CssValueColorInherit,        important = True  } ])
  , ( "color: red",                              [CssDeclaration { property = CssPropertyColor (CssValueColor 0xff0000),    important = False } ])
  , ( "color: lime !important",                  [CssDeclaration { property = CssPropertyColor (CssValueColor 0x00ff00),    important = True  } ]) -- Yes, "lime" not "green".
  , ( "color: blue !important;",                 [CssDeclaration { property = CssPropertyColor (CssValueColor 0x0000ff),    important = True  } ])
  , ( "color: #abcdef;",                         [CssDeclaration { property = CssPropertyColor (CssValueColor 0xabcdef),    important = False } ])




    -- For now only quoted strings are supported (with single or double quotes).
  , ( "content: \"\"",                           [CssDeclaration { property = CssPropertyContent (CssValueContent ""),        important = False } ])
  , ( "content: \"\" !important",                [CssDeclaration { property = CssPropertyContent (CssValueContent ""),        important = True } ])
  , ( "content: \"bullet\"",                     [CssDeclaration { property = CssPropertyContent (CssValueContent "bullet"),  important = False } ])
  , ( "content: \"bullet\" !important",          [CssDeclaration { property = CssPropertyContent (CssValueContent "bullet"),  important = True } ])
  , ( "content: \"train\"",                      [CssDeclaration { property = CssPropertyContent (CssValueContent "train"),   important = False } ])
  , ( "content: \"train\" !important",           [CssDeclaration { property = CssPropertyContent (CssValueContent "train"),   important = True } ])
  , ( "content: ''",                             [CssDeclaration { property = CssPropertyContent (CssValueContent ""),        important = False } ])
  , ( "content: '' !important",                  [CssDeclaration { property = CssPropertyContent (CssValueContent ""),        important = True } ])
  , ( "content: 'bus'",                          [CssDeclaration { property = CssPropertyContent (CssValueContent "bus"),     important = False } ])
  , ( "content: 'bus' !important",               [CssDeclaration { property = CssPropertyContent (CssValueContent "bus"),     important = True } ])
  , ( "content: 'car'",                          [CssDeclaration { property = CssPropertyContent (CssValueContent "car"),     important = False } ])
  , ( "content: 'car' !important",               [CssDeclaration { property = CssPropertyContent (CssValueContent "car"),     important = True } ])
    -- Testing for parsing of bad css: invalid property name.
  , ( "contet: \"bullet\"",                      [])
    -- Testing for parsing of bad css: invalid value.
  , ( "content: train",                          [])
    -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "content: \"bullet\" !improtant",          [CssDeclaration { property = CssPropertyContent (CssValueContent "bullet"),  important = False  } ])
  , ( "content: 'train' !improtant",             [CssDeclaration { property = CssPropertyContent (CssValueContent "train"),   important = False  } ])




  , ( "cursor: crosshair",            [CssDeclaration { property = CssPropertyCursor CssValueCursorCrosshair,   important = False } ])
  , ( "cursor: default !important",   [CssDeclaration { property = CssPropertyCursor CssValueCursorDefault,     important = True  } ])
  , ( "cursor: pointer",              [CssDeclaration { property = CssPropertyCursor CssValueCursorPointer,     important = False } ])
  , ( "cursor: move !important",      [CssDeclaration { property = CssPropertyCursor CssValueCursorMove,        important = True  } ])
  , ( "cursor: e-resize",             [CssDeclaration { property = CssPropertyCursor CssValueCursorEResize,     important = False } ])
  , ( "cursor: ne-resize !important", [CssDeclaration { property = CssPropertyCursor CssValueCursorNeResize,    important = True  } ])
  , ( "cursor: nw-resize",            [CssDeclaration { property = CssPropertyCursor CssValueCursorNwResize,    important = False } ])
  , ( "cursor: n-resize !important",  [CssDeclaration { property = CssPropertyCursor CssValueCursorNResize,     important = True  } ])
  , ( "cursor: se-resize",            [CssDeclaration { property = CssPropertyCursor CssValueCursorSeResize,    important = False } ])
  , ( "cursor: sw-resize !important", [CssDeclaration { property = CssPropertyCursor CssValueCursorSwResize,    important = True  } ])
  , ( "cursor: s-resize",             [CssDeclaration { property = CssPropertyCursor CssValueCursorSResize,     important = False } ])
  , ( "cursor: w-resize !important",  [CssDeclaration { property = CssPropertyCursor CssValueCursorWResize,     important = True  } ])
  , ( "cursor: text",                 [CssDeclaration { property = CssPropertyCursor CssValueCursorText,        important = False } ])
  , ( "cursor: wait !important",      [CssDeclaration { property = CssPropertyCursor CssValueCursorWait,        important = True  } ])
  , ( "cursor: help",                 [CssDeclaration { property = CssPropertyCursor CssValueCursorHelp,        important = False } ])
  -- Testing for parsing of bad css: invalid property name.
  , ( "cursr: crosshair",             [])
  -- Testing for parsing of bad css: invalid value.
  , ( "cursor: ponter",               [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "cursor: help !improtant",      [CssDeclaration { property = CssPropertyCursor CssValueCursorHelp,        important = False  } ])





  , ( "display: block",                         [CssDeclaration { property = CssPropertyDisplay CssValueDisplayBlock,              important = False } ])
  , ( "display: inline !important",             [CssDeclaration { property = CssPropertyDisplay CssValueDisplayInline,             important = True  } ])
  , ( "display: inline-block",                  [CssDeclaration { property = CssPropertyDisplay CssValueDisplayInlineBlock,        important = False } ])
  , ( "display: list-item !important",          [CssDeclaration { property = CssPropertyDisplay CssValueDisplayListItem,           important = True  } ])
  , ( "display: none",                          [CssDeclaration { property = CssPropertyDisplay CssValueDisplayNone,               important = False } ])
  , ( "display: table !important",              [CssDeclaration { property = CssPropertyDisplay CssValueDisplayTable,              important = True  } ])
  , ( "display: table-row-group",               [CssDeclaration { property = CssPropertyDisplay CssValueDisplayTableRowGroup,      important = False } ])
  , ( "display: table-header-group !important", [CssDeclaration { property = CssPropertyDisplay CssValueDisplayTableHeaderGroup,   important = True  } ])
  , ( "display: table-footer-group",            [CssDeclaration { property = CssPropertyDisplay CssValueDisplayTableFooterGroup,   important = False } ])
  , ( "display: table-row !important",          [CssDeclaration { property = CssPropertyDisplay CssValueDisplayTableRow,           important = True  } ])
  , ( "display: table-cell",                    [CssDeclaration { property = CssPropertyDisplay CssValueDisplayTableCell,          important = False } ])
  -- Testing for parsing of bad css: invalid property name.
  , ( "dsiplay: block",                         [])
  -- Testing for parsing of bad css: invalid value.
  , ( "display: rgb(0, 100, 200)",              [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "display: table !improtant",              [CssDeclaration { property = CssPropertyDisplay CssValueDisplayTable,              important = False  } ])




    -- CSS2.2: [ [ <'font-style'> || <'font-variant'> || <'font-weight'> ]? <'font-size'> [ / <'line-height'> ]? <'font-family'> ]
    --         | caption | icon | menu | message-box | small-caption | status-bar | inherit
  , ("font: italic small-caps 8px serif",       [ CssDeclaration { property = CssPropertyFontStyle $ CssValueFontStyleItalic,          important = False }
                                                , CssDeclaration { property = CssPropertyFontVariant $ CssValueFontVariantSmallCaps,   important = False }
                                                , CssDeclaration { property = CssPropertyFontSize $ CssValueFontSizeDistance $ CssDistanceAbsPx 8.0,          important = False }
                                                , CssDeclaration { property = CssPropertyFontFamily $ CssValueFontFamilyList ["serif"],          important = False }
                                                ])

    -- Absolute required minimum: font-size and font-family
  , ("font: 8px monospace",                     [ CssDeclaration { property = CssPropertyFontSize $ CssValueFontSizeDistance $ CssDistanceAbsPx 8.0,          important = False }
                                                , CssDeclaration { property = CssPropertyFontFamily $ CssValueFontFamilyList ["monospace"],          important = False }
                                                ])

    -- Invalid input: font-family is missing
  , ("font: small-caps 8px",                    [])


  -- TODO: because tokensAsValueStringList is too eager and consumes
  -- space-separated idents as if they were a part of a list, this test
  -- fails. "monospace inherit" is treated as two-items list. The
  -- tokensAsValueStringList function should treat a list of *comma*
  -- separated list of idents as a list.
{-
    -- Invalid input: absolute required minimum (font-size and font-family), but followed by 'inherit' that is exclusive with size/family.
  , ("font: 8px monospace inherit",             [])
-}



    -- TODO: "!important" keyword is not parsed correctly, fix it.
    -- TODO: rules for font-family are compilcated, but we don't support them well, fix it.
  , ("font-family: monospace",                  [CssDeclaration { property = CssPropertyFontFamily $ CssValueFontFamilyList ["monospace"],             important = False } ])
  , ("font-family: \"Comic Sans\", serif",      [CssDeclaration { property = CssPropertyFontFamily $ CssValueFontFamilyList ["Comic Sans", "serif"],   important = False } ])
  , ("font-family: 'My Font', cursive",         [CssDeclaration { property = CssPropertyFontFamily $ CssValueFontFamilyList ["My Font", "cursive"],    important = False } ])
  -- Testing for parsing of bad css: invalid property name.
  , ( "foht-family: monospace",                 [])




  , ("font-size: xx-small",            [CssDeclaration { property = CssPropertyFontSize CssValueFontSizeXXSmall,   important = False } ])
  , ("font-size: x-small !important",  [CssDeclaration { property = CssPropertyFontSize CssValueFontSizeXSmall,    important = True  } ])
  , ("font-size: small",               [CssDeclaration { property = CssPropertyFontSize CssValueFontSizeSmall,     important = False } ])
  , ("font-size: medium !important",   [CssDeclaration { property = CssPropertyFontSize CssValueFontSizeMedium,    important = True  } ])
  , ("font-size: large",               [CssDeclaration { property = CssPropertyFontSize CssValueFontSizeLarge,     important = False } ])
  , ("font-size: x-large !important",  [CssDeclaration { property = CssPropertyFontSize CssValueFontSizeXLarge,    important = True  } ])
  , ("font-size: xx-large",            [CssDeclaration { property = CssPropertyFontSize CssValueFontSizeXXLarge,   important = False } ])
  , ("font-size: larger !important",   [CssDeclaration { property = CssPropertyFontSize CssValueFontSizeLarger,    important = True  } ])
  , ("font-size: smaller",             [CssDeclaration { property = CssPropertyFontSize CssValueFontSizeSmaller,   important = False } ])
  -- Testing for parsing of bad css: invalid property name.
  , ( "font-site: small",              [])
  -- Testing for parsing of bad css: invalid value.
  , ( "font-size: square",             [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "font-size: large important",    [CssDeclaration { property = CssPropertyFontSize CssValueFontSizeLarge,     important = False  } ])




  , ("font-style: normal !important",    [CssDeclaration { property = CssPropertyFontStyle CssValueFontStyleNormal,   important = True  } ])
  , ("font-style: italic",               [CssDeclaration { property = CssPropertyFontStyle CssValueFontStyleItalic,   important = False } ])
  , ("font-style: oblique !important",   [CssDeclaration { property = CssPropertyFontStyle CssValueFontStyleOblique,  important = True  } ])
    -- Testing for parsing of bad css: invalid property name.
  , ( "font-syle: normal",               [])
  -- Testing for parsing of bad css: invalid value.
  , ( "font-style: obligue",             [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "font-style: normal !!important",  [CssDeclaration { property = CssPropertyFontStyle CssValueFontStyleNormal,   important = False  } ])




  , ("font-variant: normal !important",    [CssDeclaration { property = CssPropertyFontVariant CssValueFontVariantNormal,      important = True  } ])
  , ("font-variant: small-caps",           [CssDeclaration { property = CssPropertyFontVariant CssValueFontVariantSmallCaps,   important = False } ])
    -- Testing for parsing of bad css: invalid property name.
  , ( "font-wariant: normal",              [])
  -- Testing for parsing of bad css: invalid value.
  , ( "font-variant: xx-large",            [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "font-variant: normal !_mportant",   [CssDeclaration { property = CssPropertyFontVariant CssValueFontVariantNormal,      important = False  } ])




  , ("font-weight: normal",             [CssDeclaration { property = CssPropertyFontWeight CssValueFontWeightNormal,     important = False } ])
  , ("font-weight: bold !important",    [CssDeclaration { property = CssPropertyFontWeight CssValueFontWeightBold,       important = True  } ])
  , ("font-weight: bolder",             [CssDeclaration { property = CssPropertyFontWeight CssValueFontWeightBolder,     important = False } ])
  , ("font-weight: lighter !important", [CssDeclaration { property = CssPropertyFontWeight CssValueFontWeightLighter,    important = True  } ])
  , ("font-weight: 100 !important",     [CssDeclaration { property = CssPropertyFontWeight $ CssValueFontWeightInt 100,  important = True  } ])
  , ("font-weight: 900",                [CssDeclaration { property = CssPropertyFontWeight $ CssValueFontWeightInt 900,  important = False } ])

    -- Testing for parsing of bad css: invalid property name.
  , ("font-weigth: bold",               [])
    -- Testing for parsing of bad css: invalid value.
  , ("font-weight: light",              [])
  , ("font-weight: 1200",               [])
    -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "font-weight: normal !_mportant", [CssDeclaration { property = CssPropertyFontWeight CssValueFontWeightNormal,   important = False  } ])




  , ("height: auto",                     [CssDeclaration { property = CssPropertyHeight . CssValueHeightDistance $ CssDistanceAuto,                    important = False } ])
  , ("height: auto !important",          [CssDeclaration { property = CssPropertyHeight . CssValueHeightDistance $ CssDistanceAuto,                    important = True  } ])
  , ("height:   1px",                    [CssDeclaration { property = CssPropertyHeight (CssValueHeightDistance (CssDistanceAbsPx   1.0)),             important = False } ])
  , ("height:   1px !important",         [CssDeclaration { property = CssPropertyHeight (CssValueHeightDistance (CssDistanceAbsPx   1.0)),             important = True  } ])
  , ("height:  22.22mm",                 [CssDeclaration { property = CssPropertyHeight (CssValueHeightDistance (CssDistanceAbsMm  22.22)),            important = False } ])
  , ("height:  22.22mm !important",      [CssDeclaration { property = CssPropertyHeight (CssValueHeightDistance (CssDistanceAbsMm  22.22)),            important = True  } ])
  , ("height:  33.3em",                  [CssDeclaration { property = CssPropertyHeight (CssValueHeightDistance (CssDistanceRelEm  33.3)),             important = False } ])
  , ("height:  33.3em !important",       [CssDeclaration { property = CssPropertyHeight (CssValueHeightDistance (CssDistanceRelEm  33.3)),             important = True  } ])
  , ("height: 444.44ex",                 [CssDeclaration { property = CssPropertyHeight (CssValueHeightDistance (CssDistanceRelEx 444.44)),            important = False } ])
  , ("height: 444.44ex !important",      [CssDeclaration { property = CssPropertyHeight (CssValueHeightDistance (CssDistanceRelEx 444.44)),            important = True  } ])

    -- Testing for parsing of bad css: invalid property name.
  , ("heigth:  77.7em",                  [])
    -- Testing for parsing of bad css: invalid value.
  , ("height:  left",                    [])
    -- TODO: per CSS2.2 negative values are invalid. Fix this case in parser.
  , ("height: -500.0mm",                 [CssDeclaration { property = CssPropertyHeight (CssValueHeightDistance (CssDistanceAbsMm (-500.00))),         important = False } ])
    -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ("height:  22.22mm !importat",       [CssDeclaration { property = CssPropertyHeight (CssValueHeightDistance (CssDistanceAbsMm  22.22)),            important = False } ])




  , ("letter-spacing: normal",             [CssDeclaration { property = CssPropertyLetterSpacing CssValueLetterSpacingNormal,                                 important = False } ])
  , ("letter-spacing: normal !important",  [CssDeclaration { property = CssPropertyLetterSpacing CssValueLetterSpacingNormal,                                 important = True  } ])
  , ("letter-spacing: 10px",               [CssDeclaration { property = CssPropertyLetterSpacing (CssValueLetterSpacingDistance (CssDistanceAbsPx 10.0)),     important = False } ])
  , ("letter-spacing: 10px !important",    [CssDeclaration { property = CssPropertyLetterSpacing (CssValueLetterSpacingDistance (CssDistanceAbsPx 10.0)),     important = True  } ])
  , ("letter-spacing: -10px",              [CssDeclaration { property = CssPropertyLetterSpacing (CssValueLetterSpacingDistance (CssDistanceAbsPx (-10.0))),  important = False } ])
  , ("letter-spacing: -10px !important",   [CssDeclaration { property = CssPropertyLetterSpacing (CssValueLetterSpacingDistance (CssDistanceAbsPx (-10.0))),  important = True  } ])
  , ("letter-spacing: 5em",                [CssDeclaration { property = CssPropertyLetterSpacing (CssValueLetterSpacingDistance (CssDistanceRelEm 5.0)),      important = False } ])
  , ("letter-spacing: 5em !important",     [CssDeclaration { property = CssPropertyLetterSpacing (CssValueLetterSpacingDistance (CssDistanceRelEm 5.0)),      important = True  } ])

  -- Testing for parsing of bad css: invalid property name.
  , ("leter-spacing: normal",              [])
  -- Testing for parsing of bad css: invalid value.
  , ("letter-spacing: bold",               [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ("letter-spacing: normal !_omportant",  [CssDeclaration { property = CssPropertyLetterSpacing CssValueLetterSpacingNormal,                              important = False  } ])




  , ("line-height: normal",             [CssDeclaration { property = CssPropertyLineHeight CssValueLineHeightNormal,                                 important = False } ])
  , ("line-height: normal !important",  [CssDeclaration { property = CssPropertyLineHeight CssValueLineHeightNormal,                                 important = True  } ])
  , ("line-height: 10px",               [CssDeclaration { property = CssPropertyLineHeight (CssValueLineHeightDistance (CssDistanceAbsPx 10.0)),     important = False } ])
  , ("line-height: 10px !important",    [CssDeclaration { property = CssPropertyLineHeight (CssValueLineHeightDistance (CssDistanceAbsPx 10.0)),     important = True  } ])
  , ("line-height: -10px",              [CssDeclaration { property = CssPropertyLineHeight (CssValueLineHeightDistance (CssDistanceAbsPx (-10.0))),  important = False } ])
  , ("line-height: -10px !important",   [CssDeclaration { property = CssPropertyLineHeight (CssValueLineHeightDistance (CssDistanceAbsPx (-10.0))),  important = True  } ])
  , ("line-height: 5em",                [CssDeclaration { property = CssPropertyLineHeight (CssValueLineHeightDistance (CssDistanceRelEm 5.0)),      important = False } ])
  , ("line-height: 5em !important",     [CssDeclaration { property = CssPropertyLineHeight (CssValueLineHeightDistance (CssDistanceRelEm 5.0)),      important = True  } ])

  -- Testing for parsing of bad css: invalid property name.
  , ("line-heigth: normal",             [])
  -- Testing for parsing of bad css: invalid value.
  , ("line-height: bold",               [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ("line-height: normal important",  [CssDeclaration { property = CssPropertyLineHeight CssValueLineHeightNormal,                                important = False  } ])




    -- Notice that list-style-image is not supported by the parser and is not tested here.

{- -- This test item is disabled until a proper support for single "none" value is implemented.
  , ( "list-style: none",                    [ CssDeclaration { property = CssPropertyListStyle $ CssValueListStyle
                                                                { listStyleType     = initialValueListStyleType
                                                                , listStylePosition = initialValueListStylePosition
                                                                , listStyleImage    = initialValueListStyleImage
                                                                }
                                                              , important = False
                                                              }
                                             ])
-}
  , ( "list-style: disc inside",             [ CssDeclaration { property = CssPropertyListStyle $ CssValueListStyle
                                                                { listStyleType     = CssValueListStyleTypeDisc
                                                                , listStylePosition = CssValueListStylePositionInside
                                                                , listStyleImage    = initialValueListStyleImage
                                                                }
                                                              , important = False
                                                              }
                                             ])
  , ( "list-style: outside",                 [ CssDeclaration { property = CssPropertyListStyle $ CssValueListStyle
                                                                { listStyleType     = initialValueListStyleType
                                                                , listStylePosition = CssValueListStylePositionOutside
                                                                , listStyleImage    = initialValueListStyleImage
                                                                }
                                                              , important = False
                                                              }
                                             ])
  , ( "list-style: upper-roman outside",     [ CssDeclaration { property = CssPropertyListStyle $ CssValueListStyle
                                                                { listStyleType     = CssValueListStyleTypeUpperRoman
                                                                , listStylePosition = CssValueListStylePositionOutside
                                                                , listStyleImage    = initialValueListStyleImage
                                                                }
                                                              , important = False
                                                              }
                                             ])
    -- Same as above, but with flipped css value tokens.
  , ( "list-style: outside upper-roman",     [ CssDeclaration { property = CssPropertyListStyle $ CssValueListStyle
                                                                { listStyleType     = CssValueListStyleTypeUpperRoman
                                                                , listStylePosition = CssValueListStylePositionOutside
                                                                , listStyleImage    = initialValueListStyleImage
                                                                }
                                                              , important = False
                                                              }
                                             ])



  , ( "list-style-position: inside",                    [CssDeclaration { property = CssPropertyListStylePosition CssValueListStylePositionInside,   important = False } ])
  , ( "list-style-position: inside !important",         [CssDeclaration { property = CssPropertyListStylePosition CssValueListStylePositionInside,   important = True } ])
  , ( "list-style-position: outside",                   [CssDeclaration { property = CssPropertyListStylePosition CssValueListStylePositionOutside,  important = False } ])
  , ( "list-style-position: outside !important",        [CssDeclaration { property = CssPropertyListStylePosition CssValueListStylePositionOutside,  important = True } ])
  -- Testing for parsing of bad css: invalid value.
  , ( "list-style-position: outide !important",         [])




  , ( "list-style-type: disc !important",                 [CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeDisc,                important = True  } ])
  , ( "list-style-type: circle",                          [CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeCircle,              important = False } ])
  , ( "list-style-type: square !important",               [CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeSquare,              important = True  } ])
  , ( "list-style-type: decimal",                         [CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeDecimal,             important = False } ])
  , ( "list-style-type: decimal-leading-zero !important", [CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeDecimalLeadingZero,  important = True  } ])
  , ( "list-style-type: lower-roman",                     [CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeLowerRoman,          important = False } ])
  , ( "list-style-type: upper-roman !important",          [CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeUpperRoman,          important = True  } ])
  , ( "list-style-type: lower-greek",                     [CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeLowerGreek,          important = False } ])
  , ( "list-style-type: lower-alpha !important",          [CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeLowerAlpha,          important = True  } ])
  , ( "list-style-type: lower-latin",                     [CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeLowerLatin,          important = False } ])
  , ( "list-style-type: upper-alpha !important",          [CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeUpperAlpha,          important = True  } ])
  , ( "list-style-type: upper-latin",                     [CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeUpperLatin,          important = False } ])
  , ( "list-style-type: hebrew !important",               [CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeHebrew,              important = True  } ])
  , ( "list-style-type: armenian",                        [CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeArmenian,            important = False } ])
  , ( "list-style-type: georgian !important",             [CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeGeorgian,            important = True  } ])
  , ( "list-style-type: cjk-ideographic",                 [CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeCjkIdeographic,      important = False } ])
  , ( "list-style-type: hiragana !important",             [CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeHiragana,            important = True  } ])
  , ( "list-style-type: katakana",                        [CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeKatakana,            important = False } ])
  , ( "list-style-type: hiragana-iroha !important",       [CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeHiraganaIroha,       important = True  } ])
  , ( "list-style-type: katakana-iroha",                  [CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeKatakanaIroha,       important = False } ])
  , ( "list-style-type: none !important",                 [CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeNone,                important = True  } ])
  -- Testing for parsing of bad css: invalid property name.
  , ( "list-styletype: upper-latin",                      [])
  -- Testing for parsing of bad css: invalid value.
  , ( "list-style-type: lower-ronan",                     [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "list-style-type: none !improtant",                 [CssDeclaration { property = CssPropertyListStyleType CssValueListStyleTypeNone,                important = False  } ])




    -- All four values provided: top, right, bottom, left.
  , ("margin: 10.1px 20.2mm 30.3em 40.4ex",  [ CssDeclaration { property = CssPropertyMargin CssValueMargin
                                                                { marginTop    = CssValueMarginXDistance (CssDistanceAbsPx 10.1)
                                                                , marginRight  = CssValueMarginXDistance (CssDistanceAbsMm 20.2)
                                                                , marginBottom = CssValueMarginXDistance (CssDistanceRelEm 30.3)
                                                                , marginLeft   = CssValueMarginXDistance (CssDistanceRelEx 40.4)
                                                                }
                                                              , important = False }
                                             ])
    -- All four values provided: top, right, bottom, left, but with varying
    -- spaces between values.
  , ("margin:  10.1px   20.2mm  \t 30.3em \t\n 40.4ex",  [ CssDeclaration { property = CssPropertyMargin CssValueMargin
                                                                            { marginTop    = CssValueMarginXDistance (CssDistanceAbsPx 10.1)
                                                                            , marginRight  = CssValueMarginXDistance (CssDistanceAbsMm 20.2)
                                                                            , marginBottom = CssValueMarginXDistance (CssDistanceRelEm 30.3)
                                                                            , marginLeft   = CssValueMarginXDistance (CssDistanceRelEx 40.4)
                                                                            }
                                                                          , important = False }
                                                         ])
    -- Three values are provided: top, right-left, bottom.
  , ("margin: 11px 22mm 33.3em",             [ CssDeclaration { property = CssPropertyMargin CssValueMargin
                                                                { marginTop    = CssValueMarginXDistance (CssDistanceAbsPx 11.0)
                                                                , marginRight  = CssValueMarginXDistance (CssDistanceAbsMm 22.0)
                                                                , marginBottom = CssValueMarginXDistance (CssDistanceRelEm 33.3)
                                                                , marginLeft   = CssValueMarginXDistance (CssDistanceAbsMm 22.0)
                                                                }
                                                              , important = False }
                                             ])
    -- Two values are provided: top-bottom, right-left.
  , ("margin: 100px 200mm",                  [ CssDeclaration { property = CssPropertyMargin CssValueMargin
                                                                { marginTop    = CssValueMarginXDistance (CssDistanceAbsPx 100.0)
                                                                , marginRight  = CssValueMarginXDistance (CssDistanceAbsMm 200.0)
                                                                , marginBottom = CssValueMarginXDistance (CssDistanceAbsPx 100.0)
                                                                , marginLeft   = CssValueMarginXDistance (CssDistanceAbsMm 200.0)
                                                                }
                                                              , important = False }
                                             ])
    -- One value is provided: top-right-bottom-left.
  , ("margin: 38.01em",                      [ CssDeclaration { property = CssPropertyMargin CssValueMargin
                                                                { marginTop    = CssValueMarginXDistance (CssDistanceRelEm 38.01)
                                                                , marginRight  = CssValueMarginXDistance (CssDistanceRelEm 38.01)
                                                                , marginBottom = CssValueMarginXDistance (CssDistanceRelEm 38.01)
                                                                , marginLeft   = CssValueMarginXDistance (CssDistanceRelEm 38.01)
                                                                }
                                                              ,  important = False }
                                             ])
  -- Failure case: five values provided (while at most 4 expected).
  , ("margin: 10.1px 20.2mm 30.3em 40.4ex 50.5mm",  [])
  -- Failure cases: zero values provided (while at least 1 expected).
  , ("margin: ",   [])
  , ("margin: ;",  [])
  , ("margin: }",  [])




  , ( "margin-top: auto",                      [CssDeclaration { property = CssPropertyMarginTop . CssValueMarginXDistance $ CssDistanceAuto,              important = False } ])
  , ( "margin-top: auto !important",           [CssDeclaration { property = CssPropertyMarginTop . CssValueMarginXDistance $ CssDistanceAuto,              important = True  } ])
  , ( "margin-top:  1px",                      [CssDeclaration { property = CssPropertyMarginTop (CssValueMarginXDistance (CssDistanceAbsPx  1.0)),        important = False } ])
  , ( "margin-top:  2.2mm !important",         [CssDeclaration { property = CssPropertyMarginTop (CssValueMarginXDistance (CssDistanceAbsMm  2.2)),        important = True  } ])
  , ( "margin-top:  3.0em",                    [CssDeclaration { property = CssPropertyMarginTop (CssValueMarginXDistance (CssDistanceRelEm  3.0)),        important = False } ])
  , ( "margin-top: 93.0ex",                    [CssDeclaration { property = CssPropertyMarginTop (CssValueMarginXDistance (CssDistanceRelEx 93.0)),        important = False } ])
    -- Testing for parsing of bad css: invalid property name.
  , ( "margin-to: 11.0px",                     [])
    -- Testing for parsing of bad css: invalid value.
  , ( "margin-top: red",                       [])
    -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "margin-top: 26.6px !inportant",         [CssDeclaration { property = CssPropertyMarginTop (CssValueMarginXDistance (CssDistanceAbsPx 26.6)),        important = False  } ])




  , ( "margin-right: auto",                    [CssDeclaration { property = CssPropertyMarginRight . CssValueMarginXDistance $ CssDistanceAuto,            important = False } ])
  , ( "margin-right: auto !important",         [CssDeclaration { property = CssPropertyMarginRight . CssValueMarginXDistance $ CssDistanceAuto,            important = True  } ])
  , ( "margin-right: 111px",                   [CssDeclaration { property = CssPropertyMarginRight (CssValueMarginXDistance (CssDistanceAbsPx 111.0)),     important = False } ])
  , ( "margin-right: 222mm !important",        [CssDeclaration { property = CssPropertyMarginRight (CssValueMarginXDistance (CssDistanceAbsMm 222.0)),     important = True  } ])
  , ( "margin-right: 333.0em",                 [CssDeclaration { property = CssPropertyMarginRight (CssValueMarginXDistance (CssDistanceRelEm 333.0)),     important = False } ])
  , ( "margin-right: 444.0ex !important",      [CssDeclaration { property = CssPropertyMarginRight (CssValueMarginXDistance (CssDistanceRelEx 444.0)),     important = True  } ])
    -- Testing for parsing of bad css: invalid property name.
  , ( "margin-rigth: 11.0px",                  [])
    -- Testing for parsing of bad css: invalid value.
  , ( "margin-right: italic",                  [])
    -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "margin-right: 33.6px !inportant",       [CssDeclaration { property = CssPropertyMarginRight (CssValueMarginXDistance (CssDistanceAbsPx 33.6)),      important = False  } ])




  , ( "margin-bottom: auto",                   [CssDeclaration { property = CssPropertyMarginBottom . CssValueMarginXDistance $ CssDistanceAuto,           important = False } ])
  , ( "margin-bottom: auto !important",        [CssDeclaration { property = CssPropertyMarginBottom . CssValueMarginXDistance $ CssDistanceAuto,           important = True  } ])
  , ( "margin-bottom: 1.110px",                [CssDeclaration { property = CssPropertyMarginBottom (CssValueMarginXDistance (CssDistanceAbsPx 1.11)),     important = False } ])
  , ( "margin-bottom: 2.220mm !important",     [CssDeclaration { property = CssPropertyMarginBottom (CssValueMarginXDistance (CssDistanceAbsMm 2.22)),     important = True  } ])
  , ( "margin-bottom: 3.330em",                [CssDeclaration { property = CssPropertyMarginBottom (CssValueMarginXDistance (CssDistanceRelEm 3.33)),     important = False } ])
  , ( "margin-bottom: 4.440ex !important",     [CssDeclaration { property = CssPropertyMarginBottom (CssValueMarginXDistance (CssDistanceRelEx 4.44)),     important = True  } ])
    -- Testing for parsing of bad css: invalid property name.
  , ( "margin-botom: 11.0px",                  [])
    -- Testing for parsing of bad css: invalid value.
  , ( "margin-bottom: none",                   [])
    -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "margin-bottom: 33.6px !inportant",      [CssDeclaration { property = CssPropertyMarginBottom (CssValueMarginXDistance (CssDistanceAbsPx 33.6)),     important = False  } ])




  , ( "margin-left: auto",                     [CssDeclaration { property = CssPropertyMarginLeft . CssValueMarginXDistance $ CssDistanceAuto,             important = False } ])
  , ( "margin-left: auto !important",          [CssDeclaration { property = CssPropertyMarginLeft . CssValueMarginXDistance $ CssDistanceAuto,             important = True  } ])
  , ( "margin-left: 1.110px !important",       [CssDeclaration { property = CssPropertyMarginLeft (CssValueMarginXDistance (CssDistanceAbsPx 1.11)),       important = True  } ])
  , ( "margin-left: 2.220mm",                  [CssDeclaration { property = CssPropertyMarginLeft (CssValueMarginXDistance (CssDistanceAbsMm 2.22)),       important = False } ])
  , ( "margin-left: 3.330em !important",       [CssDeclaration { property = CssPropertyMarginLeft (CssValueMarginXDistance (CssDistanceRelEm 3.33)),       important = True  } ])
  , ( "margin-left: 4.440ex",                  [CssDeclaration { property = CssPropertyMarginLeft (CssValueMarginXDistance (CssDistanceRelEx 4.44)),       important = False } ])
    -- Testing for parsing of bad css: invalid property name.
  , ( "margin_left: 11.0px",                   [])
    -- Testing for parsing of bad css: invalid value.
  , ( "margin-left: latin",                    [])
    -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "margin-left: 33.6px !inportant",        [CssDeclaration { property = CssPropertyMarginLeft (CssValueMarginXDistance (CssDistanceAbsPx 33.6)),       important = False  } ])






      -- All four values provided: top, right, bottom, left.
  , ("padding: 10.1px 20.2mm 30.3em 40.4ex",  [ CssDeclaration { property = CssPropertyPadding CssValuePadding
                                                                { paddingTop    = CssValuePaddingX (CssDistanceAbsPx 10.1)
                                                                , paddingRight  = CssValuePaddingX (CssDistanceAbsMm 20.2)
                                                                , paddingBottom = CssValuePaddingX (CssDistanceRelEm 30.3)
                                                                , paddingLeft   = CssValuePaddingX (CssDistanceRelEx 40.4)
                                                                }
                                                              , important = False
                                                              }
                                             ])
    -- All four values provided: top, right, bottom, left, but with varying
    -- spaces between values.
  , ("padding:  10.1px   20.2mm  \t 30.3em \t\n 40.4ex",  [ CssDeclaration { property = CssPropertyPadding CssValuePadding
                                                                            { paddingTop    = CssValuePaddingX (CssDistanceAbsPx 10.1)
                                                                            , paddingRight  = CssValuePaddingX (CssDistanceAbsMm 20.2)
                                                                            , paddingBottom = CssValuePaddingX (CssDistanceRelEm 30.3)
                                                                            , paddingLeft   = CssValuePaddingX (CssDistanceRelEx 40.4)
                                                                            }
                                                                          , important = False
                                                                          }
                                                         ])
    -- Three values are provided: top, right-left, bottom.
  , ("padding: 11px 22mm 33.3em",             [ CssDeclaration { property = CssPropertyPadding CssValuePadding
                                                                { paddingTop    = CssValuePaddingX (CssDistanceAbsPx 11.0)
                                                                , paddingRight  = CssValuePaddingX (CssDistanceAbsMm 22.0)
                                                                , paddingBottom = CssValuePaddingX (CssDistanceRelEm 33.3)
                                                                , paddingLeft   = CssValuePaddingX (CssDistanceAbsMm 22.0)
                                                                }
                                                              , important = False
                                                              }
                                             ])
    -- Two values are provided: top-bottom, right-left.
  , ("padding: 100px 200mm",                  [ CssDeclaration { property = CssPropertyPadding CssValuePadding
                                                                { paddingTop    = CssValuePaddingX (CssDistanceAbsPx 100.0)
                                                                , paddingRight  = CssValuePaddingX (CssDistanceAbsMm 200.0)
                                                                , paddingBottom = CssValuePaddingX (CssDistanceAbsPx 100.0)
                                                                , paddingLeft   = CssValuePaddingX (CssDistanceAbsMm 200.0)
                                                                }
                                                              , important = False
                                                              }
                                             ])
    -- One value is provided: top-right-bottom-left.
  , ("padding: 38.01em",                      [ CssDeclaration { property = CssPropertyPadding CssValuePadding
                                                                { paddingTop    = CssValuePaddingX (CssDistanceRelEm 38.01)
                                                                , paddingRight  = CssValuePaddingX (CssDistanceRelEm 38.01)
                                                                , paddingBottom = CssValuePaddingX (CssDistanceRelEm 38.01)
                                                                , paddingLeft   = CssValuePaddingX (CssDistanceRelEm 38.01)
                                                                }
                                                              , important = False
                                                              }
                                             ])
  -- Failure case: five values provided (while at most 4 expected).
  , ("padding: 10.1px 20.2mm 30.3em 40.4ex 50.5mm",  [])
  -- Failure cases: zero values provided (while at least 1 expected).
  , ("padding: ",   [])
  , ("padding: ;",  [])
  , ("padding: }",  [])






  , ( "padding-top: 1.0px",                   [CssDeclaration { property = CssPropertyPaddingTop (CssValuePaddingX (CssDistanceAbsPx 1.0)),       important = False } ])
  , ( "padding-top: 2.3mm !important",        [CssDeclaration { property = CssPropertyPaddingTop (CssValuePaddingX (CssDistanceAbsMm 2.3)),       important = True  } ])
  , ( "padding-top: 4.5em",                   [CssDeclaration { property = CssPropertyPaddingTop (CssValuePaddingX (CssDistanceRelEm 4.5)),       important = False } ])
  -- Testing for parsing of bad css: invalid property name.
  , ( "padding-to: 1.0px",                    [])
  -- Testing for parsing of bad css: invalid value.
  , ( "padding-top: red",                     [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "padding-top: 6.6px !inportant",        [CssDeclaration { property = CssPropertyPaddingTop (CssValuePaddingX (CssDistanceAbsPx 6.6)),       important = False  } ])




  , ( "padding-right: 1.0px",                 [CssDeclaration { property = CssPropertyPaddingRight (CssValuePaddingX (CssDistanceAbsPx 1.0)),     important = False } ])
  , ( "padding-right: 2.3mm !important",      [CssDeclaration { property = CssPropertyPaddingRight (CssValuePaddingX (CssDistanceAbsMm 2.3)),     important = True  } ])
  , ( "padding-right: 4.5em",                 [CssDeclaration { property = CssPropertyPaddingRight (CssValuePaddingX (CssDistanceRelEm 4.5)),     important = False } ])
  -- Testing for parsing of bad css: invalid property name.
  , ( "padding-rig: 1.0px",                   [])
  -- Testing for parsing of bad css: invalid value.
  , ( "padding-right: red",                   [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "padding-right: 6.6px !inportant",      [CssDeclaration { property = CssPropertyPaddingRight (CssValuePaddingX (CssDistanceAbsPx 6.6)),     important = False  } ])




  , ( "padding-bottom: 1.0px",                [CssDeclaration { property = CssPropertyPaddingBottom (CssValuePaddingX (CssDistanceAbsPx 1.0)),    important = False } ])
  , ( "padding-bottom: 2.3mm !important",     [CssDeclaration { property = CssPropertyPaddingBottom (CssValuePaddingX (CssDistanceAbsMm 2.3)),    important = True  } ])
  , ( "padding-bottom: 4.5em",                [CssDeclaration { property = CssPropertyPaddingBottom (CssValuePaddingX (CssDistanceRelEm 4.5)),    important = False } ])
  -- Testing for parsing of bad css: invalid property name.
  , ( "padding-rig: 1.0px",                   [])
  -- Testing for parsing of bad css: invalid value.
  , ( "padding-bottom: red",                  [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "padding-bottom: 6.6px !inportant",     [CssDeclaration { property = CssPropertyPaddingBottom (CssValuePaddingX (CssDistanceAbsPx 6.6)),    important = False  } ])




  , ( "padding-left: 1.0px",                  [CssDeclaration { property = CssPropertyPaddingLeft (CssValuePaddingX (CssDistanceAbsPx 1.0)),      important = False } ])
  , ( "padding-left: 2.3mm !important",       [CssDeclaration { property = CssPropertyPaddingLeft (CssValuePaddingX (CssDistanceAbsMm 2.3)),      important = True  } ])
  , ( "padding-left: 4.5em",                  [CssDeclaration { property = CssPropertyPaddingLeft (CssValuePaddingX (CssDistanceRelEm 4.5)),      important = False } ])
  -- Testing for parsing of bad css: invalid property name.
  , ( "padding-rig: 1.0px",                   [])
  -- Testing for parsing of bad css: invalid value.
  , ( "padding-left: red",                    [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "padding-left: 6.6px !inportant",       [CssDeclaration { property = CssPropertyPaddingLeft (CssValuePaddingX (CssDistanceAbsPx 6.6)),      important = False  } ])




  , ( "text-indent:  1.1px !important",            [CssDeclaration { property = CssPropertyTextIndent (CssValueTextIndentDistance (CssDistanceAbsPx     1.1)),   important = True  } ])
  , ( "text-indent:  2.2mm",                       [CssDeclaration { property = CssPropertyTextIndent (CssValueTextIndentDistance (CssDistanceAbsMm     2.2)),   important = False } ])
  , ( "text-indent: 13.3em",                       [CssDeclaration { property = CssPropertyTextIndent (CssValueTextIndentDistance (CssDistanceRelEm    13.3)),   important = False } ])
  , ( "text-indent: 44.4ex !important",            [CssDeclaration { property = CssPropertyTextIndent (CssValueTextIndentDistance (CssDistanceRelEx    44.4)),   important = True  } ])
  -- From a real web page :)
  , ( "text-indent: -700em",                       [CssDeclaration { property = CssPropertyTextIndent (CssValueTextIndentDistance (CssDistanceRelEm (-700.0))),  important = False } ])

  -- Testing for parsing of bad css: invalid property name.
  , ( "test-indent: 55.5mm",                       [])
  -- Testing for parsing of bad css: invalid property value.
  , ( "text-indent: justify",                      [])
    -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ( "text-indent: 77.7ex !importan",             [CssDeclaration { property = CssPropertyTextIndent (CssValueTextIndentDistance (CssDistanceRelEx 77.7)),  important = False  } ])




  , ("text-align: left !important",       [CssDeclaration { property = CssPropertyTextAlign CssValueTextAlignLeft,     important = True  } ])
  , ("text-align: right",                 [CssDeclaration { property = CssPropertyTextAlign CssValueTextAlignRight,    important = False } ])
  , ("text-align: center !important",     [CssDeclaration { property = CssPropertyTextAlign CssValueTextAlignCenter,   important = True  } ])
  , ("text-align: justify",               [CssDeclaration { property = CssPropertyTextAlign CssValueTextAlignJustify,  important = False } ])
  -- Testing for parsing of bad css: invalid property name.
  , ("test-align: left",                  [])
  -- Testing for parsing of bad css: invalid value.
  , ("text-align: italic",                [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ("text-align: left !!important",      [CssDeclaration { property = CssPropertyTextAlign CssValueTextAlignLeft,     important = False  } ])




  , ("text-transform: none",                  [CssDeclaration { property = CssPropertyTextTransform CssValueTextTransformNone,        important = False } ])
  , ("text-transform: capitalize !important", [CssDeclaration { property = CssPropertyTextTransform CssValueTextTransformCapitalize,  important = True  } ])
  , ("text-transform: uppercase",             [CssDeclaration { property = CssPropertyTextTransform CssValueTextTransformUppercase,   important = False } ])
  , ("text-transform: lowercase !important",  [CssDeclaration { property = CssPropertyTextTransform CssValueTextTransformLowercase,   important = True  } ])
  -- Testing for parsing of bad css: invalid property name.
  , ("test-transform: none",                  [])
  -- Testing for parsing of bad css: invalid value.
  , ("text-transform: 1.0px",                 [])
  -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ("text-transform: uppercase _important",  [CssDeclaration { property = CssPropertyTextTransform CssValueTextTransformUppercase,   important = False  } ])




  -- First some simple cases, where only one value appears in input.
  , ("text-decoration: underline !important",     [CssDeclaration { property = CssPropertyTextDecoration [CssValueTextDecorationUnderline],    important = True  } ])
  , ("text-decoration: overline",                 [CssDeclaration { property = CssPropertyTextDecoration [CssValueTextDecorationOverline],     important = False } ])
  , ("text-decoration: line-through !important",  [CssDeclaration { property = CssPropertyTextDecoration [CssValueTextDecorationLineThrough],  important = True  } ])
  , ("text-decoration: blink",                    [CssDeclaration { property = CssPropertyTextDecoration [CssValueTextDecorationBlink],        important = False } ])

  -- Now few valid values. Notice that in different test cases the values appear in in different order.
  , ("text-decoration: underline overline line-through blink", [CssDeclaration { property = CssPropertyTextDecoration
                                                                                            [ CssValueTextDecorationUnderline
                                                                                            , CssValueTextDecorationOverline
                                                                                            , CssValueTextDecorationLineThrough
                                                                                            , CssValueTextDecorationBlink
                                                                                            ],
                                                                                 important = False } ])
  , ("text-decoration: blink overline underline line-through", [CssDeclaration { property = CssPropertyTextDecoration
                                                                                            [ CssValueTextDecorationBlink
                                                                                            , CssValueTextDecorationOverline
                                                                                            , CssValueTextDecorationUnderline
                                                                                            , CssValueTextDecorationLineThrough
                                                                                            ],
                                                                                 important = False } ])
  , ("text-decoration: overline line-through",                 [CssDeclaration { property = CssPropertyTextDecoration
                                                                                            [ CssValueTextDecorationOverline
                                                                                            , CssValueTextDecorationLineThrough
                                                                                            ],
                                                                                 important = False } ])
  , ("text-decoration: blink line-through !important",         [CssDeclaration { property = CssPropertyTextDecoration
                                                                                            [ CssValueTextDecorationBlink
                                                                                            , CssValueTextDecorationLineThrough
                                                                                            ],
                                                                                 important = True } ])

  -- Testing for parsing of bad css: invalid property name.
  , ("test-decoration: overline",                     [])
  -- Testing for parsing of bad css: invalid value. Notice that a single invalid value token invalidates entire property.
  , ("text-decoration: blue",                         [])
  , ("text-decoration: underline blue",               [])
  , ("text-decoration: 1.0px overline",               [])
  , ("text-decoration: underline italic blink",       [])
  , ("text-decoration: underline overline brink",     [])
  -- Testing for parsing of bad css: misspelled "important" word.
  --
  -- Notice that in this case the "_important" word is treated as one of
  -- possible decorations. The word is not a valid decoration, so entrie
  -- declaration is rejected.
  , ("text-decoration: underline _important",         [])




  , ( "vertical-align: top !important",       [CssDeclaration { property = CssPropertyVerticalAlign CssValueVerticalAlignTop,        important = True  } ])
  , ( "vertical-align: bottom",               [CssDeclaration { property = CssPropertyVerticalAlign CssValueVerticalAlignBottom,     important = False } ])
  , ( "vertical-align: middle !important",    [CssDeclaration { property = CssPropertyVerticalAlign CssValueVerticalAlignMiddle,     important = True  } ])
  , ( "vertical-align: baseline",             [CssDeclaration { property = CssPropertyVerticalAlign CssValueVerticalAlignBaseline,   important = False } ])
  , ( "vertical-align: sub !important",       [CssDeclaration { property = CssPropertyVerticalAlign CssValueVerticalAlignSub,        important = True  } ])
  , ( "vertical-align: super",                [CssDeclaration { property = CssPropertyVerticalAlign CssValueVerticalAlignSuper,      important = False } ])
  , ( "vertical-align: text-top !important",  [CssDeclaration { property = CssPropertyVerticalAlign CssValueVerticalAlignTextTop,    important = True  } ])
  , ( "vertical-align: text-bottom",          [CssDeclaration { property = CssPropertyVerticalAlign CssValueVerticalAlignTextBottom, important = False } ])
  -- Testing for parsing of bad css: invalid property name.
  , ( "ertical-align: pre",                   [])
  -- Testing for parsing of bad css: invalid value.
  , ( "vertical-align: suber",                [])
  -- Testing for parsing of bad css: incorrect value of "important" keyword. TODO: check how parser should behave here according to spec.
  , ( "vertical-align: top !!important",      [CssDeclaration { property = CssPropertyVerticalAlign CssValueVerticalAlignTop,        important = False } ])




  , ("width: auto",                     [CssDeclaration { property = CssPropertyWidth . CssValueWidthDistance $ CssDistanceAuto,                    important = False } ])
  , ("width: auto !important",          [CssDeclaration { property = CssPropertyWidth . CssValueWidthDistance $ CssDistanceAuto,                    important = True  } ])
  , ("width:   1px",                    [CssDeclaration { property = CssPropertyWidth (CssValueWidthDistance (CssDistanceAbsPx   1.0)),             important = False } ])
  , ("width:   1px !important",         [CssDeclaration { property = CssPropertyWidth (CssValueWidthDistance (CssDistanceAbsPx   1.0)),             important = True  } ])
  , ("width:  22.22mm",                 [CssDeclaration { property = CssPropertyWidth (CssValueWidthDistance (CssDistanceAbsMm  22.22)),            important = False } ])
  , ("width:  22.22mm !important",      [CssDeclaration { property = CssPropertyWidth (CssValueWidthDistance (CssDistanceAbsMm  22.22)),            important = True  } ])
  , ("width:  33.3em",                  [CssDeclaration { property = CssPropertyWidth (CssValueWidthDistance (CssDistanceRelEm  33.3)),             important = False } ])
  , ("width:  33.3em !important",       [CssDeclaration { property = CssPropertyWidth (CssValueWidthDistance (CssDistanceRelEm  33.3)),             important = True  } ])
  , ("width: 444.44ex",                 [CssDeclaration { property = CssPropertyWidth (CssValueWidthDistance (CssDistanceRelEx 444.44)),            important = False } ])
  , ("width: 444.44ex !important",      [CssDeclaration { property = CssPropertyWidth (CssValueWidthDistance (CssDistanceRelEx 444.44)),            important = True  } ])

    -- Testing for parsing of bad css: invalid property name.
  , ("widht:  77.7em",                  [])
    -- Testing for parsing of bad css: invalid value.
  , ("width:  left",                    [])
    -- TODO: per CSS2.2 negative values are invalid. Fix this case in parser.
  , ("width: -500.0mm",                 [CssDeclaration { property = CssPropertyWidth (CssValueWidthDistance (CssDistanceAbsMm (-500.00))),         important = False } ])
    -- Testing for parsing of bad css: misspelled "important" word. TODO: check how parser should behave here according to spec.
  , ("width:  22.22mm !importat",       [CssDeclaration { property = CssPropertyWidth (CssValueWidthDistance (CssDistanceAbsMm  22.22)),            important = False } ])




  , ( "white-space: normal !important",     [CssDeclaration { property = CssPropertyWhitespace CssValueWhitespaceNormal,   important = True  } ])
  , ( "white-space: pre",                   [CssDeclaration { property = CssPropertyWhitespace CssValueWhitespacePre,      important = False } ])
  , ( "white-space: nowrap !important",     [CssDeclaration { property = CssPropertyWhitespace CssValueWhitespaceNoWrap,   important = True  } ])
  , ( "white-space: pre-wrap",              [CssDeclaration { property = CssPropertyWhitespace CssValueWhitespacePreWrap,  important = False } ])
  , ( "white-space: pre-line  !important",  [CssDeclaration { property = CssPropertyWhitespace CssValueWhitespacePreLine,  important = True  } ])
  -- Testing for parsing of bad css: invalid property name.
  , ( "white-spac: pre",                    [])
  -- Testing for parsing of bad css: invalid value.
  , ( "white-space: prewrap",               [])
  -- Testing for parsing of bad css: incorrect value of "important" keyword. TODO: check how parser should behave here according to spec.
  , ( "white-space: pre important",         [CssDeclaration { property = CssPropertyWhitespace CssValueWhitespacePre,      important = False } ])




  , ( "word-spacing: normal !important",    [CssDeclaration { property = CssPropertyWordSpacing CssValueWordSpacingNormal,                              important = True  } ])
  , ( "word-spacing: 1.0px",                [CssDeclaration { property = CssPropertyWordSpacing (CssValueWordSpacingDistance (CssDistanceAbsPx 1.0)),   important = False } ])
  , ( "word-spacing: 2.5mm !important",     [CssDeclaration { property = CssPropertyWordSpacing (CssValueWordSpacingDistance (CssDistanceAbsMm 2.5)),   important = True  } ])
  , ( "word-spacing: 3.6em",                [CssDeclaration { property = CssPropertyWordSpacing (CssValueWordSpacingDistance (CssDistanceRelEm 3.6)),   important = False } ])
  , ( "word-spacing: 4.7ex !important",     [CssDeclaration { property = CssPropertyWordSpacing (CssValueWordSpacingDistance (CssDistanceRelEx 4.7)),   important = True  } ])
  -- Testing for parsing of bad css: invalid property name.
  , ( "words-pacing: normal !important",    [])
  -- Testing for parsing of bad css: invalid value. TODO: shouldn't "1" be considered a valid value?
  , ( "word-spacing: 1;0xz",                [])
  -- Testing for parsing of bad css: incorrect value of "important" keyword. TODO: check how parser should behave here according to spec.
  , ( "word-spacing: normal !importan",     [CssDeclaration { property = CssPropertyWordSpacing CssValueWordSpacingNormal,   important = False  } ])
  ]




-- On success return empty string. On failure return string showing
-- approximately where the problem is.
parseDeclarationTest :: [(T.Text, [CssDeclaration])] -> T.Text
parseDeclarationTest []     = ""
parseDeclarationTest (x:xs) = if expectedDeclarations /= declarations
                              then T.pack ("Got: " ++ show declarations ++ ", Expected: " ++ show expectedDeclarations)
                              else parseDeclarationTest xs
  where
    remd                 = fst x
    expectedDeclarations = snd x
    ((_parser', _token'), declarations) = parseSingleDeclaration pat

    -- This tests parses a declaration. Declaration is inside of {} block.
    -- Therefore construct a parser that has recognized that it is inside a
    -- block.
    pat = nextToken . defaultParserInBlock $ remd




{- -------------------------------------------------------------------------- -}




testCases =
  [
  -- If some error is found, test function returns some data (e.g. non-empty
  -- string or test index) which can help identify which test failed.
    TestCase (do
                 assertEqual "manual tests of declarations with parseDeclaration" "" (parseDeclarationTest parseDeclarationTestData))
  ]




testsCssParser :: IO String
testsCssParser = do
  testCounts <- runTestTT (TestList (testCases))
  if (errors testCounts + failures testCounts == 0)
    then return ""
    else return "[EE] Tests.Css.Parser failed"

