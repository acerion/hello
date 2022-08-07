{-
Copyright (C) 2021-2022 Kamil Ignacak acerion@wp.pl

This file is part of "hello" web browser.

"hello" is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

"hello" is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with "hello".  If not, see <https://www.gnu.org/licenses/>.

This file is derived from dillo-3.0.5/src/css.hh.
Copyright assignment from css.cc:
 Copyright 2008-2014 Johannes Hofmann <Johannes.Hofmann@gmx.de>
-}




{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-} -- For 'Data'. https://stackoverflow.com/questions/47861648/a-general-way-of-comparing-constructors-of-two-terms-in-haskell




module Hello.Css.Declaration
  (
    CssDeclaration (..)

  , CssValueBackgroundAttachment (..)
  , CssValueBackgroundColor (..)
  , CssValueBackgroundImage (..)
  , CssValueBackgroundPosition (..)
  , CssValueBackgroundRepeat (..)
  , CssValueBorderCollapse (..)
  , CssValueBorderColor (..)
  , CssValueBorderSpacing (..)
  , CssValueBorderStyle (..)
  , CssValueBorderWidth (..)
  , CssValueColor (..)
  , CssValueDisplay (..)
  , CssValueCursor (..)
  , CssValueFontFamily (..)
  , CssValueFontSize (..)
  , CssValueFontStyle (..)
  , CssValueFontVariant (..)
  , CssValueFontWeight (..)
  , CssValueLetterSpacing (..)
  , CssValueLineHeight (..)
  , CssValueListStylePosition (..)
  , CssValueListStyleType (..)
  , CssValuePadding (..)
  , CssValueTextAlign (..)
  , CssValueTextDecoration (..)
  , CssValueTextIndent (..)
  , CssValueTextTransform (..)
  , CssValueVerticalAlign (..)
  , CssValueWhitespace (..)
  , CssValueWordSpacing (..)
  , CssValueXLink (..)

  , makeCssDeclarationBackgroundAttachment
  , makeCssDeclarationBackgroundColor
  , makeCssDeclarationBackgroundImage
  , makeCssDeclarationBackgroundPosition
  , makeCssDeclarationBackgroundRepeat


  , makeCssDeclarationBorderCollapse
  , makeCssDeclarationBorderSpacing

  , makeCssDeclarationBorderTopColor
  , makeCssDeclarationBorderRightColor
  , makeCssDeclarationBorderBottomColor
  , makeCssDeclarationBorderLeftColor

  , makeCssDeclarationBorderTopStyle
  , makeCssDeclarationBorderRightStyle
  , makeCssDeclarationBorderBottomStyle
  , makeCssDeclarationBorderLeftStyle

  , makeCssDeclarationBorderTopWidth
  , makeCssDeclarationBorderRightWidth
  , makeCssDeclarationBorderBottomWidth
  , makeCssDeclarationBorderLeftWidth

  , makeCssDeclarationBottom
  , makeCssDeclarationCaptionSide
  , makeCssDeclarationClear
  , makeCssDeclarationClip
  , makeCssDeclarationColor
  , makeCssDeclarationContent
  , makeCssDeclarationCounterIncrement
  , makeCssDeclarationCounterReset
  , makeCssDeclarationCursor
  , makeCssDeclarationDirection
  , makeCssDeclarationDisplay
  , makeCssDeclarationEmptyCells
  , makeCssDeclarationFloat
  , makeCssDeclarationFontFamily
  , makeCssDeclarationFontSize
  , makeCssDeclarationFontSizeAdjust
  , makeCssDeclarationFontStretch
  , makeCssDeclarationFontStyle
  , makeCssDeclarationFontVariant
  , makeCssDeclarationFontWeight
  , makeCssDeclarationHeight
  , makeCssDeclarationLeft
  , makeCssDeclarationLetterSpacing
  , makeCssDeclarationLineHeight
  , makeCssDeclarationListStyleImage
  , makeCssDeclarationListStylePosition
  , makeCssDeclarationListStyleType
  , makeCssDeclarationMarginBottom
  , makeCssDeclarationMarginLeft
  , makeCssDeclarationMarginRight
  , makeCssDeclarationMarginTop
  , makeCssDeclarationMarkerOffset
  , makeCssDeclarationMarks
  , makeCssDeclarationMaxHeight
  , makeCssDeclarationMaxWidth
  , makeCssDeclarationMinHeight
  , makeCssDeclarationMinWidth
  , makeCssDeclarationOutlineColor
  , makeCssDeclarationOutlineStyle
  , makeCssDeclarationOutlineWidth
  , makeCssDeclarationOverflow
  , makeCssDeclarationPaddingBottom
  , makeCssDeclarationPaddingLeft
  , makeCssDeclarationPaddingRight
  , makeCssDeclarationPaddingTop
  , makeCssDeclarationPosition
  , makeCssDeclarationQuotes
  , makeCssDeclarationRight
  , makeCssDeclarationTextAlign
  , makeCssDeclarationTextDecoration
  , makeCssDeclarationTextIndent
  , makeCssDeclarationTextShadow
  , makeCssDeclarationTextTransform
  , makeCssDeclarationTop
  , makeCssDeclarationUnicodeBiDi
  , makeCssDeclarationVerticalAlign
  , makeCssDeclarationVisibility
  , makeCssDeclarationWhitespace
  , makeCssDeclarationWidth
  , makeCssDeclarationWordSpacing
  , makeCssDeclarationZIndex
  , makeCssDeclarationXColSpan
  , makeCssDeclarationXRowSpan
  , makeCssDeclarationXLang
  , makeCssDeclarationXImg
  , makeCssDeclarationXTooltip
  , makeCssDeclaration_LAST

  , defaultDeclaration
  , CssDeclWrapper (..)

  , makeCssDeclarationBorder
  , parseTokensAsBorderWidthValue
  , parseTokensAsBorderStyleValue
  , parseTokensAsBorderColorValue
  , parseTokensAsPaddingValue
  )
where




import Debug.Trace

import Data.Data
import Data.Maybe
import Data.List as L
import Data.Text as T

import Hello.Css.Distance
import Hello.Css.ParserHelpers
import Hello.Css.Tokenizer
import Hello.Css.Value

import Hello.Utils




data CssDeclWrapper = CssDeclWrapper
  { property  :: CssDeclaration

  -- https://www.w3.org/TR/css-syntax-3
  --
  -- "If the last two non-<whitespace-token>s in the declaration’s value are
  -- a <delim-token> with the value "!" followed by an <ident-token> with a
  -- value that is an ASCII case-insensitive match for "important", remove
  -- them from the declaration’s value and set the declaration’s important
  -- flag to true."
  --
  -- So "important" is per-declaration flag.
  , important :: Bool
  } deriving (Show, Eq)




defaultDeclaration = CssDeclWrapper
  { property  = CssDeclaration_LAST -- TODO: make it "CssDeclarationInvalid'; TODO: somewhere there is a code that does not set property2 field.
  , important = False
  }




-- A property in css declaration, but with a value.
data CssDeclaration
  = CssDeclarationBackgroundAttachment CssValueBackgroundAttachment      -- 0    parsing is unit-tested
  | CssDeclarationBackgroundColor CssValueBackgroundColor                -- 1    parsing is unit-tested
  | CssDeclarationBackgroundImage CssValueBackgroundImage                -- 2    This property is barely unit-tested because some decisions need to be made first.
  | CssDeclarationBackgroundPosition CssValueBackgroundPosition          -- 3    There are some unit tests, but they don't really test much.
  | CssDeclarationBackgroundRepeat CssValueBackgroundRepeat              -- 4
  | CssDeclarationBorderBottomColor CssValueBorderColor -- 5                parsing is tested
  | CssDeclarationBorderBottomStyle CssValueBorderStyle -- 6                parsing is tested
  | CssDeclarationBorderBottomWidth CssValueBorderWidth -- 7                parsing is tested
  | CssDeclarationBorderCollapse CssValueBorderCollapse -- 8                parsing is unit-tested
  | CssDeclarationBorderLeftColor CssValueBorderColor   -- 9                parsing is tested
  | CssDeclarationBorderLeftStyle CssValueBorderStyle   -- 10               parsing is tested
  | CssDeclarationBorderLeftWidth CssValueBorderWidth   -- 11               parsing is tested
  | CssDeclarationBorderRightColor CssValueBorderColor  -- 12               parsing is tested
  | CssDeclarationBorderRightStyle CssValueBorderStyle  -- 13               parsing is tested
  | CssDeclarationBorderRightWidth CssValueBorderWidth  -- 14               parsing is tested
  | CssDeclarationBorderSpacing CssValueBorderSpacing   -- 15               parsing is unit-tested
  | CssDeclarationBorderTopColor CssValueBorderColor    -- 16               parsing is tested
  | CssDeclarationBorderTopStyle CssValueBorderStyle    -- 17               parsing is tested
  | CssDeclarationBorderTopWidth CssValueBorderWidth    -- 18               parsing is tested
  | CssDeclarationBottom CssValue                       -- 19
  | CssDeclarationCaptionSide CssValue                  -- 20
  | CssDeclarationClear CssValue                        -- 21
  | CssDeclarationClip CssValue                         -- 22
  | CssDeclarationColor CssValueColor                   -- 23               parsing is tested
  | CssDeclarationContent CssValue                      -- 24
  | CssDeclarationCounterIncrement CssValue             -- 25
  | CssDeclarationCounterReset CssValue                 -- 26
  | CssDeclarationCursor CssValueCursor                 -- 27               parsing is unit-tested
  | CssDeclarationDirection CssValue                    -- 28
  | CssDeclarationDisplay CssValueDisplay               -- 29               parsing is unit-tested
  | CssDeclarationEmptyCells CssValue                   -- 30
  | CssDeclarationFloat CssValue                        -- 31
  | CssDeclarationFontFamily CssValueFontFamily         -- 32               parsing is unit-tested (poorly)
  | CssDeclarationFontSize CssValueFontSize             -- 33               parsing is unit-tested
  | CssDeclarationFontSizeAdjust CssValue               -- 34
  | CssDeclarationFontStretch CssValue                  -- 35
  | CssDeclarationFontStyle CssValueFontStyle           -- 36               parsing is unit-tested
  | CssDeclarationFontVariant CssValueFontVariant       -- 37               parsing is unit-tested
  | CssDeclarationFontWeight CssValueFontWeight         -- 38               parsing is unit-tested
  | CssDeclarationHeight CssValue                       -- 39
  | CssDeclarationLeft CssValue                         -- 40
  | CssDeclarationLetterSpacing CssValueLetterSpacing   -- 41               parsing is unit-tested
  | CssDeclarationLineHeight CssValueLineHeight         -- 42               parsing is unit-tested
  | CssDeclarationListStyleImage CssValue               -- 43               not supported by hello
  | CssDeclarationListStylePosition CssValueListStylePosition  -- 44        parsing is unit-tested
  | CssDeclarationListStyleType CssValueListStyleType   -- 45               parsing is unit-tested
  | CssDeclarationMarginBottom CssValue                 -- 46
  | CssDeclarationMarginLeft CssValue                   -- 47
  | CssDeclarationMarginRight CssValue                  -- 48
  | CssDeclarationMarginTop CssValue                    -- 49
  | CssDeclarationMarkerOffset CssValue                 -- 50
  | CssDeclarationMarks CssValue                        -- 51
  | CssDeclarationMaxHeight CssValue                    -- 52
  | CssDeclarationMaxWidth CssValue                     -- 53
  | CssDeclarationMinHeight CssValue                    -- 54
  | CssDeclarationMinWidth CssValue                     -- 55
  | CssDeclarationOutlineColor CssValue                 -- 56
  | CssDeclarationOutlineStyle CssValue                 -- 57
  | CssDeclarationOutlineWidth CssValue                 -- 58
  | CssDeclarationOverflow CssValue                     -- 59
  | CssDeclarationPaddingTop CssValuePadding            -- 63               parsing is unit-tested
  | CssDeclarationPaddingRight CssValuePadding          -- 62               parsing is unit-tested
  | CssDeclarationPaddingBottom CssValuePadding         -- 60               parsing is unit-tested
  | CssDeclarationPaddingLeft CssValuePadding           -- 61               parsing is unit-tested
  | CssDeclarationPosition CssValue                     -- 64
  | CssDeclarationQuotes CssValue                       -- 65
  | CssDeclarationRight CssValue                        -- 66
  | CssDeclarationTextAlign CssValueTextAlign           -- 67
  | CssDeclarationTextDecoration [CssValueTextDecoration] -- 68             Parsing is unit-tested. Using a list type because a set of values is allowed for this property.
  | CssDeclarationTextIndent CssValueTextIndent         -- 69               Parsing is unit-tested.
  | CssDeclarationTextShadow CssValue                   -- 70
  | CssDeclarationTextTransform CssValueTextTransform   -- 71               parsing is unit-tested
  | CssDeclarationTop CssValue                          -- 72
  | CssDeclarationUnicodeBiDi CssValue                  -- 73
  | CssDeclarationVerticalAlign CssValueVerticalAlign   -- 74               parsing is unit-tested
  | CssDeclarationVisibility CssValue                   -- 75
  | CssDeclarationWhitespace CssValueWhitespace         -- 76               parsing is unit-tested
  | CssDeclarationWidth CssValue                        -- 77
  | CssDeclarationWordSpacing CssValueWordSpacing       -- 78               parsing is unit-tested
  | CssDeclarationZIndex CssValue                       -- 79

  -- Pseudo-property used internally by dillo/hello. Without it following
  -- a/href links won't work.
  | CssDeclarationXLink CssValueXLink                   -- 80

  | CssDeclarationXColSpan CssValue                     -- 81
  | CssDeclarationXRowSpan CssValue                     -- 82

  -- Pseudo-property for "lang" or "xml:lang" attribute of html element.
  | CssDeclarationXLang CssValue                        -- 83

  -- Pseudo-property used (probably) to index images in a html document.
  | CssDeclarationXImg CssValue                         -- 84
  | CssDeclarationXTooltip CssValue                     -- 85

  | CssDeclaration_LAST                                 -- 86
  deriving (Eq, Show, Data)




-- ------------------------------------------------
-- Background attachment (background-attachment)
-- ------------------------------------------------




data CssValueBackgroundAttachment
  = CssValueBackgroundAttachmentScroll
  | CssValueBackgroundAttachmentFixed
  deriving (Enum, Eq, Show, Data)




makeCssDeclarationBackgroundAttachment :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclaration)
makeCssDeclarationBackgroundAttachment pat = (pat', fmap CssDeclarationBackgroundAttachment declValue)
  where
    (vs', declValue) = tokensAsValueEnumString3 vs
    pat'             = pt3 vs'

    vs :: ValueState3 CssValueBackgroundAttachment
    vs = (defaultValueState3 pat) { enums3 = [ ("scroll",  CssValueBackgroundAttachmentScroll)
                                             , ("fixed",   CssValueBackgroundAttachmentFixed)
                                             ]
                                  }




-- ------------------------------------------------
-- Background color (background-color)
-- ------------------------------------------------




data CssValueBackgroundColor
  = CssValueBackgroundColorInherit
  | CssValueBackgroundColorColor Int -- TODO: Int or Color?
  deriving (Eq, Show, Data)




makeCssDeclarationBackgroundColor :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclaration)
makeCssDeclarationBackgroundColor pat = (pat', fmap CssDeclarationBackgroundColor value)
  where
    (vs', value) = tokensAsValueEnumString3 vs >>? tokensAsValueColor3
    pat'         = pt3 vs'

    vs :: ValueState3 CssValueBackgroundColor
    vs = (defaultValueState3 pat) { colorValueCtor3 = Just CssValueBackgroundColorColor
                                  , enums3 = [ ("inherit",    CssValueBackgroundColorInherit)
                                             ]
                                  }




-- ------------------------------------------------
-- Background image (background-image)
-- ------------------------------------------------




data CssValueBackgroundImage
 = CssValueBackgroundImageUri T.Text -- TODO: change from T.Text to URI abstract type
 deriving (Data, Eq, Show)




makeCssDeclarationBackgroundImage :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclaration)
makeCssDeclarationBackgroundImage pat = (pat', fmap CssDeclarationBackgroundImage declValue)
  where
    (vs', declValue) = declValueAsURI3 vs
    pat'             = pt3 vs'

    vs :: ValueState3 CssValueBackgroundImage
    vs = (defaultValueState3 pat) { uriValueCtor = Just CssValueBackgroundImageUri
                                  }




-- ------------------------------------------------
-- Background position (background-position)
-- ------------------------------------------------




data CssValueBackgroundPosition
 = CssValueBackgroundPositionXY Int Int
 deriving (Data, Eq, Show)




makeCssDeclarationBackgroundPosition :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclaration)
makeCssDeclarationBackgroundPosition pat = (pat', fmap CssDeclarationBackgroundPosition declValue)
  where
    (vs', declValue) = tokensAsValueBgPosition3 vs
    pat'             = pt3 vs'

    vs :: ValueState3 CssValueBackgroundPosition
    vs = (defaultValueState3 pat) { bgPositionValueCtor = Just CssValueBackgroundPositionXY
                                  }




-- ------------------------------------------------
-- Background repeat (background-repeat)
-- ------------------------------------------------




data CssValueBackgroundRepeat
  = CssValueBackgroundRepeatRepeat
  | CssValueBackgroundRepeatRepeatX
  | CssValueBackgroundRepeatRepeatY
  | CssValueBackgroundRepeatNoRepeat
  deriving (Data, Enum, Eq, Show)




makeCssDeclarationBackgroundRepeat :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclaration)
makeCssDeclarationBackgroundRepeat pat = (pat', fmap CssDeclarationBackgroundRepeat declValue)
  where
    (vs', declValue) = tokensAsValueEnumString3 vs
    pat'             = pt3 vs'

    vs :: ValueState3 CssValueBackgroundRepeat
    vs = (defaultValueState3 pat) { enums3 = [ ("repeat",     CssValueBackgroundRepeatRepeat)
                                             , ("repeat-x",   CssValueBackgroundRepeatRepeatX)
                                             , ("repeat-y",   CssValueBackgroundRepeatRepeatY)
                                             , ("no-repeat",  CssValueBackgroundRepeatNoRepeat)
                                             ]
                                  }




-- ------------------------------------------------
-- Border collapse (border-collapse)
-- ------------------------------------------------




data CssValueBorderCollapse
  = CssValueBorderCollapseSeparate
  | CssValueBorderCollapseCollapse
  deriving (Bounded, Data, Enum, Eq, Show)




makeCssDeclarationBorderCollapse :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclaration)
makeCssDeclarationBorderCollapse pat = (pat', fmap CssDeclarationBorderCollapse declValue)
  where
    (vs', declValue) = tokensAsValueEnumString3 vs
    pat'             = pt3 vs'

    vs :: ValueState3 CssValueBorderCollapse
    vs = (defaultValueState3 pat) { enums3 = [ ("separate",   CssValueBorderCollapseSeparate)
                                             , ("collapse",   CssValueBorderCollapseCollapse)
                                             ]
                                  }




-- ------------------------------------------------
--
-- ------------------------------------------------




data CssValueBorderSpacing
 = CssValueBorderSpacingDistance CssDistance
 deriving (Eq, Show, Data)




makeCssDeclarationBorderSpacing :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclaration)
makeCssDeclarationBorderSpacing pat =  (pat', fmap CssDeclarationBorderSpacing declValue)
  where
    (vs', declValue) = declValueAsLength3 vs
    pat'             = pt3 vs'

    vs :: ValueState3 CssValueBorderSpacing
    vs = (defaultValueState3 pat) {  distanceValueCtor = Just CssValueBorderSpacingDistance
                                  }




-- ----------------
-- Border Color
-- ----------------




-- Here is a tricky question: should I make separate types for colors of
-- Bottom/Top/Left/Right, or can I get away with common type for all four
-- properties?
data CssValueBorderColor
  = CssValueBorderColorInherit
  | CssValueBorderColorTransparent
  | CssValueBorderColor Int -- TODO: Int or Color?
  deriving (Eq, Show, Data)




cssValueBorderColorDict :: [(T.Text, CssValueBorderColor)]
cssValueBorderColorDict = [ ("transparent", CssValueBorderColorTransparent)
                          , ("inherit",     CssValueBorderColorInherit)
                          ]




parseTokensAsBorderColorValue :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssValueBorderColor)
parseTokensAsBorderColorValue pat = (pat', declarationValue)
  where
    (vs', declarationValue) = tokensAsValueEnumString3 vs >>? tokensAsValueColor3
    pat' = pt3 vs'

    vs :: ValueState3 CssValueBorderColor
    vs = (defaultValueState3 pat) { colorValueCtor3 = Just CssValueBorderColor
                                  , enums3          = cssValueBorderColorDict
                                  }




makeCssDeclarationBorderXColor :: (CssValueBorderColor -> CssDeclaration) -> (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclaration)
makeCssDeclarationBorderXColor declCtor (parser, token) = ((parser', token'), fmap declCtor declarationValue)
  where
    ((parser', token'), declarationValue) = parseTokensAsBorderColorValue (parser, token)




makeCssDeclarationBorderTopColor    = makeCssDeclarationBorderXColor CssDeclarationBorderTopColor
makeCssDeclarationBorderRightColor  = makeCssDeclarationBorderXColor CssDeclarationBorderRightColor
makeCssDeclarationBorderBottomColor = makeCssDeclarationBorderXColor CssDeclarationBorderBottomColor
makeCssDeclarationBorderLeftColor   = makeCssDeclarationBorderXColor CssDeclarationBorderLeftColor




-- ------------------------------------------------
-- Border Style
-- https://www.w3.org/TR/CSS22/box.html#border-style-properties
-- ------------------------------------------------





data CssValueBorderStyle
  = CssValueBorderStyleNone
  | CssValueBorderStyleHidden
  | CssValueBorderStyleDotted
  | CssValueBorderStyleDashed
  | CssValueBorderStyleSolid
  | CssValueBorderStyleDouble
  | CssValueBorderStyleGroove
  | CssValueBorderStyleRidge
  | CssValueBorderStyleInset
  | CssValueBorderStyleOutset
  | CssValueBorderStyleInherit
  deriving (Eq, Show, Data)




cssValueBorderStyleDict :: [(T.Text, CssValueBorderStyle)]
cssValueBorderStyleDict = [ ("none",     CssValueBorderStyleNone)
                          , ("hidden",   CssValueBorderStyleHidden)
                          , ("dotted",   CssValueBorderStyleDotted)
                          , ("dashed",   CssValueBorderStyleDashed)
                          , ("solid",    CssValueBorderStyleSolid)
                          , ("double",   CssValueBorderStyleDouble)
                          , ("groove",   CssValueBorderStyleGroove)
                          , ("ridge",    CssValueBorderStyleRidge)
                          , ("inset",    CssValueBorderStyleInset)
                          , ("outset",   CssValueBorderStyleOutset)
                          , ("inherit",  CssValueBorderStyleInherit)
                          ]




parseTokensAsBorderStyleValue :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssValueBorderStyle)
parseTokensAsBorderStyleValue (pat) = ((pat'), declValue)
  where
    (vs', declValue) = tokensAsValueEnumString3 vs
    pat'             = pt3 vs'

    vs :: ValueState3 CssValueBorderStyle
    vs = (defaultValueState3 pat) { enums3 = cssValueBorderStyleDict
                                  }




makeCssDeclarationBorderXStyle :: (CssValueBorderStyle -> CssDeclaration) -> (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclaration)
makeCssDeclarationBorderXStyle declCtor (parser, token) = ((parser', token'), fmap declCtor value)
  where
    ((parser', token'), value) = parseTokensAsBorderStyleValue (parser, token)




makeCssDeclarationBorderTopStyle    = makeCssDeclarationBorderXStyle CssDeclarationBorderTopStyle
makeCssDeclarationBorderRightStyle  = makeCssDeclarationBorderXStyle CssDeclarationBorderRightStyle
makeCssDeclarationBorderBottomStyle = makeCssDeclarationBorderXStyle CssDeclarationBorderBottomStyle
makeCssDeclarationBorderLeftStyle   = makeCssDeclarationBorderXStyle CssDeclarationBorderLeftStyle




-- ----------------
-- Border Width
-- ----------------




-- https://www.w3.org/TR/CSS22/box.html#border-width-properties
data CssValueBorderWidth
  = CssValueBorderWidthThin
  | CssValueBorderWidthMedium
  | CssValueBorderWidthThick
  | CssValueBorderWidthInherit
  | CssValueBorderWidthDistance CssDistance
  deriving (Eq, Show, Data)




cssValueBorderWidthDict :: [(T.Text, CssValueBorderWidth)]
cssValueBorderWidthDict = [ ("thin",    CssValueBorderWidthThin)
                          , ("medium",  CssValueBorderWidthMedium)
                          , ("thick",   CssValueBorderWidthThick)
                          , ("inherit", CssValueBorderWidthInherit)
                          ]




parseTokensAsBorderWidthValue :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssValueBorderWidth)
parseTokensAsBorderWidthValue pat = (pat', declValue)
  where
    (vs', declValue) = tokensAsValueEnumString3 vs >>? declValueAsLength3
    pat' = pt3 vs'

    vs :: ValueState3 CssValueBorderWidth
    vs = (defaultValueState3 pat) { distanceValueCtor = Just CssValueBorderWidthDistance
                                  , enums3 = cssValueBorderWidthDict
                                  }




makeCssDeclarationBorderXWidth :: (CssValueBorderWidth -> CssDeclaration) -> (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclaration)
makeCssDeclarationBorderXWidth declCtor (parser, token) = ((parser', token'), fmap declCtor value)
  where
    ((parser', token'), value) = parseTokensAsBorderWidthValue (parser, token)




makeCssDeclarationBorderTopWidth    = makeCssDeclarationBorderXWidth CssDeclarationBorderTopWidth
makeCssDeclarationBorderRightWidth  = makeCssDeclarationBorderXWidth CssDeclarationBorderRightWidth
makeCssDeclarationBorderBottomWidth = makeCssDeclarationBorderXWidth CssDeclarationBorderBottomWidth
makeCssDeclarationBorderLeftWidth   = makeCssDeclarationBorderXWidth CssDeclarationBorderLeftWidth




-- ----------------
--
-- ----------------




makeCssDeclarationBottom v = CssDeclarationBottom v
makeCssDeclarationCaptionSide v = CssDeclarationCaptionSide v
makeCssDeclarationClear v = CssDeclarationClear v
makeCssDeclarationClip v = CssDeclarationClip v




-- --------------------------------
-- Color
-- --------------------------------




data CssValueColor
  = CssValueColorInherit
  | CssValueColor Int -- TODO: Int or Color?
  deriving (Eq, Show, Data)




cssValueColorDict :: [(T.Text, CssValueColor)]
cssValueColorDict = [ ("inherit", CssValueColorInherit)
                    ]




makeCssDeclarationColor :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclaration)
makeCssDeclarationColor pat = (pat', fmap CssDeclarationColor declValue)
  where
    (vs', declValue) = tokensAsValueEnumString3 vs >>? tokensAsValueColor3
    pat' = pt3 vs'

    vs :: ValueState3 CssValueColor
    vs = (defaultValueState3 pat) { colorValueCtor3 = Just CssValueColor
                                  , enums3          = cssValueColorDict
                                  }




-- --------------------------------
--
-- --------------------------------




makeCssDeclarationContent v = CssDeclarationContent v
makeCssDeclarationCounterIncrement v = CssDeclarationCounterIncrement v
makeCssDeclarationCounterReset v = CssDeclarationCounterReset v




-- --------------------------------
--
-- --------------------------------




data CssValueCursor
  = CssValueCursorCrosshair
  | CssValueCursorDefault
  | CssValueCursorPointer
  | CssValueCursorMove
  | CssValueCursorEResize
  | CssValueCursorNeResize
  | CssValueCursorNwResize
  | CssValueCursorNResize
  | CssValueCursorSeResize
  | CssValueCursorSwResize
  | CssValueCursorSResize
  | CssValueCursorWResize
  | CssValueCursorText
  | CssValueCursorWait
  | CssValueCursorHelp
  deriving (Eq, Show, Data, Enum)



cssValueCursorDict :: [(T.Text, CssValueCursor)]
cssValueCursorDict = [ ("crosshair", CssValueCursorCrosshair)
                     , ("default",   CssValueCursorDefault)
                     , ("pointer",   CssValueCursorPointer)
                     , ("move",      CssValueCursorMove)
                     , ("e-resize",  CssValueCursorEResize)
                     , ("ne-resize", CssValueCursorNeResize)
                     , ("nw-resize", CssValueCursorNwResize)
                     , ("n-resize",  CssValueCursorNResize)
                     , ("se-resize", CssValueCursorSeResize)
                     , ("sw-resize", CssValueCursorSwResize)
                     , ("s-resize",  CssValueCursorSResize)
                     , ("w-resize",  CssValueCursorWResize)
                     , ("text",      CssValueCursorText)
                     , ("wait",      CssValueCursorWait)
                     , ("help",      CssValueCursorHelp)
                     ]




makeCssDeclarationCursor :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclaration)
makeCssDeclarationCursor pat = (pat', fmap CssDeclarationCursor declValue)
  where
    (vs', declValue) = tokensAsValueEnumString3 vs
    pat'             = pt3 vs'

    vs :: ValueState3 CssValueCursor
    vs = (defaultValueState3 pat) { enums3 = cssValueCursorDict
                                  }




-- --------------------------------
--
-- --------------------------------




makeCssDeclarationDirection v = CssDeclarationDirection v




-- --------------------------------
-- Display
-- --------------------------------




data CssValueDisplay
 = CssValueDisplayBlock
 | CssValueDisplayInline
 | CssValueDisplayInlineBlock
 | CssValueDisplayListItem
 | CssValueDisplayNone
 | CssValueDisplayTable
 | CssValueDisplayTableRowGroup
 | CssValueDisplayTableHeaderGroup
 | CssValueDisplayTableFooterGroup
 | CssValueDisplayTableRow
 | CssValueDisplayTableCell
 deriving (Eq, Show, Data, Enum)




cssValueDisplayDict :: [(T.Text, CssValueDisplay)]
cssValueDisplayDict = [ ("block",              CssValueDisplayBlock)
                      , ("inline",             CssValueDisplayInline)
                      , ("inline-block",       CssValueDisplayInlineBlock)
                      , ("list-item",          CssValueDisplayListItem)
                      , ("none",               CssValueDisplayNone)
                      , ("table",              CssValueDisplayTable)
                      , ("table-row-group",    CssValueDisplayTableRowGroup)
                      , ("table-header-group", CssValueDisplayTableHeaderGroup)
                      , ("table-footer-group", CssValueDisplayTableFooterGroup)
                      , ("table-row",          CssValueDisplayTableRow)
                      , ("table-cell",         CssValueDisplayTableCell)
                      ]




makeCssDeclarationDisplay :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclaration)
makeCssDeclarationDisplay pat = (pat', fmap CssDeclarationDisplay declValue)
  where
    (vs', declValue) = tokensAsValueEnumString3 vs
    pat'             = pt3 vs'

    vs :: ValueState3 CssValueDisplay
    vs = (defaultValueState3 pat) { enums3 = cssValueDisplayDict
                                  }




-- --------------------------------
--
-- --------------------------------




makeCssDeclarationEmptyCells v = CssDeclarationEmptyCells v
makeCssDeclarationFloat v = CssDeclarationFloat v




-- --------------------------------
-- Font family (font-family)
-- --------------------------------




data CssValueFontFamily
  = CssValueFontFamilyList [T.Text]
  deriving (Eq, Show, Data)




makeCssDeclarationFontFamily :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclaration)
makeCssDeclarationFontFamily pat = (pat', fmap CssDeclarationFontFamily declValue)
  where
    (vs', declValue) = tokensAsValueStringList3 vs
    pat'             = pt3 vs'

    vs :: ValueState3 CssValueFontFamily
    vs = (defaultValueState3 pat) { stringListCtor = Just CssValueFontFamilyList
                                  }




-- --------------------------------
-- Font size (font-size)
-- https://www.w3.org/TR/CSS22/fonts.html#propdef-font-size
-- --------------------------------




data CssValueFontSize
     -- CSS2.2: <absolute-size>
  = CssValueFontSizeXXSmall
  | CssValueFontSizeXSmall
  | CssValueFontSizeSmall
  | CssValueFontSizeMedium
  | CssValueFontSizeLarge
  | CssValueFontSizeXLarge
  | CssValueFontSizeXXLarge

    -- CSS2.2: <relative-size>
  | CssValueFontSizeLarger
  | CssValueFontSizeSmaller

  | CssValueFontSizeDistance CssDistance
 deriving (Eq, Show, Data)




makeCssDeclarationFontSize :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclaration)
makeCssDeclarationFontSize pat = (pat', fmap CssDeclarationFontSize value)
  where
    (vs', value) = tokensAsValueEnumString3 vs >>? declValueAsLength3
    pat'         = pt3 vs'
    vs :: ValueState3 CssValueFontSize
    vs = ValueState3 { pt3               = pat
                  , colorValueCtor3   = Nothing
                  , distanceValueCtor = Just CssValueFontSizeDistance
                  , fontWeightValueCtor = Nothing
                  , bgPositionValueCtor = Nothing
                  , uriValueCtor        = Nothing
                  , stringListCtor      = Nothing
                  , enums3 = [ ("xx-small", CssValueFontSizeXXSmall)
                             , ("x-small",  CssValueFontSizeXSmall)
                             , ("small",    CssValueFontSizeSmall)
                             , ("medium",   CssValueFontSizeMedium)
                             , ("large",    CssValueFontSizeLarge)
                             , ("x-large",  CssValueFontSizeXLarge)
                             , ("xx-large", CssValueFontSizeXXLarge)
                             , ("larger",   CssValueFontSizeLarger)
                             , ("smaller",  CssValueFontSizeSmaller)
                             ]
                  , allowUnitlessDistance = False -- TODO: do we allow "1.0" (i.e. without unit) to be a valid value of font size?
                  }




-- --------------------------------
--
-- --------------------------------




makeCssDeclarationFontSizeAdjust v = CssDeclarationFontSizeAdjust v
makeCssDeclarationFontStretch v = CssDeclarationFontStretch v




-- --------------------------------
-- Font style (font-style)
-- --------------------------------




data CssValueFontStyle
  = CssValueFontStyleNormal
  | CssValueFontStyleItalic
  | CssValueFontStyleOblique
 deriving (Eq, Show, Data, Enum)




makeCssDeclarationFontStyle :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclaration)
makeCssDeclarationFontStyle pat = (pat', fmap CssDeclarationFontStyle declValue)
  where
    (vs', declValue) = tokensAsValueEnumString3 vs
    pat'             = pt3 vs'
    vs :: ValueState3 CssValueFontStyle =
      ValueState3 { pt3               = pat
                  , colorValueCtor3   = Nothing
                  , distanceValueCtor = Nothing
                  , fontWeightValueCtor = Nothing
                  , bgPositionValueCtor = Nothing
                  , uriValueCtor        = Nothing
                  , stringListCtor      = Nothing
                  , enums3 = [ ("normal",  CssValueFontStyleNormal)
                             , ("italic",  CssValueFontStyleItalic)
                             , ("oblique", CssValueFontStyleOblique)
                             ]
                  , allowUnitlessDistance = False
                  }




-- --------------------------------
-- Font variant (font-variant)
-- --------------------------------




data CssValueFontVariant
  = CssValueFontVariantNormal
  | CssValueFontVariantSmallCaps
 deriving (Eq, Show, Data, Enum)




makeCssDeclarationFontVariant :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclaration)
makeCssDeclarationFontVariant pat = (pat', fmap CssDeclarationFontVariant declValue)
  where
    (vs', declValue) = tokensAsValueEnumString3 vs
    pat'             = pt3 vs'
    vs :: ValueState3 CssValueFontVariant =
      ValueState3 { pt3               = pat
                  , colorValueCtor3   = Nothing
                  , distanceValueCtor = Nothing
                  , fontWeightValueCtor = Nothing
                  , bgPositionValueCtor = Nothing
                  , uriValueCtor        = Nothing
                  , stringListCtor      = Nothing
                  , enums3 = [ ("normal",  CssValueFontVariantNormal)
                             , ("small-caps",  CssValueFontVariantSmallCaps)
                             ]
                  , allowUnitlessDistance = False
                  }




-- ------------------------------------------------
-- Font weight (font-weight)
-- https://www.w3.org/TR/CSS22/fonts.html#font-boldness
-- ------------------------------------------------




data CssValueFontWeight
 = CssValueFontWeightNormal
 | CssValueFontWeightBold
 | CssValueFontWeightBolder
 | CssValueFontWeightLighter
 | CssValueFontWeightInt Int
 deriving (Eq, Show, Data)




makeCssDeclarationFontWeight :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclaration)
makeCssDeclarationFontWeight pat = (pat', fmap CssDeclarationFontWeight declValue)
  where
    (vs', declValue) = tokensAsValueEnumString3 vs >>? declValueAsFontWeightInteger3
    pat'             = pt3 vs'

    vs :: ValueState3 CssValueFontWeight
    vs = (defaultValueState3 pat) { fontWeightValueCtor = Just CssValueFontWeightInt
                                  , enums3 = [ ("normal",  CssValueFontWeightNormal)
                                             , ("bold",    CssValueFontWeightBold)
                                             , ("bolder",  CssValueFontWeightBolder)
                                             , ("lighter", CssValueFontWeightLighter)
                                             ] -- dillo also included "light" value in this list.
                                  }




-- ------------------------------------------------
--
-- ------------------------------------------------




makeCssDeclarationHeight v = CssDeclarationHeight v
makeCssDeclarationLeft v = CssDeclarationLeft v




-- ------------------------------------------------
-- Letter spacing (letter-spacing)
-- ------------------------------------------------




data CssValueLetterSpacing
  = CssValueLetterSpacingNormal
  | CssValueLetterSpacingDistance CssDistance
  deriving (Data, Eq, Show)




makeCssDeclarationLetterSpacing :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclaration)
makeCssDeclarationLetterSpacing pat = (pat', fmap CssDeclarationLetterSpacing value)
  where
    (vs', value) = tokensAsValueEnumString3 vs >>? declValueAsLength3
    pat'         = pt3 vs'

    vs :: ValueState3 CssValueLetterSpacing
    vs = (defaultValueState3 pat) { distanceValueCtor = Just CssValueLetterSpacingDistance
                                  , enums3 = [ ("normal",    CssValueLetterSpacingNormal)
                                             ]
                                  }




-- ------------------------------------------------
-- Line height (line-height)
-- ------------------------------------------------




data CssValueLineHeight
  = CssValueLineHeightNormal
  | CssValueLineHeightDistance CssDistance
  deriving (Data, Eq, Show)




makeCssDeclarationLineHeight :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclaration)
makeCssDeclarationLineHeight pat = (pat', fmap CssDeclarationLineHeight value)
  where
    (vs', value) = tokensAsValueEnumString3 vs >>? declValueAsLength3 -- declValueAsLengthPercentNumber
    pat'         = pt3 vs'

    vs :: ValueState3 CssValueLineHeight
    vs = (defaultValueState3 pat) { distanceValueCtor = Just CssValueLineHeightDistance
                                  , enums3 = [ ("normal",    CssValueLineHeightNormal)
                                             ]
                                  -- Original dillo code allowed unitless
                                  -- numeric values for zero and for values
                                  -- of type "length/percent/number". Line
                                  -- height was one of the properties that
                                  -- had this type.
                                  , allowUnitlessDistance = True
                                  }




-- --------------------------------
-- List Style Image
-- --------------------------------




-- This property is not supported.
makeCssDeclarationListStyleImage :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclaration)
makeCssDeclarationListStyleImage pat = (pat, Nothing) -- CssDeclarationListStyleImage




-- --------------------------------
-- List Style Position
-- --------------------------------




-- TODO: add support for "inherit"
-- TODO: add support for "initial"
data CssValueListStylePosition
 = CssValueListStylePositionInside
 | CssValueListStylePositionOutside
  deriving (Eq, Show, Data, Enum, Bounded)


cssValueListStylePositionDict :: [(T.Text, CssValueListStylePosition)]
cssValueListStylePositionDict = [ ("inside",               CssValueListStylePositionInside)
                                , ("outside",              CssValueListStylePositionOutside)
                                ]


makeCssDeclarationListStylePosition :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclaration)
makeCssDeclarationListStylePosition (parser, token) = ((parser', token'), fmap CssDeclarationListStylePosition value)
  where
    ((parser', token'), value) = parseTokensAsListStylePositionValue (parser, token)




parseTokensAsListStylePositionValue :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssValueListStylePosition)
parseTokensAsListStylePositionValue pat = (pat', declValue)
  where
    (vs', declValue) = tokensAsValueEnumString3 vs
    pat'             = pt3 vs'

    vs :: ValueState3 CssValueListStylePosition
    vs = (defaultValueState3 pat) { enums3 = cssValueListStylePositionDict
                                  }




-- --------------------------------
-- List Style Type
-- --------------------------------




-- TODO: add support for "inherit"
-- TODO: add support for "initial"
data CssValueListStyleType
  = CssValueListStyleTypeDisc
  | CssValueListStyleTypeCircle
  | CssValueListStyleTypeSquare
  | CssValueListStyleTypeDecimal
  | CssValueListStyleTypeDecimalLeadingZero
  | CssValueListStyleTypeLowerRoman
  | CssValueListStyleTypeUpperRoman
  | CssValueListStyleTypeLowerGreek
  | CssValueListStyleTypeLowerAlpha
  | CssValueListStyleTypeLowerLatin
  | CssValueListStyleTypeUpperAlpha
  | CssValueListStyleTypeUpperLatin
  | CssValueListStyleTypeHebrew
  | CssValueListStyleTypeArmenian
  | CssValueListStyleTypeGeorgian
  | CssValueListStyleTypeCjkIdeographic
  | CssValueListStyleTypeHiragana
  | CssValueListStyleTypeKatakana
  | CssValueListStyleTypeHiraganaIroha
  | CssValueListStyleTypeKatakanaIroha
  | CssValueListStyleTypeNone
  deriving (Eq, Show, Data, Enum, Bounded)




cssValueListStyleTypeDict :: [(T.Text, CssValueListStyleType)]
cssValueListStyleTypeDict = [ ("disc",                 CssValueListStyleTypeDisc)
                            , ("circle",               CssValueListStyleTypeCircle)
                            , ("square",               CssValueListStyleTypeSquare)
                            , ("decimal",              CssValueListStyleTypeDecimal)
                            , ("decimal-leading-zero", CssValueListStyleTypeDecimalLeadingZero)
                            , ("lower-roman",          CssValueListStyleTypeLowerRoman)
                            , ("upper-roman",          CssValueListStyleTypeUpperRoman)
                            , ("lower-greek",          CssValueListStyleTypeLowerGreek)
                            , ("lower-alpha",          CssValueListStyleTypeLowerAlpha)
                            , ("lower-latin",          CssValueListStyleTypeLowerLatin)
                            , ("upper-alpha",          CssValueListStyleTypeUpperAlpha)
                            , ("upper-latin",          CssValueListStyleTypeUpperLatin)
                            , ("hebrew",               CssValueListStyleTypeHebrew)
                            , ("armenian",             CssValueListStyleTypeArmenian)
                            , ("georgian",             CssValueListStyleTypeGeorgian)
                            , ("cjk-ideographic",      CssValueListStyleTypeCjkIdeographic)
                            , ("hiragana",             CssValueListStyleTypeHiragana)
                            , ("katakana",             CssValueListStyleTypeKatakana)
                            , ("hiragana-iroha",       CssValueListStyleTypeHiraganaIroha)
                            , ("katakana-iroha",       CssValueListStyleTypeKatakanaIroha)
                            , ("none",                 CssValueListStyleTypeNone)
                            ]




makeCssDeclarationListStyleType :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclaration)
makeCssDeclarationListStyleType (parser, token) = ((parser', token'), fmap CssDeclarationListStyleType value)
  where
    ((parser', token'), value) = parseTokensAsListStyleTypeValue (parser, token)




parseTokensAsListStyleTypeValue :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssValueListStyleType)
parseTokensAsListStyleTypeValue pat = (pat', declValue)
  where
    (vs', declValue) = tokensAsValueEnumString3 vs
    pat'             = pt3 vs'

    vs :: ValueState3 CssValueListStyleType
    vs = (defaultValueState3 pat) { enums3 = cssValueListStyleTypeDict
                                  }




-- --------------------------------
--
-- --------------------------------




makeCssDeclarationMarginBottom v = CssDeclarationMarginBottom v
makeCssDeclarationMarginLeft v = CssDeclarationMarginLeft v
makeCssDeclarationMarginRight v = CssDeclarationMarginRight v
makeCssDeclarationMarginTop v = CssDeclarationMarginTop v
makeCssDeclarationMarkerOffset v = CssDeclarationMarkerOffset v
makeCssDeclarationMarks v = CssDeclarationMarks v
makeCssDeclarationMaxHeight v = CssDeclarationMaxHeight v
makeCssDeclarationMaxWidth v = CssDeclarationMaxWidth v
makeCssDeclarationMinHeight v = CssDeclarationMinHeight v
makeCssDeclarationMinWidth v = CssDeclarationMinWidth v
makeCssDeclarationOutlineColor v = CssDeclarationOutlineColor v
makeCssDeclarationOutlineStyle v = CssDeclarationOutlineStyle v
makeCssDeclarationOutlineWidth v = CssDeclarationOutlineWidth v
makeCssDeclarationOverflow v = CssDeclarationOverflow v




-- ------------------------------------------------
-- Padding
-- ------------------------------------------------



-- Here is a tricky question: should I make separate types for padding of
-- Bottom/Top/Left/Right, or can I get away with common type for all four
-- properties?
data CssValuePadding
  = CssValuePadding CssDistance
  deriving (Eq, Show, Data)




makeCssDeclarationPaddingX :: (CssValuePadding -> CssDeclaration) -> (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclaration)
makeCssDeclarationPaddingX declCtor (parser, token) = ((parser', token'), fmap declCtor value)
  where
    ((parser', token'), value) = parseTokensAsPaddingValue (parser, token)




parseTokensAsPaddingValue :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssValuePadding)
parseTokensAsPaddingValue (parser, token) = ((parser', token'), value)
  where
    (vs', value)      = declValueAsLength3 vs
    (parser', token') = pt3 vs'
    vs :: ValueState3 CssValuePadding
      = ValueState3 { pt3                   = (parser, token)
                    , colorValueCtor3       = Nothing
                    , distanceValueCtor     = Just CssValuePadding
                    , fontWeightValueCtor   = Nothing
                    , bgPositionValueCtor = Nothing
                    , uriValueCtor        = Nothing
                    , stringListCtor      = Nothing
                    , enums3                = []
                    , allowUnitlessDistance = False -- TODO: do we allow "1.0" (i.e. without unit) to be a valid value of padding?
                    }




makeCssDeclarationPaddingTop    = makeCssDeclarationPaddingX CssDeclarationPaddingTop
makeCssDeclarationPaddingRight  = makeCssDeclarationPaddingX CssDeclarationPaddingRight
makeCssDeclarationPaddingBottom = makeCssDeclarationPaddingX CssDeclarationPaddingBottom
makeCssDeclarationPaddingLeft   = makeCssDeclarationPaddingX CssDeclarationPaddingLeft





-- ------------------------------------------------
--
-- ------------------------------------------------




makeCssDeclarationPosition v = CssDeclarationPosition v
makeCssDeclarationQuotes v = CssDeclarationQuotes v
makeCssDeclarationRight v = CssDeclarationRight v




-- ------------------------------------------------
-- Text align (text-align)
-- https://www.w3.org/TR/CSS22/text.html#propdef-text-align
-- ------------------------------------------------




-- dillo also specified a "string" value as one of accepted values of the
-- property, but CSS2.2 doesn't mention this value.
data CssValueTextAlign
 = CssValueTextAlignLeft
 | CssValueTextAlignRight
 | CssValueTextAlignCenter
 | CssValueTextAlignJustify
 deriving (Bounded, Data, Enum, Eq, Show)




makeCssDeclarationTextAlign :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclaration)
makeCssDeclarationTextAlign pat = (pat', fmap CssDeclarationTextAlign declValue)
  where
    pat'             = pt3 vs'
    (vs', declValue) = tokensAsValueEnumString3 vs
    vs = (defaultValueState3 pat) { enums3 = [ ("left",    CssValueTextAlignLeft)
                                             , ("right",   CssValueTextAlignRight)
                                             , ("center",  CssValueTextAlignCenter)
                                             , ("justify", CssValueTextAlignJustify)
                                             ]
                                  }




-- ------------------------------------------------
-- Text decoration (text-decoration)
-- https://www.w3.org/TR/CSS22/text.html#lining-striking-props
-- https://www.w3.org/TR/css-text-decor-3/
-- ------------------------------------------------




-- TODO: add support for "none" value
data CssValueTextDecoration
  = CssValueTextDecorationUnderline
  | CssValueTextDecorationOverline
  | CssValueTextDecorationLineThrough
  | CssValueTextDecorationBlink
 deriving (Data, Enum, Eq, Show)



makeCssDeclarationTextDecoration :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclaration)
makeCssDeclarationTextDecoration pat = (pat', fmap CssDeclarationTextDecoration declValue)
  where
    pat'             = pt3 vs'
    (vs', declValue) = tokensAsValueMultiEnum3 vs
    vs = (defaultValueState3 pat) { enums3 = [ ("underline",     CssValueTextDecorationUnderline)
                                             , ("overline",      CssValueTextDecorationOverline)
                                             , ("line-through",  CssValueTextDecorationLineThrough)
                                             , ("blink",         CssValueTextDecorationBlink)
                                             ]
                                  }



-- ------------------------------------------------
-- Text indent (text-indent)
-- ------------------------------------------------




data CssValueTextIndent
 = CssValueTextIndentDistance CssDistance
 deriving (Data, Eq, Show)




makeCssDeclarationTextIndent :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclaration)
makeCssDeclarationTextIndent pat =  (pat', fmap CssDeclarationTextIndent declValue)
  where
    (vs', declValue) = declValueAsLength3 vs
    pat'             = pt3 vs'

    vs :: ValueState3 CssValueTextIndent
    vs = (defaultValueState3 pat) { distanceValueCtor = Just CssValueTextIndentDistance
                                  }




-- ------------------------------------------------
-- Text shadow (text-shadow)
-- ------------------------------------------------




makeCssDeclarationTextShadow v = CssDeclarationTextShadow v




-- ------------------------------------------------
-- Text transform (text-transform)
-- ------------------------------------------------




data CssValueTextTransform
 = CssValueTextTransformNone
 | CssValueTextTransformCapitalize
 | CssValueTextTransformUppercase
 | CssValueTextTransformLowercase
 deriving (Bounded, Data, Enum, Eq, Show)




makeCssDeclarationTextTransform :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclaration)
makeCssDeclarationTextTransform pat = (pat', fmap CssDeclarationTextTransform declValue)
  where
    pat'             = pt3 vs'
    (vs', declValue) = tokensAsValueEnumString3 vs
    vs = (defaultValueState3 pat) { enums3 = [ ("none",       CssValueTextTransformNone)
                                             , ("capitalize", CssValueTextTransformCapitalize)
                                             , ("uppercase",  CssValueTextTransformUppercase)
                                             , ("lowercase",  CssValueTextTransformLowercase)
                                             ]
                                  }




-- ------------------------------------------------
--
-- ------------------------------------------------




makeCssDeclarationTop v = CssDeclarationTop v
makeCssDeclarationUnicodeBiDi v = CssDeclarationUnicodeBiDi v




-- ------------------------------------------------
-- Vertical align (vertical-align)
-- https://www.w3.org/TR/CSS22/visudet.html#propdef-vertical-align
-- ------------------------------------------------




data CssValueVerticalAlign
  = CssValueVerticalAlignTop
  | CssValueVerticalAlignBottom
  | CssValueVerticalAlignMiddle
  | CssValueVerticalAlignBaseline
  | CssValueVerticalAlignSub
  | CssValueVerticalAlignSuper
  | CssValueVerticalAlignTextTop
  | CssValueVerticalAlignTextBottom
  deriving (Bounded, Data, Enum, Eq, Show)




cssValueVerticalAlignDict :: [(T.Text, CssValueVerticalAlign)]
cssValueVerticalAlignDict = [ ("top",         CssValueVerticalAlignTop)
                            , ("bottom",      CssValueVerticalAlignBottom)
                            , ("middle",      CssValueVerticalAlignMiddle)
                            , ("baseline",    CssValueVerticalAlignBaseline)
                            , ("sub",         CssValueVerticalAlignSub)
                            , ("super",       CssValueVerticalAlignSuper)
                            , ("text-top",    CssValueVerticalAlignTextTop)
                            , ("text-bottom", CssValueVerticalAlignTextBottom)
                            ]




makeCssDeclarationVerticalAlign :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclaration)
makeCssDeclarationVerticalAlign pat = (pat', fmap CssDeclarationVerticalAlign declValue)
  where
    (vs', declValue) = tokensAsValueEnumString3 vs
    pat'             = pt3 vs'

    vs :: ValueState3 CssValueVerticalAlign
    vs = (defaultValueState3 pat) { enums3 = cssValueVerticalAlignDict
                                  }




-- ------------------------------------------------
-- Visibility (visibility)
-- ------------------------------------------------



makeCssDeclarationVisibility v = CssDeclarationVisibility v




-- ------------------------------------------------
-- White space (white-space)
-- ------------------------------------------------




data CssValueWhitespace
  = CssValueWhitespaceNormal
  | CssValueWhitespacePre
  | CssValueWhitespaceNoWrap
  | CssValueWhitespacePreWrap
  | CssValueWhitespacePreLine
  deriving (Bounded, Data, Enum, Eq, Show)




cssValueWhitespaceDict :: [(T.Text, CssValueWhitespace)]
cssValueWhitespaceDict = [ ("normal",   CssValueWhitespaceNormal)
                         , ("pre",      CssValueWhitespacePre)
                         , ("nowrap",   CssValueWhitespaceNoWrap)
                         , ("pre-wrap", CssValueWhitespacePreWrap)
                         , ("pre-line", CssValueWhitespacePreLine)
                         ]




makeCssDeclarationWhitespace :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclaration)
makeCssDeclarationWhitespace pat = (pat', fmap CssDeclarationWhitespace declValue)
  where
    (vs', declValue) = tokensAsValueEnumString3 vs
    pat'             = pt3 vs'

    vs :: ValueState3 CssValueWhitespace
    vs = (defaultValueState3 pat) { enums3 = cssValueWhitespaceDict
                                  }




-- ------------------------------------------------
-- Width (width)
-- ------------------------------------------------




makeCssDeclarationWidth v = CssDeclarationWidth v




-- ------------------------------------------------
-- Word spacing (word-spacing)
-- https://www.w3.org/TR/CSS22/text.html#propdef-word-spacing
-- ------------------------------------------------




data CssValueWordSpacing
  = CssValueWordSpacingNormal
  | CssValueWordSpacingDistance CssDistance
  deriving (Data, Eq, Show)




cssValueWordSpacingDict = [ ("normal",    CssValueWordSpacingNormal)
                          ]




makeCssDeclarationWordSpacing :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclaration)
makeCssDeclarationWordSpacing pat = (pat', fmap CssDeclarationWordSpacing declValue)
  where
    (vs', declValue) = tokensAsValueEnumString3 vs >>? declValueAsLength3
    pat'             = pt3 vs'

    vs :: ValueState3 CssValueWordSpacing
    vs = (defaultValueState3 pat) { distanceValueCtor = Just CssValueWordSpacingDistance
                                  , enums3 = cssValueWordSpacingDict
                                  }




-- ------------------------------------------------
--
-- ------------------------------------------------




makeCssDeclarationZIndex v = CssDeclarationZIndex v




-- ------------------------------------------------
-- x-link pseudoproperty
-- ------------------------------------------------




data CssValueXLink
  = CssValueXLink Int
  deriving (Data, Eq, Show)




-- There is no parser/constructor function because the value is set not
-- during parsing of CSS, but during parsing of HTML. The HTML parser has a
-- dedicated method of passsing the value of x-link pseudo-property that
-- doesn't require us to pretend that we parse x-link property.



-- ------------------------------------------------
--
-- ------------------------------------------------




makeCssDeclarationXColSpan v = CssDeclarationXColSpan v
makeCssDeclarationXRowSpan v = CssDeclarationXRowSpan v
makeCssDeclarationXLang v = CssDeclarationXLang v
makeCssDeclarationXImg v = CssDeclarationXImg v
makeCssDeclarationXTooltip v = CssDeclarationXTooltip v
makeCssDeclaration_LAST _ = CssDeclaration_LAST




-- Parse "{ border = X Y Z }" CSS declaration. Expand the single "border"
-- declaration into a series of "border-top-width", "border-left-color" etc.
-- properties with their values. Return the list of the expanded
-- declarations.
--
-- TODO: this implementation can correctly parse all value tokens only when
-- they appear in the same order as 'property' integers. The function should
-- be able to handle the tokens in any order.
makeCssDeclarationBorder :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssDeclaration])
makeCssDeclarationBorder pt0 = (pt3, declarations)
  where
    declarations = catMaybes [ fmap CssDeclarationBorderTopWidth    declValueWidth,
                               fmap CssDeclarationBorderRightWidth  declValueWidth,
                               fmap CssDeclarationBorderBottomWidth declValueWidth,
                               fmap CssDeclarationBorderLeftWidth   declValueWidth

                             , fmap CssDeclarationBorderTopStyle    declValueStyle,
                               fmap CssDeclarationBorderRightStyle  declValueStyle,
                               fmap CssDeclarationBorderBottomStyle declValueStyle,
                               fmap CssDeclarationBorderLeftStyle   declValueStyle

                             , fmap CssDeclarationBorderTopColor    declValueColor,
                               fmap CssDeclarationBorderRightColor  declValueColor,
                               fmap CssDeclarationBorderBottomColor declValueColor,
                               fmap CssDeclarationBorderLeftColor   declValueColor
                             ]

    -- TODO: this piece of code has zero error checking.
    (pt1, declValueWidth) :: ((CssParser, CssToken), Maybe CssValueBorderWidth) = parseTokensAsBorderWidthValue pt0
    (pt2, declValueStyle) :: ((CssParser, CssToken), Maybe CssValueBorderStyle) = parseTokensAsBorderStyleValue pt1
    (pt3, declValueColor) :: ((CssParser, CssToken), Maybe CssValueBorderColor) = parseTokensAsBorderColorValue pt2




