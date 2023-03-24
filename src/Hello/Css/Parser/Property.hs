{-
Copyright (C) 2021-2023 Kamil Ignacak acerion@wp.pl

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




module Hello.Css.Parser.Property
  (
    CssProperty (..)

  , CssValueBackground (..)
  , CssValueBackgroundAttachment (..)
  , CssValueBackgroundColor (..)
  , CssValueBackgroundImage (..)
  , CssValueBackgroundPosition (..)
  , CssValueBackgroundRepeat (..)

  -- , CssValueBorder (..)
  , CssValueBorderTRBL (..)
  , CssValueBorderCollapse (..)
  , CssValueBorderColor (..)
  , CssValueBorderColor' (..)
  , CssValueBorderSpacing (..)
  , CssValueBorderStyle (..)
  , CssValueBorderStyle' (..)
  , CssValueBorderWidth (..)
  , CssValueBorderWidth' (..)
  , CssValueColor (..)
  , CssValueContent (..)
  , CssValueDisplay (..)
  , CssValueCursor (..)

  , CssValueFont (..)
  , CssValueFontRecord (..)
  , CssValueFontFamily (..)
  , CssValueFontSize (..)
  , CssValueFontStyle (..)
  , CssValueFontVariant (..)
  , CssValueFontWeight (..)
  , CssValueHeight (..)
  , CssValueLetterSpacing (..)
  , CssValueLineHeight (..)
  , CssValueListStyle (..)
  , CssValueListStyleType (..)
  , CssValueListStylePosition (..)
  , CssValueListStyleImage (..)
  , CssValueMargin (..)
  , CssValueMarginX (..)
  , CssValuePadding (..)
  , CssValuePaddingX (..)
  , CssValueTextAlign (..)
  , CssValueTextDecoration (..)
  , CssValueTextIndent (..)
  , CssValueTextTransform (..)
  , CssValueVerticalAlign (..)
  , CssValueWhitespace (..)
  , CssValueWidth (..)
  , CssValueWordSpacing (..)
  , CssValueXImg (..)
  , CssValueXLang (..)
  , CssValueXLink (..)
  , CssValueXTooltip (..)

  , parserPropertyBackground
  , parserPropertyBackgroundAttachment
  , parserPropertyBackgroundColor
  , parserPropertyBackgroundImage
  , parserPropertyBackgroundPosition
  , parserPropertyBackgroundRepeat

  , parserPropertyBorder
  , parserPropertyBorderColor
  , parserPropertyBorderWidth
  , parserPropertyBorderStyle

  , parserPropertyBorderTop
  , parserPropertyBorderRight
  , parserPropertyBorderBottom
  , parserPropertyBorderLeft

  , parserPropertyBorderCollapse
  , parserPropertyBorderSpacing

  , parserPropertyBorderTopColor
  , parserPropertyBorderRightColor
  , parserPropertyBorderBottomColor
  , parserPropertyBorderLeftColor

  , parserPropertyBorderTopStyle
  , parserPropertyBorderRightStyle
  , parserPropertyBorderBottomStyle
  , parserPropertyBorderLeftStyle

  , parserPropertyBorderTopWidth
  , parserPropertyBorderRightWidth
  , parserPropertyBorderBottomWidth
  , parserPropertyBorderLeftWidth

  , parserPropertyBottom
  , parserPropertyCaptionSide
  , parserPropertyClear
  , parserPropertyClip
  , parserPropertyColor
  , parserPropertyContent
  , parserPropertyCounterIncrement
  , parserPropertyCounterReset
  , parserPropertyCursor
  , parserPropertyDirection
  , parserPropertyDisplay
  , parserPropertyEmptyCells
  , parserPropertyFloat

  , parserPropertyFont
  , parserPropertyFontFamily
  , parserPropertyFontSize
  , parserPropertyFontSizeAdjust
  , parserPropertyFontStretch
  , parserPropertyFontStyle
  , parserPropertyFontVariant
  , parserPropertyFontWeight

  , parserPropertyHeight
  , parserPropertyLeft
  , parserPropertyLetterSpacing
  , parserPropertyLineHeight
  , parserPropertyListStyle
  , parserPropertyListStyleImage
  , parserPropertyListStylePosition
  , parserPropertyListStyleType
  , parserPropertyMargin
  , parserPropertyMarginBottom
  , parserPropertyMarginLeft
  , parserPropertyMarginRight
  , parserPropertyMarginTop
  , parserPropertyMarkerOffset
  , parserPropertyMarks
  , parserPropertyMaxHeight
  , parserPropertyMaxWidth
  , parserPropertyMinHeight
  , parserPropertyMinWidth
  , parserPropertyOutlineColor
  , parserPropertyOutlineStyle
  , parserPropertyOutlineWidth
  , parserPropertyOverflow
  , parserPropertyPadding
  , parserPropertyPaddingBottom
  , parserPropertyPaddingLeft
  , parserPropertyPaddingRight
  , parserPropertyPaddingTop
  , parserPropertyPosition
  , parserPropertyQuotes
  , parserPropertyRight
  , parserPropertyTextAlign
  , parserPropertyTextDecoration
  , parserPropertyTextIndent
  , parserPropertyTextShadow
  , parserPropertyTextTransform
  , parserPropertyTop
  , parserPropertyUnicodeBiDi
  , parserPropertyVerticalAlign
  , parserPropertyVisibility
  , parserPropertyWhitespace
  , parserPropertyWidth
  , parserPropertyWordSpacing
  , parserPropertyZIndex
  , parserPropertyInvalid

  , defaultBorderTRBLWidth
  , defaultBorderTRBLStyle
  , defaultBorderTRBLColor
  , defaultCssValueFont
  , defaultCssValueFontRecord

  , initialValueBackground

  , initialValueListStyleType
  , initialValueListStylePosition
  , initialValueListStyleImage

  , ParserProperty

  , parserValueFontFamily
  )
where




--import Debug.Trace

import Control.Applicative (Alternative(..), many)

import Data.Data
import Data.Text as T

import Hello.Css.Distance
import Hello.Css.Parser.Combinators
import Hello.Css.ParserHelpers
import Hello.Css.Tokenizer
import Hello.Css.Value

import Hello.Utils.Parser




type ParserProperty = Parser (CssParser, CssToken) CssProperty




-- https://www.w3.org/TR/css-color-4/#valdef-color-currentcolor
-- TODO: implement proper support for current color
cssValueCurrentColor :: CssValueBorderColor
cssValueCurrentColor = CssValueBorderColor 0x000000




-- A property name + property value.
data CssProperty
  = CssPropertyBackground CssValueBackground
  | CssPropertyBackgroundAttachment CssValueBackgroundAttachment      -- 0    parsing is unit-tested
  | CssPropertyBackgroundColor CssValueBackgroundColor                -- 1    parsing is unit-tested
  | CssPropertyBackgroundImage CssValueBackgroundImage                -- 2    This property is barely unit-tested because some decisions need to be made first.
  | CssPropertyBackgroundPosition CssValueBackgroundPosition          -- 3    There are some unit tests, but they don't really test much.
  | CssPropertyBackgroundRepeat CssValueBackgroundRepeat              -- 4

  | CssPropertyBorder CssValueBorderTRBL

  | CssPropertyBorderTop CssValueBorderTRBL
  | CssPropertyBorderRight CssValueBorderTRBL
  | CssPropertyBorderBottom CssValueBorderTRBL
  | CssPropertyBorderLeft CssValueBorderTRBL

  | CssPropertyBorderColor CssValueBorderColor'
  | CssPropertyBorderStyle CssValueBorderStyle'
  | CssPropertyBorderWidth CssValueBorderWidth'

  | CssPropertyBorderBottomColor CssValueBorderColor -- 5                parsing is tested
  | CssPropertyBorderBottomStyle CssValueBorderStyle -- 6                parsing is tested
  | CssPropertyBorderBottomWidth CssValueBorderWidth -- 7                parsing is tested
  | CssPropertyBorderCollapse CssValueBorderCollapse -- 8                parsing is unit-tested
  | CssPropertyBorderLeftColor CssValueBorderColor   -- 9                parsing is tested
  | CssPropertyBorderLeftStyle CssValueBorderStyle   -- 10               parsing is tested
  | CssPropertyBorderLeftWidth CssValueBorderWidth   -- 11               parsing is tested
  | CssPropertyBorderRightColor CssValueBorderColor  -- 12               parsing is tested
  | CssPropertyBorderRightStyle CssValueBorderStyle  -- 13               parsing is tested
  | CssPropertyBorderRightWidth CssValueBorderWidth  -- 14               parsing is tested
  | CssPropertyBorderSpacing CssValueBorderSpacing   -- 15               parsing is unit-tested
  | CssPropertyBorderTopColor CssValueBorderColor    -- 16               parsing is tested
  | CssPropertyBorderTopStyle CssValueBorderStyle    -- 17               parsing is tested
  | CssPropertyBorderTopWidth CssValueBorderWidth    -- 18               parsing is tested
  | CssPropertyBottom CssValue                       -- 19
  | CssPropertyCaptionSide CssValue                  -- 20
  | CssPropertyClear CssValue                        -- 21
  | CssPropertyClip CssValue                         -- 22
  | CssPropertyColor CssValueColor                   -- 23               parsing is tested
  | CssPropertyContent CssValueContent               -- 24               parsing is unit-tested
  | CssPropertyCounterIncrement CssValue             -- 25
  | CssPropertyCounterReset CssValue                 -- 26
  | CssPropertyCursor CssValueCursor                 -- 27               parsing is unit-tested
  | CssPropertyDirection CssValue                    -- 28
  | CssPropertyDisplay CssValueDisplay               -- 29               parsing is unit-tested
  | CssPropertyEmptyCells CssValue                   -- 30
  | CssPropertyFloat CssValue                        -- 31

  | CssPropertyFont CssValueFont
  | CssPropertyFontFamily CssValueFontFamily         -- 32               parsing is unit-tested (poorly)
  | CssPropertyFontSize CssValueFontSize             -- 33               parsing is unit-tested
  | CssPropertyFontSizeAdjust CssValue               -- 34
  | CssPropertyFontStretch CssValue                  -- 35
  | CssPropertyFontStyle CssValueFontStyle           -- 36               parsing is unit-tested
  | CssPropertyFontVariant CssValueFontVariant       -- 37               parsing is unit-tested
  | CssPropertyFontWeight CssValueFontWeight         -- 38               parsing is unit-tested
  | CssPropertyHeight CssValueHeight                 -- 39               parsing is unit-tested
  | CssPropertyLeft CssValue                         -- 40
  | CssPropertyLetterSpacing CssValueLetterSpacing   -- 41               parsing is unit-tested
  | CssPropertyLineHeight CssValueLineHeight         -- 42               parsing is unit-tested

  | CssPropertyListStyle CssValueListStyle
  | CssPropertyListStyleImage CssValueListStyleImage -- 43               not supported by hello
  | CssPropertyListStylePosition CssValueListStylePosition  -- 44        parsing is unit-tested
  | CssPropertyListStyleType CssValueListStyleType   -- 45               parsing is unit-tested

  | CssPropertyMargin CssValueMargin
  | CssPropertyMarginTop CssValueMarginX             -- 49               parsing is unit-tested
  | CssPropertyMarginRight CssValueMarginX           -- 48               parsing is unit-tested
  | CssPropertyMarginBottom CssValueMarginX          -- 46               parsing is unit-tested
  | CssPropertyMarginLeft CssValueMarginX            -- 47               parsing is unit-tested

  | CssPropertyMarkerOffset CssValue                 -- 50
  | CssPropertyMarks CssValue                        -- 51
  | CssPropertyMaxHeight CssValue                    -- 52
  | CssPropertyMaxWidth CssValue                     -- 53
  | CssPropertyMinHeight CssValue                    -- 54
  | CssPropertyMinWidth CssValue                     -- 55
  | CssPropertyOutlineColor CssValue                 -- 56
  | CssPropertyOutlineStyle CssValue                 -- 57
  | CssPropertyOutlineWidth CssValue                 -- 58
  | CssPropertyOverflow CssValue                     -- 59
  | CssPropertyPadding CssValuePadding
  | CssPropertyPaddingTop CssValuePaddingX           -- 63               parsing is unit-tested
  | CssPropertyPaddingRight CssValuePaddingX         -- 62               parsing is unit-tested
  | CssPropertyPaddingBottom CssValuePaddingX        -- 60               parsing is unit-tested
  | CssPropertyPaddingLeft CssValuePaddingX          -- 61               parsing is unit-tested
  | CssPropertyPosition CssValue                     -- 64
  | CssPropertyQuotes CssValue                       -- 65
  | CssPropertyRight CssValue                        -- 66
  | CssPropertyTextAlign CssValueTextAlign           -- 67
  | CssPropertyTextDecoration [CssValueTextDecoration] -- 68             Parsing is unit-tested. Using a list type because a set of values is allowed for this property.
  | CssPropertyTextIndent CssValueTextIndent         -- 69               Parsing is unit-tested.
  | CssPropertyTextShadow CssValue                   -- 70
  | CssPropertyTextTransform CssValueTextTransform   -- 71               parsing is unit-tested
  | CssPropertyTop CssValue                          -- 72
  | CssPropertyUnicodeBiDi CssValue                  -- 73
  | CssPropertyVerticalAlign CssValueVerticalAlign   -- 74               parsing is unit-tested
  | CssPropertyVisibility CssValue                   -- 75
  | CssPropertyWhitespace CssValueWhitespace         -- 76               parsing is unit-tested
  | CssPropertyWidth CssValueWidth                   -- 77
  | CssPropertyWordSpacing CssValueWordSpacing       -- 78               parsing is unit-tested
  | CssPropertyZIndex CssValue                       -- 79

  | CssPropertyXLink CssValueXLink                   -- 80               parsing is NOT unit-tested because there is no CSS parsing of this property
  | CssPropertyXColSpan CssValue                     -- 81               parsing is NOT unit-tested because there is no CSS parsing of this property
  | CssPropertyXRowSpan CssValue                     -- 82               parsing is NOT unit-tested because there is no CSS parsing of this property
  | CssPropertyXLang CssValueXLang                   -- 83               parsing is NOT unit-tested because there is no CSS parsing of this property
  | CssPropertyXImg CssValueXImg                     -- 84               parsing is NOT unit-tested because there is no CSS parsing of this property
  | CssPropertyXTooltip CssValueXTooltip             -- 85               parsing is NOT unit-tested because there is no CSS parsing of this property

  | CssPropertyInvalid                               -- 86
  deriving (Eq, Show, Data)




-- Second arg is a parser that can parse a value of some property (e.g.
-- font-size).
--
-- First arg is a function that can update given accumulator (a record or a
-- list) with a value returned by a parser.
--
-- This entire function turns "state to value" parser into "state to
-- update-function" parser.
--
-- This is just a simple fmap, so it's not really necessary, but I wanted to
-- give the operation a name and to have a place for some comments.
insertClosure :: (value -> acc -> acc) -> (Parser (CssParser, CssToken) value) -> Parser (CssParser, CssToken) (acc -> acc)
insertClosure updateAccWithValue parser = updateAccWithValue <$> parser




-- ------------------------------------------------
-- Background ("background")
-- This is a shorthand property.
--
-- This implementation is closer to CSS 2 rather than CSS Backgrounds 3.
--
-- https://www.w3.org/TR/CSS22/colors.html#propdef-background
-- https://www.w3.org/TR/css-backgrounds-3/#background
-- ------------------------------------------------




data CssValueBackground = CssValueBackground
  { backgroundColor       :: CssValueBackgroundColor
  , backgroundImage       :: CssValueBackgroundImage
  , backgroundPosition    :: CssValueBackgroundPosition
  , backgroundRepeatStyle :: CssValueBackgroundRepeat
  , backgroundAttachment  :: CssValueBackgroundAttachment
{-
  , backgroundOrigin      :: CssValueBackgroundOrigin
  , backgroundClip        :: CssValueBackgroundClip
-}
  } deriving (Data, Eq, Show)


initialValueBackground :: CssValueBackground
initialValueBackground = CssValueBackground
                         initialValueBackgroundColor
                         initialValueBackgroundImage
                         initialValueBackgroundPosition
                         initialValueBackgroundRepeatStyle
                         initialValueBackgroundAttachment
{-
                         initialValueBackgroundOrigin
                         initialValueBackgroundClip
-}




parserPropertyBackground :: Parser (CssParser, CssToken) CssProperty
parserPropertyBackground = CssPropertyBackground <$> parserValue
  where
    -- Run the combined parsers, get composed closures ("fs") returned by the
    -- parsers, and run the composed closures on initial/default value of
    -- property.
    parserValue :: Parser (CssParser, CssToken) CssValueBackground
    parserValue = (\ fs -> fs initialValueBackground) <$> (combinatorOneOrMoreUnordered parsersC)

    parsersC :: [ Parser (CssParser, CssToken) (CssValueBackground -> CssValueBackground) ]
    parsersC = [ parserBgColor, parserBgImage, parserBgPosition, parserBgRepeatStyle, parserBgAttachment ]

    -- TODO: sooner or later we will need to add here "many parserTokenWhitespace *>" in front of "parserValueBackground*".
    parserBgColor       = insertClosure (\ value acc -> acc { backgroundColor = value }) parserValueBackgroundColor
    parserBgImage       = insertClosure (\ value acc -> acc { backgroundImage = value }) parserValueBackgroundImage
    parserBgPosition    = insertClosure (\ value acc -> acc { backgroundPosition = value }) parserValueBackgroundPosition
    parserBgRepeatStyle = insertClosure (\ value acc -> acc { backgroundRepeatStyle = value }) parserValueBackgroundStyle
    parserBgAttachment  = insertClosure (\ value acc -> acc { backgroundAttachment = value }) parserValueBackgroundAttachment
{-
    parserBgOrigin      = insertClosure (\ value acc -> acc { backgroundOrigin = value }) <$> parserValueBackgroundOrigin pat'
    parserBgClip        = insertClosure (\ value acc -> acc { backgroundClip = value }) <$> ctorValueBackgroundClip pat'
-}




-- ------------------------------------------------
-- Background attachment (background-attachment)
-- ------------------------------------------------




data CssValueBackgroundAttachment
  = CssValueBackgroundAttachmentScroll
  | CssValueBackgroundAttachmentFixed
  deriving (Enum, Eq, Show, Data)


-- https://www.w3.org/TR/css-backgrounds-3/#background-attachment
initialValueBackgroundAttachment :: CssValueBackgroundAttachment
initialValueBackgroundAttachment = CssValueBackgroundAttachmentScroll




cssValueBackgroundAttachmentDict :: [(Text, CssValueBackgroundAttachment)]
cssValueBackgroundAttachmentDict = [ ("scroll",  CssValueBackgroundAttachmentScroll)
                                   , ("fixed",   CssValueBackgroundAttachmentFixed)
                                   ]



parserPropertyBackgroundAttachment :: Parser (CssParser, CssToken) CssProperty
parserPropertyBackgroundAttachment = CssPropertyBackgroundAttachment <$> parserValueBackgroundAttachment




parserValueBackgroundAttachment :: Parser (CssParser, CssToken) CssValueBackgroundAttachment
parserValueBackgroundAttachment = mkParserEnum cssValueBackgroundAttachmentDict




-- ------------------------------------------------
-- Background clip ("background-clip")
--
-- https://www.w3.org/TR/css-backgrounds-3/#background-clip
-- ------------------------------------------------




data CssValueBackgroundClip
  = CssValueBackgroundClipBorderBox
  | CssValueBackgroundClipPaddingBox
  | CssValueBackgroundClipContentBox
  deriving (Eq, Show, Data)

_initialValueBackgroundClip :: CssValueBackgroundClip
_initialValueBackgroundClip = CssValueBackgroundClipBorderBox




-- TODO: implement
_parserValueBackgroundClip :: Parser (CssParser, CssToken) CssValueBackgroundClip
_parserValueBackgroundClip = Parser $ \ _ -> Nothing




-- ------------------------------------------------
-- Background color (background-color)
-- ------------------------------------------------




data CssValueBackgroundColor
  = CssValueBackgroundColorInherit
  | CssValueBackgroundColorColor Int -- TODO: Int or Color?
  deriving (Eq, Show, Data)


-- TODO: this should be "transparent" according to
-- https://www.w3.org/TR/css-backgrounds-3/#propdef-background-color
initialValueBackgroundColor :: CssValueBackgroundColor
initialValueBackgroundColor = CssValueBackgroundColorColor 0xffffff




cssValueBackgroundColorDict :: [(Text, CssValueBackgroundColor)]
cssValueBackgroundColorDict = [ ("inherit",    CssValueBackgroundColorInherit)
                              ]




parserPropertyBackgroundColor :: Parser (CssParser, CssToken) CssProperty
parserPropertyBackgroundColor = CssPropertyBackgroundColor <$> parserValueBackgroundColor




parserValueBackgroundColor :: Parser (CssParser, CssToken) CssValueBackgroundColor
parserValueBackgroundColor = mkParserEnum cssValueBackgroundColorDict
                             <|> fmap CssValueBackgroundColorColor parserColor




-- ------------------------------------------------
-- Background image (background-image)
-- ------------------------------------------------




data CssValueBackgroundImage
 = CssValueBackgroundImageUri ParsedUri -- TODO: change from T.Text to URI abstract type
 deriving (Data, Eq, Show)


-- TODO: according to https://www.w3.org/TR/css-backgrounds-3/#background-image we need here a "none" value.
initialValueBackgroundImage :: CssValueBackgroundImage
initialValueBackgroundImage = CssValueBackgroundImageUri $ ParsedUri "" ""




parserPropertyBackgroundImage :: Parser (CssParser, CssToken) CssProperty
parserPropertyBackgroundImage = CssPropertyBackgroundImage <$> parserValueBackgroundImage




parserValueBackgroundImage :: Parser (CssParser, CssToken) CssValueBackgroundImage
parserValueBackgroundImage = Parser $ interpretTokensAsURI CssValueBackgroundImageUri




-- ------------------------------------------------
-- Background origin ("background-origin")
--
-- https://www.w3.org/TR/css-backgrounds-3/#background-origin
-- ------------------------------------------------




data CssValueBackgroundOrigin
 = CssValueBackgroundOriginPaddingBox
 | CssValueBackgroundOriginBorderBox
 | CssValueBackgroundOriginContentBox
 deriving (Data, Enum, Eq, Show)


_initialValueBackgroundOrigin :: CssValueBackgroundOrigin
_initialValueBackgroundOrigin = CssValueBackgroundOriginPaddingBox




-- TODO: implement
_parserValueBackgroundOrigin :: Parser (CssParser, CssToken) CssValueBackgroundOrigin
_parserValueBackgroundOrigin = Parser $ \ _ -> Nothing



-- ------------------------------------------------
-- Background position (background-position)
-- ------------------------------------------------




data CssValueBackgroundPosition
 = CssValueBackgroundPositionXY Int Int
 deriving (Data, Eq, Show)


-- According to https://www.w3.org/TR/css-backgrounds-3/#background-position,
-- these values should be percentages.
initialValueBackgroundPosition :: CssValueBackgroundPosition
initialValueBackgroundPosition = CssValueBackgroundPositionXY 0 0




parserPropertyBackgroundPosition :: Parser (CssParser, CssToken) CssProperty
parserPropertyBackgroundPosition = CssPropertyBackgroundPosition <$> parserValueBackgroundPosition




parserValueBackgroundPosition :: Parser (CssParser, CssToken) CssValueBackgroundPosition
parserValueBackgroundPosition = Parser $ interpretTokensAsBgPosition CssValueBackgroundPositionXY




-- ------------------------------------------------
-- Background repeat (background-repeat)
-- ------------------------------------------------




data CssValueBackgroundRepeat
  = CssValueBackgroundRepeatRepeat
  | CssValueBackgroundRepeatRepeatX
  | CssValueBackgroundRepeatRepeatY
  | CssValueBackgroundRepeatNoRepeat
  deriving (Data, Enum, Eq, Show)


-- https://www.w3.org/TR/css-backgrounds-3/#background-repeat
initialValueBackgroundRepeatStyle :: CssValueBackgroundRepeat
initialValueBackgroundRepeatStyle = CssValueBackgroundRepeatRepeat




cssValueBackgroundRepeatDict :: [(Text, CssValueBackgroundRepeat)]
cssValueBackgroundRepeatDict = [ ("repeat",     CssValueBackgroundRepeatRepeat)
                               , ("repeat-x",   CssValueBackgroundRepeatRepeatX)
                               , ("repeat-y",   CssValueBackgroundRepeatRepeatY)
                               , ("no-repeat",  CssValueBackgroundRepeatNoRepeat)
                               ]




parserPropertyBackgroundRepeat :: Parser (CssParser, CssToken) CssProperty
parserPropertyBackgroundRepeat = CssPropertyBackgroundRepeat <$> parserValueBackgroundStyle




parserValueBackgroundStyle :: Parser (CssParser, CssToken) CssValueBackgroundRepeat
parserValueBackgroundStyle = mkParserEnum cssValueBackgroundRepeatDict




-- ------------------------------------------------
-- Border ("border")
-- This is a shorthand property.
-- Unit-tested: yes.
--
-- https://drafts.csswg.org/css-backgrounds-3/#propdef-border
-- ------------------------------------------------




data CssValueBorder = CssValueBorder
  { borderWidth :: CssValueBorderWidth'
  , borderStyle :: CssValueBorderStyle'
  , borderColor :: CssValueBorderColor'
  } deriving (Data, Eq, Show)




-- Parse "{ border = X Y Z }" CSS declaration. The simple trio of values
-- should be expanded to full list of non-shortcu properties by style engine.
parserPropertyBorder :: Parser (CssParser, CssToken) CssProperty
parserPropertyBorder = CssPropertyBorder <$> parserValue
  where
    -- Run the combined parsers, get composed closures ("fs") returned by the
    -- parsers, and run the composed closures on initial/default value of
    -- property.
    parserValue :: Parser (CssParser, CssToken) CssValueBorderTRBL
    parserValue = (\ fs -> fs defaultValueBorderTRBL) <$> (combinatorOneOrMoreUnordered parsersC)

    parsersC :: [ Parser (CssParser, CssToken) (CssValueBorderTRBL -> CssValueBorderTRBL)]
    parsersC = [ parserBorderWidthValue, parserBorderStyleValue, parserBorderColorValue ]




-- ------------------------------------------------
-- Border width ("border-width")
-- This is a shorthand property.
-- Unit-tested: yes.
--
-- https://drafts.csswg.org/css-backgrounds-3/#propdef-border-width
-- ------------------------------------------------




data CssValueBorderWidth' = CssValueBorderWidth'
  { borderWidthTop    :: CssValueBorderWidth
  , borderWidthRight  :: CssValueBorderWidth
  , borderWidthBottom :: CssValueBorderWidth
  , borderWidthLeft   :: CssValueBorderWidth
  } deriving (Data, Eq, Show)




parserPropertyBorderWidth :: Parser (CssParser, CssToken) CssProperty
parserPropertyBorderWidth = CssPropertyBorderWidth <$> (mkParser1234 parserValue CssValueBorderWidth')
  where
    -- TODO: check if we should use 'many' or 'some' for space parsers.
    parserValue :: Parser (CssParser, CssToken) [CssValueBorderWidth]
    parserValue = some (many parserTokenWhitespace *> parserValueBorderWidth <* many parserTokenWhitespace)




-- ------------------------------------------------
-- Border color ("border-color")
-- This is a shorthand property.
-- Unit-tested: yes.
--
-- https://drafts.csswg.org/css-backgrounds-3/#propdef-border-color
-- ------------------------------------------------




data CssValueBorderColor' = CssValueBorderColor'
  { borderColorTop    :: CssValueBorderColor
  , borderColorRight  :: CssValueBorderColor
  , borderColorBottom :: CssValueBorderColor
  , borderColorLeft   :: CssValueBorderColor
  } deriving (Data, Eq, Show)




parserPropertyBorderColor :: Parser (CssParser, CssToken) CssProperty
parserPropertyBorderColor = CssPropertyBorderColor <$> (mkParser1234 parserValue CssValueBorderColor')
  where
    -- TODO: check if we should use 'many' or 'some' for space parsers.
    parserValue :: Parser (CssParser, CssToken) [CssValueBorderColor]
    parserValue = some (many parserTokenWhitespace *> parserValueBorderColor <* many parserTokenWhitespace)




-- ------------------------------------------------
-- Border style ("border-style")
-- This is a shorthand property.
-- Unit-tested: yes.
--
-- https://drafts.csswg.org/css-backgrounds-3/#propdef-border-style
-- ------------------------------------------------




data CssValueBorderStyle' = CssValueBorderStyle'
  { borderStyleTop    :: CssValueBorderStyle
  , borderStyleRight  :: CssValueBorderStyle
  , borderStyleBottom :: CssValueBorderStyle
  , borderStyleLeft   :: CssValueBorderStyle
  } deriving (Data, Eq, Show)




parserPropertyBorderStyle :: Parser (CssParser, CssToken) CssProperty
parserPropertyBorderStyle = CssPropertyBorderStyle <$> (mkParser1234 parserValue CssValueBorderStyle')
  where
    -- TODO: check if we should use 'many' or 'some' for space parsers.
    parserValue :: Parser (CssParser, CssToken) [CssValueBorderStyle]
    parserValue = some (many parserTokenWhitespace *> parserValueBorderStyle <* many parserTokenWhitespace)




-- ------------------------------------------------
-- Border top ("border-top")
-- This is a shorthand property.
-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-top
-- Unit-tested: yes
-- ------------------------------------------------




-- Common type for border-{top|right|bottom|left}
data CssValueBorderTRBL = CssValueBorderTRBL
  { borderTRBLWidth :: CssValueBorderWidth
  , borderTRBLStyle :: CssValueBorderStyle
  , borderTRBLColor :: CssValueBorderColor
  } deriving (Data, Eq, Show)




-- Ctor of default value of CssValueBorderTRBL type.
defaultValueBorderTRBL :: CssValueBorderTRBL
defaultValueBorderTRBL = CssValueBorderTRBL defaultBorderTRBLWidth defaultBorderTRBLStyle defaultBorderTRBLColor




-- Default values taken from
-- https://www.w3.org/TR/css-backgrounds-3/#property-index
defaultBorderTRBLWidth :: CssValueBorderWidth
defaultBorderTRBLWidth = CssValueBorderWidthMedium




defaultBorderTRBLStyle :: CssValueBorderStyle
defaultBorderTRBLStyle = CssValueBorderStyleNone




defaultBorderTRBLColor :: CssValueBorderColor
defaultBorderTRBLColor = cssValueCurrentColor




-- Parser of "border-top" property.
parserPropertyBorderTop :: Parser (CssParser, CssToken) CssProperty
parserPropertyBorderTop = CssPropertyBorderTop <$> parserValue
  where
    -- Run the combined parsers, get composed closures ("fs") returned by the
    -- parsers, and run the composed closures on initial/default value of
    -- property.
    parserValue :: Parser (CssParser, CssToken) CssValueBorderTRBL
    parserValue = (\ fs -> fs defaultValueBorderTRBL) <$> (combinatorOneOrMoreUnordered parsersC)

    parsersC :: [ Parser (CssParser, CssToken) (CssValueBorderTRBL -> CssValueBorderTRBL)]
    parsersC = [ parserBorderWidthValue, parserBorderStyleValue, parserBorderColorValue ]




parserBorderWidthValue :: Parser (CssParser, CssToken) (CssValueBorderTRBL -> CssValueBorderTRBL)
parserBorderWidthValue = insertClosure (\ value acc -> acc { borderTRBLWidth = value }) parserValueBorderWidth




parserBorderStyleValue :: Parser (CssParser, CssToken) (CssValueBorderTRBL -> CssValueBorderTRBL)
parserBorderStyleValue = insertClosure (\ value acc -> acc { borderTRBLStyle = value }) parserValueBorderStyle




parserBorderColorValue :: Parser (CssParser, CssToken) (CssValueBorderTRBL -> CssValueBorderTRBL)
parserBorderColorValue = insertClosure (\ value acc -> acc { borderTRBLColor = value }) parserValueBorderColor




-- ------------------------------------------------
-- Border right (border-right)
-- This is a shorthand property.
-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-right
-- Unit-tested: yes
-- ------------------------------------------------




-- Parser of "border-right" property.
parserPropertyBorderRight :: Parser (CssParser, CssToken) CssProperty
parserPropertyBorderRight = CssPropertyBorderRight <$> parserValue
  where
    -- Run the combined parsers, get composed closures ("fs") returned by the
    -- parsers, and run the composed closures on initial/default value of
    -- property.
    parserValue :: Parser (CssParser, CssToken) CssValueBorderTRBL
    parserValue = (\ fs -> fs defaultValueBorderTRBL) <$> (combinatorOneOrMoreUnordered parsersC)

    parsersC :: [ Parser (CssParser, CssToken) (CssValueBorderTRBL -> CssValueBorderTRBL)]
    parsersC = [ parserBorderWidthValue, parserBorderStyleValue, parserBorderColorValue ]




-- ------------------------------------------------
-- Border bottom (border-bottom)
-- This is a shorthand property.
-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-bottom
-- Unit-tested: yes
-- ------------------------------------------------




-- Parser of "border-bottom" property.
parserPropertyBorderBottom :: Parser (CssParser, CssToken) CssProperty
parserPropertyBorderBottom = CssPropertyBorderBottom <$> parserValue
  where
    -- Run the combined parsers, get composed closures ("fs") returned by the
    -- parsers, and run the composed closures on initial/default value of
    -- property.
    parserValue :: Parser (CssParser, CssToken) CssValueBorderTRBL
    parserValue = (\ fs -> fs defaultValueBorderTRBL) <$> (combinatorOneOrMoreUnordered parsersC)

    parsersC :: [ Parser (CssParser, CssToken) (CssValueBorderTRBL -> CssValueBorderTRBL)]
    parsersC = [ parserBorderWidthValue, parserBorderStyleValue, parserBorderColorValue ]




-- ------------------------------------------------
-- Border left (border-left)
-- This is a shorthand property.
-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-left
-- Unit-tested: yes
-- ------------------------------------------------




-- Parser of "border-left" property.
parserPropertyBorderLeft :: Parser (CssParser, CssToken) CssProperty
parserPropertyBorderLeft = CssPropertyBorderLeft <$> parserValue
  where
    -- Run the combined parsers, get composed closures ("fs") returned by the
    -- parsers, and run the composed closures on initial/default value of
    -- property.
    parserValue :: Parser (CssParser, CssToken) CssValueBorderTRBL
    parserValue = (\ fs -> fs defaultValueBorderTRBL) <$> (combinatorOneOrMoreUnordered parsersC)

    parsersC :: [ Parser (CssParser, CssToken) (CssValueBorderTRBL -> CssValueBorderTRBL)]
    parsersC = [ parserBorderWidthValue, parserBorderStyleValue, parserBorderColorValue ]




-- ------------------------------------------------
-- Border collapse (border-collapse)
-- ------------------------------------------------




data CssValueBorderCollapse
  = CssValueBorderCollapseSeparate
  | CssValueBorderCollapseCollapse
  deriving (Bounded, Data, Enum, Eq, Show)




cssValueBorderCollapseDict :: [(Text, CssValueBorderCollapse)]
cssValueBorderCollapseDict = [ ("separate",   CssValueBorderCollapseSeparate)
                             , ("collapse",   CssValueBorderCollapseCollapse)
                             ]




parserPropertyBorderCollapse :: Parser (CssParser, CssToken) CssProperty
parserPropertyBorderCollapse = CssPropertyBorderCollapse <$> parserValue
  where
    parserValue = mkParserEnum cssValueBorderCollapseDict




-- ------------------------------------------------
--
-- ------------------------------------------------




data CssValueBorderSpacing
 = CssValueBorderSpacingDistance CssDistance
 deriving (Eq, Show, Data)




parserPropertyBorderSpacing :: Parser (CssParser, CssToken) CssProperty
parserPropertyBorderSpacing = (CssPropertyBorderSpacing . CssValueBorderSpacingDistance) <$> parserValue
  where
    parserValue = mkParserLength False




-- ------------------------------------------------
-- Border Color
-- ------------------------------------------------




-- TODO: Here is a tricky question: should I make separate types for colors
-- of Bottom/Top/Left/Right, or can I get away with common type for all four
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




parserValueBorderColor :: Parser (CssParser, CssToken) CssValueBorderColor
parserValueBorderColor = mkParserEnum cssValueBorderColorDict
                         <|> fmap CssValueBorderColor parserColor




parserPropertyBorderTopColor :: Parser (CssParser, CssToken) CssProperty
parserPropertyBorderTopColor = CssPropertyBorderTopColor <$> parserValueBorderColor




parserPropertyBorderRightColor :: Parser (CssParser, CssToken) CssProperty
parserPropertyBorderRightColor = CssPropertyBorderRightColor <$> parserValueBorderColor




parserPropertyBorderBottomColor :: Parser (CssParser, CssToken) CssProperty
parserPropertyBorderBottomColor = CssPropertyBorderBottomColor <$> parserValueBorderColor




parserPropertyBorderLeftColor :: Parser (CssParser, CssToken) CssProperty
parserPropertyBorderLeftColor = CssPropertyBorderLeftColor <$> parserValueBorderColor




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




parserValueBorderStyle :: Parser (CssParser, CssToken) CssValueBorderStyle
parserValueBorderStyle = mkParserEnum cssValueBorderStyleDict




parserPropertyBorderTopStyle :: Parser (CssParser, CssToken) CssProperty
parserPropertyBorderTopStyle = CssPropertyBorderTopStyle <$> parserValueBorderStyle

parserPropertyBorderRightStyle :: Parser (CssParser, CssToken) CssProperty
parserPropertyBorderRightStyle = CssPropertyBorderRightStyle <$> parserValueBorderStyle

parserPropertyBorderBottomStyle :: Parser (CssParser, CssToken) CssProperty
parserPropertyBorderBottomStyle = CssPropertyBorderBottomStyle <$> parserValueBorderStyle

parserPropertyBorderLeftStyle :: Parser (CssParser, CssToken) CssProperty
parserPropertyBorderLeftStyle = CssPropertyBorderLeftStyle <$> parserValueBorderStyle




-- ------------------------------------------------
-- Border Width
-- ------------------------------------------------




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




parserValueBorderWidth :: Parser (CssParser, CssToken) CssValueBorderWidth
parserValueBorderWidth = mkParserEnum cssValueBorderWidthDict
                         <|> fmap CssValueBorderWidthDistance (mkParserLength False)




parserPropertyBorderTopWidth :: Parser (CssParser, CssToken) CssProperty
parserPropertyBorderTopWidth = CssPropertyBorderTopWidth <$> parserValueBorderWidth

parserPropertyBorderRightWidth :: Parser (CssParser, CssToken) CssProperty
parserPropertyBorderRightWidth = CssPropertyBorderRightWidth <$> parserValueBorderWidth

parserPropertyBorderBottomWidth :: Parser (CssParser, CssToken) CssProperty
parserPropertyBorderBottomWidth = CssPropertyBorderBottomWidth <$> parserValueBorderWidth

parserPropertyBorderLeftWidth :: Parser (CssParser, CssToken) CssProperty
parserPropertyBorderLeftWidth = CssPropertyBorderLeftWidth <$> parserValueBorderWidth




-- ------------------------------------------------
--
-- ------------------------------------------------




parserPropertyBottom :: CssValue -> CssProperty
parserPropertyBottom v = CssPropertyBottom v

parserPropertyCaptionSide :: CssValue -> CssProperty
parserPropertyCaptionSide v = CssPropertyCaptionSide v

parserPropertyClear :: CssValue -> CssProperty
parserPropertyClear v = CssPropertyClear v

parserPropertyClip :: CssValue -> CssProperty
parserPropertyClip v = CssPropertyClip v




-- ------------------------------------------------
-- Color
-- ------------------------------------------------




data CssValueColor
  = CssValueColorInherit
  | CssValueColor Int -- TODO: Int or Color?
  deriving (Eq, Show, Data)




cssValueColorDict :: [(T.Text, CssValueColor)]
cssValueColorDict = [ ("inherit", CssValueColorInherit)
                    ]




parserPropertyColor :: Parser (CssParser, CssToken) CssProperty
parserPropertyColor = CssPropertyColor <$> parserValue
  where
    parserValue = mkParserEnum cssValueColorDict <|> fmap CssValueColor parserColor




-- ------------------------------------------------
-- Content (content)
--
-- Not really supported by the program, and not supported by this
-- implementation either (beyond simple creation of declaration).
-- ------------------------------------------------




data CssValueContent
  = CssValueContent T.Text
  deriving (Data, Eq, Show)




parserPropertyContent :: Parser (CssParser, CssToken) CssProperty
parserPropertyContent = (CssPropertyContent . CssValueContent) <$> parserValue
  where
    parserValue = parserTokenStringValue




-- ------------------------------------------------
--
-- ------------------------------------------------




parserPropertyCounterIncrement :: CssValue -> CssProperty
parserPropertyCounterIncrement v = CssPropertyCounterIncrement v

parserPropertyCounterReset :: CssValue -> CssProperty
parserPropertyCounterReset v = CssPropertyCounterReset v




-- ------------------------------------------------
--
-- ------------------------------------------------




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




parserPropertyCursor :: Parser (CssParser, CssToken) CssProperty
parserPropertyCursor = CssPropertyCursor <$> parserValue
  where
    parserValue = mkParserEnum cssValueCursorDict




-- ------------------------------------------------
--
-- ------------------------------------------------




parserPropertyDirection :: CssValue -> CssProperty
parserPropertyDirection v = CssPropertyDirection v




-- ------------------------------------------------
-- Display
-- ------------------------------------------------




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




parserPropertyDisplay :: Parser (CssParser, CssToken) CssProperty
parserPropertyDisplay = CssPropertyDisplay <$> parserValue
  where
    parserValue = mkParserEnum cssValueDisplayDict




-- ------------------------------------------------
--
-- ------------------------------------------------




parserPropertyEmptyCells :: CssValue -> CssProperty
parserPropertyEmptyCells v = CssPropertyEmptyCells v

parserPropertyFloat :: CssValue -> CssProperty
parserPropertyFloat v = CssPropertyFloat v




-- ------------------------------------------------
-- Font ("font")
-- This is a shorthand property.
-- https://www.w3.org/TR/CSS22/fonts.html#font-shorthand
-- https://www.w3.org/TR/css-fonts-3/#font-prop

-- CSS2.2: [ [ <'font-style'> || <'font-variant'> || <'font-weight'> ]? <'font-size'> [ / <'line-height'> ]? <'font-family'> ]
--         | caption | icon | menu | message-box | small-caption | status-bar | inherit
--
-- ------------------------------------------------




data CssValueFont
  = CssValueFontRecord' CssValueFontRecord
  | CssValueFontEnum' CssValueFontEnum
  deriving (Eq, Show, Data)

defaultCssValueFont :: CssValueFont
defaultCssValueFont = CssValueFontRecord' defaultCssValueFontRecord




-- TODO: we are probably missing line-spacing value here, check the CSS spec.
data CssValueFontRecord = CssValueFontRecord
  { fontValueStyle       :: CssValueFontStyle
  , fontValueVariant     :: CssValueFontVariant
  , fontValueWeight      :: CssValueFontWeight
  , fontValueSize        :: CssValueFontSize
  , fontValueFamily      :: CssValueFontFamily
  } deriving (Eq, Show, Data)


defaultCssValueFontRecord :: CssValueFontRecord
defaultCssValueFontRecord = CssValueFontRecord
  { fontValueStyle   = CssValueFontStyleNormal
  , fontValueVariant = CssValueFontVariantNormal
  , fontValueWeight  = CssValueFontWeightNormal
  , fontValueSize    = CssValueFontSizeMedium
  , fontValueFamily  = CssValueFontFamilyList ["serif"]
  }




data CssValueFontEnum
 = CssValueFontCaption
 | CssValueFontIcon
 | CssValueFontMenu
 | CssValueFontMessageBox
 | CssValueFontSmallCaption
 | CssValueFontStatusBar
 | CssValueFontInherit
 deriving (Eq, Show, Data, Enum)




cssValueFontDict :: [(T.Text, CssValueFontEnum)]
cssValueFontDict = [ ("caption",          CssValueFontCaption)
                   , ("icon",             CssValueFontIcon)
                   , ("menu",             CssValueFontMenu)
                   , ("message-box",      CssValueFontMessageBox)
                   , ("small-caption",    CssValueFontSmallCaption)
                   , ("status-bar",       CssValueFontStatusBar)
                   , ("inherit",          CssValueFontInherit)
                   ]




{-
:m +Hello.Css.Tokenizer
:m +Hello.Css.Parser.Property
:m +Hello.Utils.Parser
:m +Hello.Css.ParserHelpers
:set prompt >
parserPropertyFont ((startTokenizer . defaultParser $ "8px serif"))
-}
parserPropertyFont :: Parser (CssParser, CssToken) CssProperty
parserPropertyFont = CssPropertyFont <$> parserValue
  where
    -- Run the combined parsers, get composed closures ("fs") returned by the
    -- parsers, and run the composed closures on initial/default value of
    -- property.
    parserValue :: Parser (CssParser, CssToken) CssValueFont
    parserValue = (\ fs -> fs defaultCssValueFont) <$> parserC

    parserC :: Parser (CssParser, CssToken) (CssValueFont -> CssValueFont)
    parserC = combinatorExactlyOne [ multiplierOnce (combinatorAllInOrder [ multiplierZeroOrOnce (combinatorOneOrMoreUnordered [parserFontStyle, parserFontVariant, parserFontWeight])
                                                                          , multiplierOnce parserFontSize
                                                                            -- TODO: there should be a parser for "/" token here (a combination of "/" and height).
                                                                            -- , multiplierZeroOrOnce lineHeight2 TODO: re-enable
                                                                          , multiplierOnce parserFontFamily
                                                                          ])
                                   , multiplierOnce parserFontEnum
                                   ]



parserFontStyle :: Parser (CssParser, CssToken) (CssValueFont -> CssValueFont)
parserFontStyle = insertClosure (\ x (CssValueFontRecord' acc) -> CssValueFontRecord' acc { fontValueStyle = x }) (many parserTokenWhitespace *> parserValueFontStyle)

parserFontVariant :: Parser (CssParser, CssToken) (CssValueFont -> CssValueFont)
parserFontVariant = insertClosure (\ x (CssValueFontRecord' acc) -> CssValueFontRecord' acc { fontValueVariant = x }) (many parserTokenWhitespace *> parserValueFontVariant)

parserFontWeight :: Parser (CssParser, CssToken) (CssValueFont -> CssValueFont)
parserFontWeight = insertClosure (\ x (CssValueFontRecord' acc) -> CssValueFontRecord' acc { fontValueWeight = x }) (many parserTokenWhitespace *> parserValueFontWeight)

parserFontSize :: Parser (CssParser, CssToken) (CssValueFont -> CssValueFont)
parserFontSize = insertClosure (\ x (CssValueFontRecord' acc) -> CssValueFontRecord' acc { fontValueSize = x }) (many parserTokenWhitespace *> parserValueFontSize)

parserFontFamily :: Parser (CssParser, CssToken) (CssValueFont -> CssValueFont)
parserFontFamily = insertClosure (\ x (CssValueFontRecord' acc) -> CssValueFontRecord' acc { fontValueFamily = x }) (many parserTokenWhitespace *> parserValueFontFamily)

parserFontEnum :: Parser (CssParser, CssToken) (CssValueFont -> CssValueFont)
parserFontEnum = insertClosure enumFn (many parserTokenWhitespace *> parserValueFontEnum)

enumFn :: CssValueFontEnum -> CssValueFont -> CssValueFont
enumFn enum _font = (CssValueFontEnum' enum)

parserValueFontEnum :: Parser (CssParser, CssToken) CssValueFontEnum
parserValueFontEnum = mkParserEnum cssValueFontDict




-- ------------------------------------------------
-- Font family ("font-family")
--
-- https://drafts.csswg.org/css-fonts-3/#propdef-font-family
-- [ <family-name> | <generic-family> ] #
-- ------------------------------------------------




data CssValueFontFamily
  = CssValueFontFamilyList [T.Text]
  deriving (Eq, Show, Data)




{-
:m +Hello.Css.Tokenizer
:m +Hello.Css.Parser.Property
:m +Hello.Utils.Parser
:m +Hello.Css.ParserHelpers
:set prompt >
parserPropertyFontFamily ((startTokenizer . defaultParser $ "serif"))
parserPropertyFontFamily ((startTokenizer . defaultParser $ "\"serif\""))
parserPropertyFontFamily ((startTokenizer . defaultParser $ "serif, \"hello\""))
-}
parserPropertyFontFamily :: Parser (CssParser, CssToken) CssProperty
parserPropertyFontFamily = CssPropertyFontFamily <$> parserValueFontFamily




-- FIXME: parserStringList is tricky. It returns emtpy list for failure, but
-- the empty list may be treated by callers as a sign of success. Therefore
-- callers must look for empty list as a sign of failure. TODO: check other
-- places that use parserStringList to see if the other places make this mistake.
parserValueFontFamily :: Parser (CssParser, CssToken) CssValueFontFamily
parserValueFontFamily = Parser $ \ pat -> case runParser parserStringList pat of
                                            Just (_pat', []) -> Nothing
                                            Just (pat', xs)  -> Just (pat', CssValueFontFamilyList xs)
                                            Nothing          -> Nothing
  where
    parserStringList = parserSeparatedList value sep
    value = parserTokenIdentValue <|> parserTokenStringValue
    sep = many parserTokenWhitespace *> parserTokenComma <* many parserTokenWhitespace




-- ------------------------------------------------
-- Font size (font-size)
-- https://www.w3.org/TR/CSS22/fonts.html#propdef-font-size
-- ------------------------------------------------




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




cssValueFontSizeDict :: [(Text, CssValueFontSize)]
cssValueFontSizeDict = [ ("xx-small", CssValueFontSizeXXSmall)
                       , ("x-small",  CssValueFontSizeXSmall)
                       , ("small",    CssValueFontSizeSmall)
                       , ("medium",   CssValueFontSizeMedium)
                       , ("large",    CssValueFontSizeLarge)
                       , ("x-large",  CssValueFontSizeXLarge)
                       , ("xx-large", CssValueFontSizeXXLarge)
                       , ("larger",   CssValueFontSizeLarger)
                       , ("smaller",  CssValueFontSizeSmaller)
                       ]




parserPropertyFontSize :: Parser (CssParser, CssToken) CssProperty
parserPropertyFontSize = CssPropertyFontSize <$> parserValueFontSize




parserValueFontSize :: Parser (CssParser, CssToken) CssValueFontSize
parserValueFontSize = mkParserEnum cssValueFontSizeDict
                      -- TODO: do we allow "1.0" (i.e. without unit) to be a valid value of font size?
                      <|> fmap CssValueFontSizeDistance (mkParserLength False)




-- ------------------------------------------------
--
-- ------------------------------------------------




parserPropertyFontSizeAdjust :: CssValue -> CssProperty
parserPropertyFontSizeAdjust v = CssPropertyFontSizeAdjust v

parserPropertyFontStretch :: CssValue -> CssProperty
parserPropertyFontStretch v = CssPropertyFontStretch v




-- ------------------------------------------------
-- Font style (font-style)
-- ------------------------------------------------




data CssValueFontStyle
  = CssValueFontStyleNormal
  | CssValueFontStyleItalic
  | CssValueFontStyleOblique
 deriving (Eq, Show, Data, Enum)




cssValueFontStyleDict :: [(Text, CssValueFontStyle)]
cssValueFontStyleDict = [ ("normal",  CssValueFontStyleNormal)
                        , ("italic",  CssValueFontStyleItalic)
                        , ("oblique", CssValueFontStyleOblique)
                        ]




parserPropertyFontStyle :: Parser (CssParser, CssToken) CssProperty
parserPropertyFontStyle = CssPropertyFontStyle <$> parserValueFontStyle




parserValueFontStyle :: Parser (CssParser, CssToken) CssValueFontStyle
parserValueFontStyle = mkParserEnum cssValueFontStyleDict




-- ------------------------------------------------
-- Font variant (font-variant)
-- ------------------------------------------------




data CssValueFontVariant
  = CssValueFontVariantNormal
  | CssValueFontVariantSmallCaps
 deriving (Eq, Show, Data, Enum)




cssValueFontVariantDict :: [(Text, CssValueFontVariant)]
cssValueFontVariantDict = [ ("normal",  CssValueFontVariantNormal)
                          , ("small-caps",  CssValueFontVariantSmallCaps)
                          ]




parserPropertyFontVariant :: Parser (CssParser, CssToken) CssProperty
parserPropertyFontVariant = CssPropertyFontVariant <$> parserValueFontVariant




parserValueFontVariant :: Parser (CssParser, CssToken) CssValueFontVariant
parserValueFontVariant = mkParserEnum cssValueFontVariantDict




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




cssValueFontWeightDict :: [(Text, CssValueFontWeight)]
cssValueFontWeightDict = [ ("normal",  CssValueFontWeightNormal)
                         , ("bold",    CssValueFontWeightBold)
                         , ("bolder",  CssValueFontWeightBolder)
                         , ("lighter", CssValueFontWeightLighter)
                         ] -- dillo also included "light" value in this list.




parserPropertyFontWeight :: Parser (CssParser, CssToken) CssProperty
parserPropertyFontWeight = CssPropertyFontWeight <$> parserValueFontWeight




parserValueFontWeight :: Parser (CssParser, CssToken) CssValueFontWeight
parserValueFontWeight = mkParserEnum cssValueFontWeightDict
                        <|> fmap CssValueFontWeightInt (mkParserRangeInteger (100, 900))




-- ------------------------------------------------
-- Height (height)
-- https://www.w3.org/TR/CSS22/visudet.html#propdef-height
-- ------------------------------------------------




data CssValueHeight
  = CssValueHeightDistance CssDistance
  deriving (Data, Eq, Show)




-- TODO: CSS2.2 says: "Negative values for 'height' are illegal.". Implement this.
parserPropertyHeight :: Parser (CssParser, CssToken) CssProperty
parserPropertyHeight = (CssPropertyHeight . CssValueHeightDistance) <$> parserValue
  where
    parserValue = mkParserLength False <|> parserDistanceAuto




-- ------------------------------------------------
--
-- ------------------------------------------------




parserPropertyLeft :: CssValue -> CssProperty
parserPropertyLeft v = CssPropertyLeft v




-- ------------------------------------------------
-- Letter spacing (letter-spacing)
-- ------------------------------------------------




data CssValueLetterSpacing
  = CssValueLetterSpacingNormal
  | CssValueLetterSpacingDistance CssDistance
  deriving (Data, Eq, Show)




cssValueLetterSpacingDict :: [(Text, CssValueLetterSpacing)]
cssValueLetterSpacingDict = [ ("normal",    CssValueLetterSpacingNormal)
                            ]




parserPropertyLetterSpacing :: Parser (CssParser, CssToken) CssProperty
parserPropertyLetterSpacing = CssPropertyLetterSpacing <$> parserValue
  where
    parserValue = mkParserEnum cssValueLetterSpacingDict
                  <|> fmap CssValueLetterSpacingDistance (mkParserLength False)




-- ------------------------------------------------
-- Line height (line-height)
-- ------------------------------------------------




data CssValueLineHeight
  = CssValueLineHeightNormal
  | CssValueLineHeightDistance CssDistance
  deriving (Data, Eq, Show)




cssValueLineHeightDict :: [(Text, CssValueLineHeight)]
cssValueLineHeightDict = [ ("normal",    CssValueLineHeightNormal)
                         ]




parserPropertyLineHeight :: Parser (CssParser, CssToken) CssProperty
parserPropertyLineHeight = CssPropertyLineHeight <$> parserValue
  where
    parserValue = mkParserEnum cssValueLineHeightDict
                  -- True: Original dillo code allowed unitless numeric values for
                  -- zero and for values of type "length/percent/number". Line
                  -- height was one of the properties that had this type.
                  <|> fmap CssValueLineHeightDistance (mkParserLength True)




-- ------------------------------------------------
-- List Style ("list-style")
-- This is a shorthand property.
-- https://drafts.csswg.org/css2/#propdef-list-style
--
-- Unit-tested: yes (poorly)
-- ------------------------------------------------


-- TODO: this is very imperfect implementation. It doesn't support handling
-- of single "none" keyword: "A value of none within the list-style property
-- sets whichever of list-style-type and list-style-image are not otherwise
-- specified to none. "
--
-- In general this property is weird.




data CssValueListStyle = CssValueListStyle
  { listStyleType     :: CssValueListStyleType
  , listStylePosition :: CssValueListStylePosition
  , listStyleImage    :: CssValueListStyleImage
  } deriving (Data, Eq, Show)

initialValueListStyle :: CssValueListStyle
initialValueListStyle = CssValueListStyle initialValueListStyleType initialValueListStylePosition initialValueListStyleImage




parserPropertyListStyle :: Parser (CssParser, CssToken) CssProperty
parserPropertyListStyle = CssPropertyListStyle <$> parserValue
  where
    -- Run the combined parsers, get composed closures ("fs") returned by the
    -- parsers, and run the composed closures on initial/default value of
    -- property.
    parserValue :: Parser (CssParser, CssToken) CssValueListStyle
    parserValue = (\ fs -> fs initialValueListStyle) <$> (combinatorOneOrMoreUnordered parsersC)

    parsersC :: [ Parser (CssParser, CssToken) (CssValueListStyle -> CssValueListStyle) ]
    parsersC = [ parserType, parserPosition, parserImage ]

    parserType     = insertClosure (\ value acc -> acc { listStyleType = value } ) parserValueListStyleType
    parserPosition = insertClosure (\ value acc -> acc { listStylePosition = value } ) parserValueListStylePosition
    parserImage    = insertClosure (\ value acc -> acc { listStyleImage = value } ) parserValueListStyleImage




-- ------------------------------------------------
-- List Style Image
--
-- This property is not supported.
-- https://drafts.csswg.org/css2/#propdef-list-style-image
-- ------------------------------------------------




data CssValueListStyleImage
 = CssValueListStyleImageURL T.Text
 | CssValueListStyleImageNone
 | CssValueListStyleImageInherit
 deriving (Eq, Show, Data)

initialValueListStyleImage :: CssValueListStyleImage
initialValueListStyleImage = CssValueListStyleImageNone




parserPropertyListStyleImage :: Parser (CssParser, CssToken) CssProperty
parserPropertyListStyleImage = CssPropertyListStyleImage <$> parserValueListStyleImage




parserValueListStyleImage :: Parser (CssParser, CssToken) CssValueListStyleImage
parserValueListStyleImage = Parser (const Nothing) -- TODO: implement parsing of the value




-- ------------------------------------------------
-- List Style Position
-- https://drafts.csswg.org/css2/#propdef-list-style-position
-- ------------------------------------------------




-- TODO: add support for "inherit"
-- TODO: add support for "initial"
data CssValueListStylePosition
 = CssValueListStylePositionInside
 | CssValueListStylePositionOutside
  deriving (Eq, Show, Data, Enum, Bounded)

initialValueListStylePosition :: CssValueListStylePosition
initialValueListStylePosition = CssValueListStylePositionOutside




cssValueListStylePositionDict :: [(T.Text, CssValueListStylePosition)]
cssValueListStylePositionDict = [ ("inside",               CssValueListStylePositionInside)
                                , ("outside",              CssValueListStylePositionOutside)
                                ]




parserPropertyListStylePosition :: Parser (CssParser, CssToken) CssProperty
parserPropertyListStylePosition = CssPropertyListStylePosition <$> parserValueListStylePosition




parserValueListStylePosition :: Parser (CssParser, CssToken) CssValueListStylePosition
parserValueListStylePosition = mkParserEnum cssValueListStylePositionDict




-- ------------------------------------------------
-- List Style Type
-- ------------------------------------------------




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

initialValueListStyleType :: CssValueListStyleType
initialValueListStyleType = CssValueListStyleTypeDisc




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




parserPropertyListStyleType :: Parser (CssParser, CssToken) CssProperty
parserPropertyListStyleType = CssPropertyListStyleType <$> parserValueListStyleType




parserValueListStyleType :: Parser (CssParser, CssToken) CssValueListStyleType
parserValueListStyleType = mkParserEnum cssValueListStyleTypeDict




-- ------------------------------------------------
-- Margin ("margin")
-- This is a shorthand property.
-- Parsing is unit-tested.
-- https://www.w3.org/TR/css-box-3/#margin-shorthand
-- ------------------------------------------------




data CssValueMargin = CssValueMargin
  { marginTop    :: CssValueMarginX
  , marginRight  :: CssValueMarginX
  , marginBottom :: CssValueMarginX
  , marginLeft   :: CssValueMarginX
  } deriving (Data, Eq, Show)




parserPropertyMargin :: Parser (CssParser, CssToken) CssProperty
parserPropertyMargin = CssPropertyMargin <$> (mkParser1234 parserValue CssValueMargin)
  where
    -- TODO: check if we should use 'many' or 'some' for space parsers.
    parserValue :: Parser (CssParser, CssToken) [CssValueMarginX]
    parserValue = some (many parserTokenWhitespace *> parserValueMargin <* many parserTokenWhitespace)




-- ------------------------------------------------
-- Margin
-- margin-top, margin-right, margin-bottom, margin-left
-- ------------------------------------------------




-- TODO: Here is a tricky question: should I make separate types for margin
-- of Bottom/Top/Left/Right, or can I get away with common type for all four
-- properties?
--
-- 'X' stands for Top/Right/Bottom/Left.
data CssValueMarginX
  = CssValueMarginXDistance CssDistance
  deriving (Data, Eq, Show)




parserValueMargin :: Parser (CssParser, CssToken) CssValueMarginX
parserValueMargin = fmap CssValueMarginXDistance (mkParserLength False <|> parserDistanceAuto)




parserPropertyMarginTop :: Parser (CssParser, CssToken) CssProperty
parserPropertyMarginTop = CssPropertyMarginTop <$> parserValueMargin

parserPropertyMarginRight :: Parser (CssParser, CssToken) CssProperty
parserPropertyMarginRight = CssPropertyMarginRight <$> parserValueMargin

parserPropertyMarginBottom :: Parser (CssParser, CssToken) CssProperty
parserPropertyMarginBottom = CssPropertyMarginBottom <$> parserValueMargin

parserPropertyMarginLeft :: Parser (CssParser, CssToken) CssProperty
parserPropertyMarginLeft = CssPropertyMarginLeft <$> parserValueMargin




-- ------------------------------------------------
--
-- ------------------------------------------------




parserPropertyMarkerOffset :: CssValue -> CssProperty
parserPropertyMarkerOffset v = CssPropertyMarkerOffset v

parserPropertyMarks :: CssValue -> CssProperty
parserPropertyMarks v = CssPropertyMarks v

parserPropertyMaxHeight :: CssValue -> CssProperty
parserPropertyMaxHeight v = CssPropertyMaxHeight v

parserPropertyMaxWidth :: CssValue -> CssProperty
parserPropertyMaxWidth v = CssPropertyMaxWidth v

parserPropertyMinHeight :: CssValue -> CssProperty
parserPropertyMinHeight v = CssPropertyMinHeight v

parserPropertyMinWidth :: CssValue -> CssProperty
parserPropertyMinWidth v = CssPropertyMinWidth v

parserPropertyOutlineColor :: CssValue -> CssProperty
parserPropertyOutlineColor v = CssPropertyOutlineColor v

parserPropertyOutlineStyle :: CssValue -> CssProperty
parserPropertyOutlineStyle v = CssPropertyOutlineStyle v

parserPropertyOutlineWidth :: CssValue -> CssProperty
parserPropertyOutlineWidth v = CssPropertyOutlineWidth v

parserPropertyOverflow :: CssValue -> CssProperty
parserPropertyOverflow v = CssPropertyOverflow v




-- ------------------------------------------------
-- Padding ("padding")
-- This is a shorthand property.
-- https://drafts.csswg.org/css2/#propdef-padding
--
-- Unit tested: yes
-- ------------------------------------------------




data CssValuePadding = CssValuePadding
  { paddingTop    :: CssValuePaddingX
  , paddingRight  :: CssValuePaddingX
  , paddingBottom :: CssValuePaddingX
  , paddingLeft   :: CssValuePaddingX
  } deriving (Data, Eq, Show)




parserPropertyPadding :: Parser (CssParser, CssToken) CssProperty
parserPropertyPadding = CssPropertyPadding <$> (mkParser1234 parserValue CssValuePadding)
  where
    -- TODO: check if we should use 'many' or 'some' for space parsers.
    parserValue :: Parser (CssParser, CssToken) [CssValuePaddingX]
    parserValue = some (many parserTokenWhitespace *> parserValuePadding <* many parserTokenWhitespace)




-- ------------------------------------------------
-- Padding-{top|right|bottom|left} (padding-X)
-- https://drafts.csswg.org/css2/#propdef-padding-top
-- ------------------------------------------------




-- TODO: Here is a tricky question: should I make separate types for padding
-- of Bottom/Top/Left/Right, or can I get away with common type for all four
-- properties?
data CssValuePaddingX
  = CssValuePaddingX CssDistance
  deriving (Eq, Show, Data)




 -- TODO: do we allow "1.0" (i.e. without unit) to be a valid value of padding?
parserValuePadding :: Parser (CssParser, CssToken) CssValuePaddingX
parserValuePadding = fmap CssValuePaddingX (mkParserLength False)




parserPropertyPaddingTop :: Parser (CssParser, CssToken) CssProperty
parserPropertyPaddingTop = CssPropertyPaddingTop <$> parserValuePadding

parserPropertyPaddingRight :: Parser (CssParser, CssToken) CssProperty
parserPropertyPaddingRight = CssPropertyPaddingRight <$> parserValuePadding

parserPropertyPaddingBottom :: Parser (CssParser, CssToken) CssProperty
parserPropertyPaddingBottom = CssPropertyPaddingBottom <$> parserValuePadding

parserPropertyPaddingLeft :: Parser (CssParser, CssToken) CssProperty
parserPropertyPaddingLeft = CssPropertyPaddingLeft <$> parserValuePadding




-- ------------------------------------------------
--
-- ------------------------------------------------




parserPropertyPosition :: CssValue -> CssProperty
parserPropertyPosition v = CssPropertyPosition v




parserPropertyQuotes :: CssValue -> CssProperty
parserPropertyQuotes v = CssPropertyQuotes v




parserPropertyRight :: CssValue -> CssProperty
parserPropertyRight v = CssPropertyRight v




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




cssValueTextAlignDict :: [(Text, CssValueTextAlign)]
cssValueTextAlignDict = [ ("left",    CssValueTextAlignLeft)
                        , ("right",   CssValueTextAlignRight)
                        , ("center",  CssValueTextAlignCenter)
                        , ("justify", CssValueTextAlignJustify)
                        ]




parserPropertyTextAlign :: Parser (CssParser, CssToken) CssProperty
parserPropertyTextAlign = CssPropertyTextAlign <$> parserValue
  where
    parserValue = mkParserEnum cssValueTextAlignDict




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




cssValueTextDecorationDict :: [(Text, CssValueTextDecoration)]
cssValueTextDecorationDict = [ ("underline",     CssValueTextDecorationUnderline)
                             , ("overline",      CssValueTextDecorationOverline)
                             , ("line-through",  CssValueTextDecorationLineThrough)
                             , ("blink",         CssValueTextDecorationBlink)
                             ]




parserPropertyTextDecoration :: Parser (CssParser, CssToken) CssProperty
parserPropertyTextDecoration = CssPropertyTextDecoration <$> parserValue
  where
    parserValue = Parser $ \ pat -> interpretTokensAsMultiEnum cssValueTextDecorationDict pat




-- ------------------------------------------------
-- Text indent (text-indent)
-- ------------------------------------------------




data CssValueTextIndent
 = CssValueTextIndentDistance CssDistance
 deriving (Data, Eq, Show)




parserPropertyTextIndent :: Parser (CssParser, CssToken) CssProperty
parserPropertyTextIndent = (CssPropertyTextIndent . CssValueTextIndentDistance) <$> parserValue
  where
    parserValue = mkParserLength False




-- ------------------------------------------------
-- Text shadow (text-shadow)
-- ------------------------------------------------




parserPropertyTextShadow :: CssValue -> CssProperty
parserPropertyTextShadow v = CssPropertyTextShadow v




-- ------------------------------------------------
-- Text transform (text-transform)
-- ------------------------------------------------




data CssValueTextTransform
 = CssValueTextTransformNone
 | CssValueTextTransformCapitalize
 | CssValueTextTransformUppercase
 | CssValueTextTransformLowercase
 deriving (Bounded, Data, Enum, Eq, Show)




cssValueTextTransformDict :: [(Text, CssValueTextTransform)]
cssValueTextTransformDict = [ ("none",       CssValueTextTransformNone)
                            , ("capitalize", CssValueTextTransformCapitalize)
                            , ("uppercase",  CssValueTextTransformUppercase)
                            , ("lowercase",  CssValueTextTransformLowercase)
                            ]




parserPropertyTextTransform :: Parser (CssParser, CssToken) CssProperty
parserPropertyTextTransform = CssPropertyTextTransform <$> parserValue
  where
    parserValue = mkParserEnum cssValueTextTransformDict




-- ------------------------------------------------
--
-- ------------------------------------------------




parserPropertyTop :: CssValue -> CssProperty
parserPropertyTop v = CssPropertyTop v



parserPropertyUnicodeBiDi :: CssValue -> CssProperty
parserPropertyUnicodeBiDi v = CssPropertyUnicodeBiDi v




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




parserPropertyVerticalAlign :: Parser (CssParser, CssToken) CssProperty
parserPropertyVerticalAlign = CssPropertyVerticalAlign <$> parserValue
  where
    parserValue = mkParserEnum cssValueVerticalAlignDict




-- ------------------------------------------------
-- Visibility (visibility)
-- ------------------------------------------------




parserPropertyVisibility :: CssValue -> CssProperty
parserPropertyVisibility v = CssPropertyVisibility v




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




parserPropertyWhitespace :: Parser (CssParser, CssToken) CssProperty
parserPropertyWhitespace = CssPropertyWhitespace <$> parserValue
  where
    parserValue = mkParserEnum cssValueWhitespaceDict




-- ------------------------------------------------
-- Width (width)
-- https://www.w3.org/TR/CSS22/visudet.html#propdef-width
-- ------------------------------------------------




data CssValueWidth
  = CssValueWidthDistance CssDistance
  deriving (Data, Eq, Show)




-- TODO: CSS2.2 says: "Negative values for 'width' are illegal.". Implement this.
parserPropertyWidth :: Parser (CssParser, CssToken) CssProperty
parserPropertyWidth = (CssPropertyWidth . CssValueWidthDistance) <$> parserValue
  where
    parserValue = mkParserLength False <|> parserDistanceAuto




-- ------------------------------------------------
-- Word spacing (word-spacing)
-- https://www.w3.org/TR/CSS22/text.html#propdef-word-spacing
-- ------------------------------------------------




data CssValueWordSpacing
  = CssValueWordSpacingNormal
  | CssValueWordSpacingDistance CssDistance
  deriving (Data, Eq, Show)




cssValueWordSpacingDict :: [(Text, CssValueWordSpacing)]
cssValueWordSpacingDict = [ ("normal",    CssValueWordSpacingNormal)
                          ]




parserPropertyWordSpacing :: Parser (CssParser, CssToken) CssProperty
parserPropertyWordSpacing = CssPropertyWordSpacing <$> parserValue
  where
    parserValue = mkParserEnum cssValueWordSpacingDict
                  <|> fmap CssValueWordSpacingDistance (mkParserLength False)




-- ------------------------------------------------
--
-- ------------------------------------------------




parserPropertyZIndex :: CssValue -> CssProperty
parserPropertyZIndex v = CssPropertyZIndex v




-- ------------------------------------------------
-- x-link pseudo-property
--
-- Pseudo-property used internally by the browser. It is not parsed by CSS
-- parser, but set from HTML parser.

-- Without this property following a/href links won't work.
--
-- There is no parser/constructor function because the value is set not
-- during parsing of CSS, but during parsing of HTML. The HTML parser has a
-- dedicated method of passsing the value of x-link pseudo-property that
-- doesn't require us to pretend that we parse x-link property.
-- ------------------------------------------------




data CssValueXLink
  = CssValueXLink Int
  deriving (Data, Eq, Show)




-- ------------------------------------------------
-- x-colspan pseudo-property
-- Currently unused.
--
-- Pseudo-property used internally by the browser. It is not parsed by CSS
-- parser, but set from HTML parser.
--
-- There is no parser/constructor function because the value never appears in
-- CSS input.
-- TODO: check why/if we need this pseudo-property.
-- ------------------------------------------------




-- ------------------------------------------------
-- x-rowspan pseudo-property
-- Currently unused.
--
-- Pseudo-property used internally by the browser. It is not parsed by CSS
-- parser, but set from HTML parser.
--
-- There is no parser/constructor function because the value never appears in
-- CSS input.
-- TODO: check why/if we need this pseudo-property.
-- ------------------------------------------------




-- ------------------------------------------------
-- x-lang pseudo-property
--
-- Pseudo-property for "lang" or "xml:lang" attribute of html element.
--
-- Pseudo-property used internally by the browser. It is not parsed by CSS
-- parser, but set from HTML parser.
--
-- There is no parser/constructor function because the value never appears in
-- CSS input.
-- ------------------------------------------------




data CssValueXLang
  = CssValueXLang T.Text
  deriving (Data, Eq, Show)




-- ------------------------------------------------
-- x-img pseudo-property
--
-- Pseudo-property used (probably) to index images in a html document.
--
-- Pseudo-property used internally by the browser. It is not parsed by CSS
-- parser, but set from HTML parser.
--
-- There is no parser/constructor function because the value never appears in
-- CSS input.
-- ------------------------------------------------




data CssValueXImg
  = CssValueXImg Int
  deriving (Data, Eq, Show)




-- ------------------------------------------------
-- x-tooltip pseudo-property
--
-- Pseudo-property used internally by the browser. It is not parsed by CSS
-- parser, but set from HTML parser.
--
-- There is no parser/constructor function because the value never appears in
-- CSS input.
-- ------------------------------------------------




data CssValueXTooltip
  = CssValueXTooltip T.Text
  deriving (Data, Eq, Show)




-- ------------------------------------------------
--
-- ------------------------------------------------




parserPropertyInvalid :: CssValue -> CssProperty
parserPropertyInvalid _ = CssPropertyInvalid




-- ------------------------------------------------
-- End of properties
-- ------------------------------------------------



{-
-- TODO: this implementation can correctly parse all values only if they
-- appear in input CSS in the same order as they appear in a list of ctors.
-- The example in CSS2.2 for "background" property suggests that values in
-- input CSS string may appear in any order. This function should be able to
-- handle this situation.
parseDeclarationMultiple :: (CssParser, CssToken) -> [ParserProperty] -> ((CssParser, CssToken), [CssProperty])
parseDeclarationMultiple patArg propCtors = L.foldl f (patArg, []) propCtors
  where
    f (pat, acc) propCtor = case propCtor pat of
                              Just (pat', decl) -> (pat', acc ++ [decl])
                              Nothing           -> (pat, acc)
-}




-- Make a parser that parses 1, 2, 3 or 4 component values, where the component values are
-- specifying values for all sides of a box at once, or for (top-bottom)/(right-left) sides,
-- or for top/(right-left)/bottom sides, or for top/right/bottom/left sides.
--
-- See this part of CSS spec
-- (https://drafts.csswg.org/css-backgrounds-3/#propdef-border-style): "If
-- there is only one component value, it applies to all sides. If there are
-- two values, the top and bottom are set to the first value and the right
-- and left are set to the second [...]"
mkParser1234 :: Parser input [compo] -> (compo -> compo -> compo -> compo -> value) -> Parser input value
mkParser1234 parser componenetsToValue = Parser $ \ input ->
  case runParser parser input of
    Just (input', [top, right, bottom, left]) -> Just (input', componenetsToValue top right bottom left)
    Just (input', [top, rl, bottom])          -> Just (input', componenetsToValue top rl    bottom rl)
    Just (input', [tb, rl])                   -> Just (input', componenetsToValue tb  rl    tb     rl)
    Just (input', [v])                        -> Just (input', componenetsToValue v   v     v      v)
    _                                         -> Nothing



