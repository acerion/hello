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




module Hello.Css.Declaration
  (
    CssProperty (..)
  , CssDeclaration (..)

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

  , ctorCssPropertyBackground
  , makeCssPropertyBackgroundAttachment
  , makeCssPropertyBackgroundColor
  , makeCssPropertyBackgroundImage
  , makeCssPropertyBackgroundPosition
  , makeCssPropertyBackgroundRepeat

  , ctorCssPropertyBorder
  , ctorCssPropertyBorderColor
  , ctorCssPropertyBorderWidth
  , ctorCssPropertyBorderStyle

  , makeCssPropertyBorderTop
  , makeCssPropertyBorderRight
  , makeCssPropertyBorderBottom
  , makeCssPropertyBorderLeft

  , makeCssPropertyBorderCollapse
  , makeCssPropertyBorderSpacing

  , makeCssPropertyBorderTopColor
  , makeCssPropertyBorderRightColor
  , makeCssPropertyBorderBottomColor
  , makeCssPropertyBorderLeftColor

  , makeCssPropertyBorderTopStyle
  , makeCssPropertyBorderRightStyle
  , makeCssPropertyBorderBottomStyle
  , makeCssPropertyBorderLeftStyle

  , makeCssPropertyBorderTopWidth
  , makeCssPropertyBorderRightWidth
  , makeCssPropertyBorderBottomWidth
  , makeCssPropertyBorderLeftWidth

  , makeCssPropertyBottom
  , makeCssPropertyCaptionSide
  , makeCssPropertyClear
  , makeCssPropertyClip
  , makeCssPropertyColor
  , makeCssPropertyContent
  , makeCssPropertyCounterIncrement
  , makeCssPropertyCounterReset
  , makeCssPropertyCursor
  , makeCssPropertyDirection
  , makeCssPropertyDisplay
  , makeCssPropertyEmptyCells
  , makeCssPropertyFloat

  , ctorCssPropertyFont
  , makeCssPropertyFontFamily
  , makeCssPropertyFontSize
  , makeCssPropertyFontSizeAdjust
  , makeCssPropertyFontStretch
  , makeCssPropertyFontStyle
  , makeCssPropertyFontVariant
  , makeCssPropertyFontWeight

  , makeCssPropertyHeight
  , makeCssPropertyLeft
  , makeCssPropertyLetterSpacing
  , makeCssPropertyLineHeight
  , ctorCssPropertyListStyle
  , ctorCssPropertyListStyleImage
  , ctorCssPropertyListStylePosition
  , ctorCssPropertyListStyleType
  , makeCssPropertyMargin
  , makeCssPropertyMarginBottom
  , makeCssPropertyMarginLeft
  , makeCssPropertyMarginRight
  , makeCssPropertyMarginTop
  , makeCssPropertyMarkerOffset
  , makeCssPropertyMarks
  , makeCssPropertyMaxHeight
  , makeCssPropertyMaxWidth
  , makeCssPropertyMinHeight
  , makeCssPropertyMinWidth
  , makeCssPropertyOutlineColor
  , makeCssPropertyOutlineStyle
  , makeCssPropertyOutlineWidth
  , makeCssPropertyOverflow
  , makeCssPropertyPadding
  , makeCssPropertyPaddingBottom
  , makeCssPropertyPaddingLeft
  , makeCssPropertyPaddingRight
  , makeCssPropertyPaddingTop
  , makeCssPropertyPosition
  , makeCssPropertyQuotes
  , makeCssPropertyRight
  , makeCssPropertyTextAlign
  , makeCssPropertyTextDecoration
  , makeCssPropertyTextIndent
  , makeCssPropertyTextShadow
  , makeCssPropertyTextTransform
  , makeCssPropertyTop
  , makeCssPropertyUnicodeBiDi
  , makeCssPropertyVerticalAlign
  , makeCssPropertyVisibility
  , makeCssPropertyWhitespace
  , makeCssPropertyWidth
  , makeCssPropertyWordSpacing
  , makeCssPropertyZIndex
  , makeCssPropertyInvalid

  , defaultDeclaration
  , defaultBorderTRBLWidth
  , defaultBorderTRBLStyle
  , defaultBorderTRBLColor

  , initialValueBackground

  , initialValueListStyleType
  , initialValueListStylePosition
  , initialValueListStyleImage

  , PropertyCtor
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




type PropertyCtor = (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)


-- TODO: this is too similar to PropertyCtor
type CssPropertyParser = MyParser (CssParser, CssToken) CssProperty





-- https://www.w3.org/TR/css-syntax-3/#declaration: "Conceptually,
-- declarations are a particular instance of associating a property or
-- descriptor name with a value. Syntactically, a declaration has a name, a
-- value consisting of a list of component values, and an important flag
-- which is initially unset."
--
-- Also https://www.w3.org/TR/css-syntax-3/#syntax-description: "Declarations
-- are separated by semicolons." (this is useful to know when parsing a
-- declaration).
data CssDeclaration = CssDeclaration
  { property  :: CssProperty
  , important :: Bool
  } deriving (Show, Eq)




defaultDeclaration :: CssDeclaration
defaultDeclaration = CssDeclaration
  { property  = CssPropertyInvalid -- TODO: somewhere there is a code that does not set property2 field.
  , important = False
  }




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






ctorCssPropertyBackground :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
ctorCssPropertyBackground pat = (fmap . fmap) CssPropertyBackground (combinatorOneOrMoreUnordered2 initialValueBackground fs pat)
  where
    fs = [fnColor, fnImage, fnPosition, fnRepeatStyle, fnAttachment]

    fnColor :: (CssParser, CssToken) -> CssValueBackground -> Maybe ((CssParser, CssToken), CssValueBackground)
    fnColor pat' acc       = fmap (\ x -> acc { backgroundColor = x }) <$> ctorValueBackgroundColor pat'
    fnImage pat' acc       = fmap (\ x -> acc { backgroundImage = x }) <$> ctorValueBackgroundImage pat'
    fnPosition pat' acc    = fmap (\ x -> acc { backgroundPosition = x }) <$> ctorValueBackgroundPosition pat'
    fnRepeatStyle pat' acc = fmap (\ x -> acc { backgroundRepeatStyle = x }) <$> ctorValueBackgroundStyle pat'
    fnAttachment pat' acc  = fmap (\ x -> acc { backgroundAttachment = x }) <$> ctorValueBackgroundAttachment pat'
{-
    fnOrigin pat' acc      = fmap (\ x -> acc { backgroundOrigin = x }) <$> ctorValueBackgroundOrigin pat'
    fnClip pat' acc        = fmap (\ x -> acc { backgroundClip = x }) <$> ctorValueBackgroundClip pat'
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



makeCssPropertyBackgroundAttachment :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBackgroundAttachment pat = (fmap . fmap) CssPropertyBackgroundAttachment (ctorValueBackgroundAttachment pat)




parserValueBackgroundAttachment :: Parser (CssParser, CssToken) CssValueBackgroundAttachment
parserValueBackgroundAttachment = mkParserEnum cssValueBackgroundAttachmentDict




ctorValueBackgroundAttachment :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssValueBackgroundAttachment)
ctorValueBackgroundAttachment pat = runParser parserValueBackgroundAttachment pat




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
_ctorValueBackgroundClip :: (CssParser, CssToken) -> Maybe (CssParser, CssToken)
_ctorValueBackgroundClip _pat = Nothing




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




makeCssPropertyBackgroundColor :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBackgroundColor pat = (fmap . fmap) CssPropertyBackgroundColor (ctorValueBackgroundColor pat)




paserValueBackgroundColor :: Parser (CssParser, CssToken) CssValueBackgroundColor
paserValueBackgroundColor = mkParserEnum cssValueBackgroundColorDict
                            <|> Parser (interpretTokensAsColor CssValueBackgroundColorColor)




ctorValueBackgroundColor :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssValueBackgroundColor)
ctorValueBackgroundColor pat = runParser paserValueBackgroundColor pat




-- ------------------------------------------------
-- Background image (background-image)
-- ------------------------------------------------




data CssValueBackgroundImage
 = CssValueBackgroundImageUri T.Text -- TODO: change from T.Text to URI abstract type
 deriving (Data, Eq, Show)


-- TODO: according to https://www.w3.org/TR/css-backgrounds-3/#background-image we need here a "none" value.
initialValueBackgroundImage :: CssValueBackgroundImage
initialValueBackgroundImage = CssValueBackgroundImageUri ""




makeCssPropertyBackgroundImage :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBackgroundImage pat = (fmap . fmap) CssPropertyBackgroundImage (ctorValueBackgroundImage pat)




parserValueBackgroundImage :: Parser (CssParser, CssToken) CssValueBackgroundImage
parserValueBackgroundImage = Parser $ interpretTokensAsURI CssValueBackgroundImageUri




ctorValueBackgroundImage :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssValueBackgroundImage)
ctorValueBackgroundImage pat = runParser parserValueBackgroundImage pat




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
_ctorValueBackgroundOrigin ::  (CssParser, CssToken) -> Maybe (CssParser, CssToken)
_ctorValueBackgroundOrigin _pat = Nothing



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




makeCssPropertyBackgroundPosition :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBackgroundPosition pat = (fmap . fmap) CssPropertyBackgroundPosition (ctorValueBackgroundPosition pat)




parserValueBackgroundPosition :: Parser (CssParser, CssToken) CssValueBackgroundPosition
parserValueBackgroundPosition = Parser $ interpretTokensAsBgPosition CssValueBackgroundPositionXY




ctorValueBackgroundPosition :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssValueBackgroundPosition)
ctorValueBackgroundPosition pat = runParser parserValueBackgroundPosition pat




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




makeCssPropertyBackgroundRepeat :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBackgroundRepeat pat = (fmap . fmap) CssPropertyBackgroundRepeat (ctorValueBackgroundStyle pat)




parserValueBackgroundStyle :: Parser (CssParser, CssToken) CssValueBackgroundRepeat
parserValueBackgroundStyle = mkParserEnum cssValueBackgroundRepeatDict




ctorValueBackgroundStyle :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssValueBackgroundRepeat)
ctorValueBackgroundStyle pat = runParser parserValueBackgroundStyle pat




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
ctorCssPropertyBorder :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
ctorCssPropertyBorder pat = (fmap . fmap) CssPropertyBorder (combinatorOneOrMoreUnordered2 defaultValueBorderTRBL fs pat)
  where
    fs = [ parseBorderWidthValue
         , parseBorderStyleValue
         , parseBorderColorValue
         ]




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




ctorCssPropertyBorderWidth :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
ctorCssPropertyBorderWidth pat = parse1234 parser pat CssPropertyBorderWidth CssValueBorderWidth'
  where
    -- TODO: check if we should use 'many' or 'some' for space parsers.
    parser :: Parser (CssParser, CssToken) [CssValueBorderWidth]
    parser = some (many ignoreSpace *> parserValueBorderWidth <* many ignoreSpace)




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




ctorCssPropertyBorderColor :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
ctorCssPropertyBorderColor pat = parse1234 parser pat CssPropertyBorderColor CssValueBorderColor'
  where
    -- TODO: check if we should use 'many' or 'some' for space parsers.
    parser :: Parser (CssParser, CssToken) [CssValueBorderColor]
    parser = some (many ignoreSpace *> parserValueBorderColor <* many ignoreSpace)




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




ctorCssPropertyBorderStyle :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
ctorCssPropertyBorderStyle pat = parse1234 parser pat CssPropertyBorderStyle CssValueBorderStyle'
  where
    -- TODO: check if we should use 'many' or 'some' for space parsers.
    parser :: Parser (CssParser, CssToken) [CssValueBorderStyle]
    parser = some (many ignoreSpace *> parserValueBorderStyle <* many ignoreSpace)




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
makeCssPropertyBorderTop :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBorderTop pat = (fmap . fmap) CssPropertyBorderTop (combinatorOneOrMoreUnordered2 defaultValueBorderTRBL fs pat)
  where
    fs = [ parseBorderWidthValue
         , parseBorderStyleValue
         , parseBorderColorValue
         ]




parseBorderWidthValue :: (CssParser, CssToken) -> CssValueBorderTRBL -> Maybe ((CssParser, CssToken), CssValueBorderTRBL)
parseBorderWidthValue pat acc = fmap (\ x -> acc { borderTRBLWidth = x }) <$> parseTokensAsBorderWidthValue pat




parseBorderStyleValue :: (CssParser, CssToken) -> CssValueBorderTRBL -> Maybe ((CssParser, CssToken), CssValueBorderTRBL)
parseBorderStyleValue pat acc = fmap (\ x -> acc { borderTRBLStyle = x }) <$> parseTokensAsBorderStyleValue pat



parseBorderColorValue :: (CssParser, CssToken) -> CssValueBorderTRBL -> Maybe ((CssParser, CssToken), CssValueBorderTRBL)
parseBorderColorValue pat acc = fmap (\ x -> acc { borderTRBLColor = x }) <$> parseTokensAsBorderColorValue pat




-- ------------------------------------------------
-- Border right (border-right)
-- This is a shorthand property.
-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-right
-- Unit-tested: yes
-- ------------------------------------------------




-- Parser of "border-right" property.
makeCssPropertyBorderRight :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBorderRight pat = (fmap . fmap) CssPropertyBorderRight (combinatorOneOrMoreUnordered2 defaultValueBorderTRBL fs pat)
  where
    fs = [ parseBorderWidthValue
         , parseBorderStyleValue
         , parseBorderColorValue
         ]




-- ------------------------------------------------
-- Border bottom (border-bottom)
-- This is a shorthand property.
-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-bottom
-- Unit-tested: yes
-- ------------------------------------------------




-- Parser of "border-bottom" property.
makeCssPropertyBorderBottom :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBorderBottom pat = (fmap . fmap) CssPropertyBorderBottom (combinatorOneOrMoreUnordered2 defaultValueBorderTRBL fs pat)
  where
    fs = [ parseBorderWidthValue
         , parseBorderStyleValue
         , parseBorderColorValue
         ]




-- ------------------------------------------------
-- Border left (border-left)
-- This is a shorthand property.
-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-left
-- Unit-tested: yes
-- ------------------------------------------------




-- Parser of "border-left" property.
makeCssPropertyBorderLeft :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBorderLeft pat = (fmap . fmap) CssPropertyBorderLeft (combinatorOneOrMoreUnordered2 defaultValueBorderTRBL fs pat)
  where
    fs = [ parseBorderWidthValue
         , parseBorderStyleValue
         , parseBorderColorValue
         ]




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




makeCssPropertyBorderCollapse :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBorderCollapse pat = (fmap . fmap) CssPropertyBorderCollapse (runParser parser pat)
  where
    parser = mkParserEnum cssValueBorderCollapseDict




-- ------------------------------------------------
--
-- ------------------------------------------------




data CssValueBorderSpacing
 = CssValueBorderSpacingDistance CssDistance
 deriving (Eq, Show, Data)




makeCssPropertyBorderSpacing :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBorderSpacing pat = (fmap . fmap) CssPropertyBorderSpacing (parser pat)
  where
    parser = interpretTokensAsLength False CssValueBorderSpacingDistance




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




parseTokensAsBorderColorValue :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssValueBorderColor)
parseTokensAsBorderColorValue pat = runParser parserValueBorderColor pat




parserValueBorderColor :: Parser (CssParser, CssToken) CssValueBorderColor
parserValueBorderColor = mkParserEnum cssValueBorderColorDict
                         <|> Parser (interpretTokensAsColor CssValueBorderColor)




makeCssPropertyBorderXColor :: (CssValueBorderColor -> CssProperty) -> (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBorderXColor propCtor pat = (fmap . fmap) propCtor (parser pat)
  where
    parser = parseTokensAsBorderColorValue




makeCssPropertyBorderTopColor :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBorderTopColor = makeCssPropertyBorderXColor CssPropertyBorderTopColor




makeCssPropertyBorderRightColor :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBorderRightColor = makeCssPropertyBorderXColor CssPropertyBorderRightColor




makeCssPropertyBorderBottomColor :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBorderBottomColor = makeCssPropertyBorderXColor CssPropertyBorderBottomColor




makeCssPropertyBorderLeftColor :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBorderLeftColor = makeCssPropertyBorderXColor CssPropertyBorderLeftColor




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




parseTokensAsBorderStyleValue :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssValueBorderStyle)
parseTokensAsBorderStyleValue pat = runParser parserValueBorderStyle pat




parserValueBorderStyle :: Parser (CssParser, CssToken) CssValueBorderStyle
parserValueBorderStyle = mkParserEnum cssValueBorderStyleDict




makeCssPropertyBorderXStyle :: (CssValueBorderStyle -> CssProperty) -> (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBorderXStyle propCtor pat = (fmap . fmap) propCtor (parser pat)
  where
    parser = parseTokensAsBorderStyleValue




makeCssPropertyBorderTopStyle :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBorderTopStyle = makeCssPropertyBorderXStyle CssPropertyBorderTopStyle

makeCssPropertyBorderRightStyle :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBorderRightStyle = makeCssPropertyBorderXStyle CssPropertyBorderRightStyle

makeCssPropertyBorderBottomStyle :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBorderBottomStyle = makeCssPropertyBorderXStyle CssPropertyBorderBottomStyle

makeCssPropertyBorderLeftStyle :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBorderLeftStyle = makeCssPropertyBorderXStyle CssPropertyBorderLeftStyle




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




parseTokensAsBorderWidthValue :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssValueBorderWidth)
parseTokensAsBorderWidthValue pat = runParser parserValueBorderWidth pat




parserValueBorderWidth :: Parser (CssParser, CssToken) CssValueBorderWidth
parserValueBorderWidth = mkParserEnum cssValueBorderWidthDict
                         <|> Parser (interpretTokensAsLength False CssValueBorderWidthDistance)




makeCssPropertyBorderXWidth :: (CssValueBorderWidth -> CssProperty) -> (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBorderXWidth propCtor pat = (fmap . fmap) propCtor (parser pat)
  where
    parser = parseTokensAsBorderWidthValue




makeCssPropertyBorderTopWidth :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBorderTopWidth = makeCssPropertyBorderXWidth CssPropertyBorderTopWidth

makeCssPropertyBorderRightWidth :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBorderRightWidth = makeCssPropertyBorderXWidth CssPropertyBorderRightWidth

makeCssPropertyBorderBottomWidth :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBorderBottomWidth = makeCssPropertyBorderXWidth CssPropertyBorderBottomWidth

makeCssPropertyBorderLeftWidth :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBorderLeftWidth = makeCssPropertyBorderXWidth CssPropertyBorderLeftWidth




-- ------------------------------------------------
--
-- ------------------------------------------------




makeCssPropertyBottom :: CssValue -> CssProperty
makeCssPropertyBottom v = CssPropertyBottom v

makeCssPropertyCaptionSide :: CssValue -> CssProperty
makeCssPropertyCaptionSide v = CssPropertyCaptionSide v

makeCssPropertyClear :: CssValue -> CssProperty
makeCssPropertyClear v = CssPropertyClear v

makeCssPropertyClip :: CssValue -> CssProperty
makeCssPropertyClip v = CssPropertyClip v




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




makeCssPropertyColor :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyColor pat = (fmap . fmap) CssPropertyColor (runParser parser pat)
  where
    parser = mkParserEnum cssValueColorDict
             <|> Parser (interpretTokensAsColor CssValueColor)




-- ------------------------------------------------
-- Content (content)
--
-- Not really supported by the program, and not supported by this
-- implementation either (beyond simple creation of declaration).
-- ------------------------------------------------




data CssValueContent
  = CssValueContent T.Text
  deriving (Data, Eq, Show)




makeCssPropertyContent :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyContent pat = (fmap . fmap) CssPropertyContent (parser pat)
  where
    parser = interpretTokensAsString CssValueContent




-- ------------------------------------------------
--
-- ------------------------------------------------




makeCssPropertyCounterIncrement :: CssValue -> CssProperty
makeCssPropertyCounterIncrement v = CssPropertyCounterIncrement v

makeCssPropertyCounterReset :: CssValue -> CssProperty
makeCssPropertyCounterReset v = CssPropertyCounterReset v




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




makeCssPropertyCursor :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyCursor pat = (fmap . fmap) CssPropertyCursor (runParser parser pat)
  where
    parser = mkParserEnum cssValueCursorDict




-- ------------------------------------------------
--
-- ------------------------------------------------




makeCssPropertyDirection :: CssValue -> CssProperty
makeCssPropertyDirection v = CssPropertyDirection v




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




makeCssPropertyDisplay :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyDisplay pat = (fmap . fmap) CssPropertyDisplay (runParser parser pat)
  where
    parser = mkParserEnum cssValueDisplayDict




-- ------------------------------------------------
--
-- ------------------------------------------------




makeCssPropertyEmptyCells :: CssValue -> CssProperty
makeCssPropertyEmptyCells v = CssPropertyEmptyCells v

makeCssPropertyFloat :: CssValue -> CssProperty
makeCssPropertyFloat v = CssPropertyFloat v




-- ------------------------------------------------
-- Font (font)
-- This is a shorthand property.
-- https://www.w3.org/TR/CSS22/fonts.html#font-shorthand
-- https://www.w3.org/TR/css-fonts-3/#font-prop

-- CSS2.2: [ [ <'font-style'> || <'font-variant'> || <'font-weight'> ]? <'font-size'> [ / <'line-height'> ]? <'font-family'> ]
--         | caption | icon | menu | message-box | small-caption | status-bar | inherit
--
-- FIXME: this implementation doesn't follow a standard because it doesn't
-- first set all properties to their default values. The implementation
-- returns only those values that are explicitly set in CSS string.
-- ------------------------------------------------




-- Value of the "font" shortcut property contains non-shortcut font-*
-- properties.
--
-- TODO: use a proper record with font properties as type of CssValueFont.
-- Rewrite parsing of value tokens: parser should put the parsed tokens into
-- the record.
data CssValueFont = CssValueFont [CssProperty]
  deriving (Data, Eq, Show)




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




ctorCssPropertyFont :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
ctorCssPropertyFont pat = case makeCssPropertyFont pat of
                            (pat', list@(_:_)) -> Just (pat', CssPropertyFont $ CssValueFont list)
                            _                  -> Nothing





makeCssPropertyFont :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssProperty])
makeCssPropertyFont patArg = case runRecipe patArg of
                               (pat', Just acc) -> (pat', acc)
                               (_, Nothing)     -> (patArg, [])
  where
    -- This recipe is reflecting the grammar (?) from CSS2.2 spec.
    runRecipe pat = combinatorExactlyOne [ multiplierOnce (combinatorAllInOrder [ multiplierZeroOrOnce (combinatorOneOrMoreUnordered [fontStyle2, fontVariant2, fontWeight2])
                                                                                , multiplierOnce fontSize2
                                                                                -- TODO: there should be a parser for "/" token here (a combination of "/" and height).
                                                                                , multiplierZeroOrOnce lineHeight2
                                                                                , multiplierOnce fontFamily2
                                                                                ])
                                         , multiplierOnce fontEnum2
                                         ] pat




shortcutWrapper :: ((CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)) -> (CssParser, CssToken) -> ((CssParser, CssToken), Maybe [CssProperty])
shortcutWrapper ctor pat = case ctor pat of
                             Just (pat', prop) -> (pat', Just [prop])
                             Nothing           -> (pat, Nothing)




fontStyle2 :: CssPropertyParser
fontStyle2 pat = shortcutWrapper makeCssPropertyFontStyle pat

fontVariant2 :: CssPropertyParser
fontVariant2 pat = shortcutWrapper makeCssPropertyFontVariant pat

fontWeight2 :: CssPropertyParser
fontWeight2 pat = shortcutWrapper makeCssPropertyFontWeight pat

-- TODO: "line-height" is not processed here.
fontSize2 :: CssPropertyParser
fontSize2 pat = shortcutWrapper makeCssPropertyFontSize pat

fontFamily2 :: CssPropertyParser
fontFamily2 pat = shortcutWrapper makeCssPropertyFontFamily pat

lineHeight2 :: CssPropertyParser
lineHeight2 pat = shortcutWrapper makeCssPropertyHeight pat -- TODO: define correctly

fontEnum2 :: CssPropertyParser
fontEnum2 pat = case runParser (mkParserEnum cssValueFontDict) pat of
                  Just (pat', _) -> (pat', Just []) -- TODO correctly handle enum values
                  Nothing        -> (pat, Nothing)




-- ------------------------------------------------
-- Font family (font-family)
-- ------------------------------------------------




data CssValueFontFamily
  = CssValueFontFamilyList [T.Text]
  deriving (Eq, Show, Data)




makeCssPropertyFontFamily :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyFontFamily pat = (fmap . fmap) CssPropertyFontFamily (parser pat)
  where
    parser = interpretTokensAsStringList CssValueFontFamilyList




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




makeCssPropertyFontSize :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyFontSize pat = (fmap . fmap) CssPropertyFontSize (runParser parser pat)
  where
    parser = mkParserEnum cssValueFontSizeDict
             -- TODO: do we allow "1.0" (i.e. without unit) to be a valid value of font size?
             <|> Parser (interpretTokensAsLength False CssValueFontSizeDistance)




-- ------------------------------------------------
--
-- ------------------------------------------------




makeCssPropertyFontSizeAdjust :: CssValue -> CssProperty
makeCssPropertyFontSizeAdjust v = CssPropertyFontSizeAdjust v

makeCssPropertyFontStretch :: CssValue -> CssProperty
makeCssPropertyFontStretch v = CssPropertyFontStretch v




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




makeCssPropertyFontStyle :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyFontStyle pat = (fmap . fmap) CssPropertyFontStyle (runParser parser pat)
  where
    parser = mkParserEnum cssValueFontStyleDict




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




makeCssPropertyFontVariant :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyFontVariant pat = (fmap . fmap) CssPropertyFontVariant (runParser parser pat)
  where
    parser = mkParserEnum cssValueFontVariantDict




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




makeCssPropertyFontWeight :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyFontWeight pat = (fmap . fmap) CssPropertyFontWeight (runParser parser pat)
  where
    parser = mkParserEnum cssValueFontWeightDict
             <|> Parser (interpretTokensAsInteger CssValueFontWeightInt (100, 900))




-- ------------------------------------------------
-- Height (height)
-- https://www.w3.org/TR/CSS22/visudet.html#propdef-height
-- ------------------------------------------------




data CssValueHeight
  = CssValueHeightDistance CssDistance
  deriving (Data, Eq, Show)




-- TODO: CSS2.2 says: "Negative values for 'height' are illegal.". Implement this.
makeCssPropertyHeight :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyHeight pat = (fmap . fmap) CssPropertyHeight (runParser parser pat)
  where
    parser = Parser (interpretTokensAsLength False CssValueHeightDistance)
             <|> Parser (interpretTokensAsAuto CssValueHeightDistance)




-- ------------------------------------------------
--
-- ------------------------------------------------




makeCssPropertyLeft :: CssValue -> CssProperty
makeCssPropertyLeft v = CssPropertyLeft v




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




makeCssPropertyLetterSpacing :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyLetterSpacing pat = (fmap . fmap) CssPropertyLetterSpacing (runParser parser pat)
  where
    parser = mkParserEnum cssValueLetterSpacingDict
             <|> Parser (interpretTokensAsLength False CssValueLetterSpacingDistance)




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




makeCssPropertyLineHeight :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyLineHeight pat = (fmap . fmap) CssPropertyLineHeight (runParser parser pat)
  where
    parser = mkParserEnum cssValueLineHeightDict
             -- True: Original dillo code allowed unitless numeric values for
             -- zero and for values of type "length/percent/number". Line
             -- height was one of the properties that had this type.
             <|> Parser (interpretTokensAsLength True CssValueLineHeightDistance)




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




-- Parser of "list-style" property.
ctorCssPropertyListStyle :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
ctorCssPropertyListStyle pat = (fmap . fmap) CssPropertyListStyle (combinatorOneOrMoreUnordered2 initialValueListStyle fs pat)
  where
    fs = [fnType, fnPosition, fnImage]

    fnType :: (CssParser, CssToken) -> CssValueListStyle -> Maybe ((CssParser, CssToken), CssValueListStyle)
    fnType pat' acc     = fmap (\ x -> acc { listStyleType = x }) <$> ctorValueListStyleType pat'
    fnPosition pat' acc = fmap (\ x -> acc { listStylePosition = x }) <$> ctorValueListStylePosition pat'
    fnImage pat' acc    = fmap (\ x -> acc { listStyleImage = x }) <$> ctorValueListStyleImage pat'




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




ctorCssPropertyListStyleImage :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
ctorCssPropertyListStyleImage pat = (fmap . fmap) CssPropertyListStyleImage (ctorValueListStyleImage pat)




ctorValueListStyleImage :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssValueListStyleImage)
ctorValueListStyleImage pat = runParser parserValueListStyleImage pat




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




ctorCssPropertyListStylePosition :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
ctorCssPropertyListStylePosition pat = (fmap . fmap) CssPropertyListStylePosition (ctorValueListStylePosition pat)




ctorValueListStylePosition :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssValueListStylePosition)
ctorValueListStylePosition pat = runParser parserValueListStylePosition pat




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




ctorCssPropertyListStyleType :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
ctorCssPropertyListStyleType pat = (fmap . fmap) CssPropertyListStyleType (ctorValueListStyleType pat)




ctorValueListStyleType :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssValueListStyleType)
ctorValueListStyleType pat = runParser parserValueListStyleType pat




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




makeCssPropertyMargin :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyMargin pat = parse1234 parser pat CssPropertyMargin CssValueMargin
  where
    -- TODO: check if we should use 'many' or 'some' for space parsers.
    parser :: Parser (CssParser, CssToken) [CssValueMarginX]
    parser = some (many ignoreSpace *> marginValueParser <* many ignoreSpace)




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




makeCssPropertyMarginX :: (CssValueMarginX -> CssProperty) -> (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyMarginX propCtor pat = (fmap . fmap) propCtor (parser pat)
  where
    parser = parseTokensAsMarginValue



marginValueParser :: Parser (CssParser, CssToken) CssValueMarginX
marginValueParser = Parser (interpretTokensAsLength False CssValueMarginXDistance)
                    <|> Parser (interpretTokensAsAuto CssValueMarginXDistance)




parseTokensAsMarginValue :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssValueMarginX)
parseTokensAsMarginValue pat = runParser marginValueParser pat




makeCssPropertyMarginTop :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyMarginTop = makeCssPropertyMarginX CssPropertyMarginTop

makeCssPropertyMarginRight :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyMarginRight = makeCssPropertyMarginX CssPropertyMarginRight

makeCssPropertyMarginBottom :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyMarginBottom = makeCssPropertyMarginX CssPropertyMarginBottom

makeCssPropertyMarginLeft :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyMarginLeft = makeCssPropertyMarginX CssPropertyMarginLeft




-- ------------------------------------------------
--
-- ------------------------------------------------




makeCssPropertyMarkerOffset :: CssValue -> CssProperty
makeCssPropertyMarkerOffset v = CssPropertyMarkerOffset v

makeCssPropertyMarks :: CssValue -> CssProperty
makeCssPropertyMarks v = CssPropertyMarks v

makeCssPropertyMaxHeight :: CssValue -> CssProperty
makeCssPropertyMaxHeight v = CssPropertyMaxHeight v

makeCssPropertyMaxWidth :: CssValue -> CssProperty
makeCssPropertyMaxWidth v = CssPropertyMaxWidth v

makeCssPropertyMinHeight :: CssValue -> CssProperty
makeCssPropertyMinHeight v = CssPropertyMinHeight v

makeCssPropertyMinWidth :: CssValue -> CssProperty
makeCssPropertyMinWidth v = CssPropertyMinWidth v

makeCssPropertyOutlineColor :: CssValue -> CssProperty
makeCssPropertyOutlineColor v = CssPropertyOutlineColor v

makeCssPropertyOutlineStyle :: CssValue -> CssProperty
makeCssPropertyOutlineStyle v = CssPropertyOutlineStyle v

makeCssPropertyOutlineWidth :: CssValue -> CssProperty
makeCssPropertyOutlineWidth v = CssPropertyOutlineWidth v

makeCssPropertyOverflow :: CssValue -> CssProperty
makeCssPropertyOverflow v = CssPropertyOverflow v




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




makeCssPropertyPadding :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyPadding pat = parse1234 parser pat CssPropertyPadding CssValuePadding
  where
    -- TODO: check if we should use 'many' or 'some' for space parsers.
    parser :: Parser (CssParser, CssToken) [CssValuePaddingX]
    parser = some (many ignoreSpace *> paddingValueParser <* many ignoreSpace)




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




makeCssPropertyPaddingX :: (CssValuePaddingX -> CssProperty) -> (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyPaddingX propCtor pat = (fmap . fmap) propCtor (parser pat)
  where
    parser = parseTokensAsPaddingValue




parseTokensAsPaddingValue :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssValuePaddingX)
parseTokensAsPaddingValue pat = runParser paddingValueParser pat




 -- TODO: do we allow "1.0" (i.e. without unit) to be a valid value of padding?
paddingValueParser :: Parser (CssParser, CssToken) CssValuePaddingX
paddingValueParser = Parser $ interpretTokensAsLength False CssValuePaddingX




makeCssPropertyPaddingTop :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyPaddingTop    = makeCssPropertyPaddingX CssPropertyPaddingTop

makeCssPropertyPaddingRight :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyPaddingRight = makeCssPropertyPaddingX CssPropertyPaddingRight

makeCssPropertyPaddingBottom :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyPaddingBottom = makeCssPropertyPaddingX CssPropertyPaddingBottom

makeCssPropertyPaddingLeft :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyPaddingLeft = makeCssPropertyPaddingX CssPropertyPaddingLeft





-- ------------------------------------------------
--
-- ------------------------------------------------




makeCssPropertyPosition :: CssValue -> CssProperty
makeCssPropertyPosition v = CssPropertyPosition v




makeCssPropertyQuotes :: CssValue -> CssProperty
makeCssPropertyQuotes v = CssPropertyQuotes v




makeCssPropertyRight :: CssValue -> CssProperty
makeCssPropertyRight v = CssPropertyRight v




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




makeCssPropertyTextAlign :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyTextAlign pat = (fmap . fmap) CssPropertyTextAlign (runParser parser pat)
  where
    parser = mkParserEnum cssValueTextAlignDict




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




makeCssPropertyTextDecoration :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyTextDecoration pat = (fmap . fmap) CssPropertyTextDecoration (parser pat)
  where
    parser = interpretTokensAsMultiEnum cssValueTextDecorationDict




-- ------------------------------------------------
-- Text indent (text-indent)
-- ------------------------------------------------




data CssValueTextIndent
 = CssValueTextIndentDistance CssDistance
 deriving (Data, Eq, Show)




makeCssPropertyTextIndent :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyTextIndent pat = (fmap . fmap) CssPropertyTextIndent (parser pat)
  where
    parser = interpretTokensAsLength False CssValueTextIndentDistance




-- ------------------------------------------------
-- Text shadow (text-shadow)
-- ------------------------------------------------




makeCssPropertyTextShadow :: CssValue -> CssProperty
makeCssPropertyTextShadow v = CssPropertyTextShadow v




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




makeCssPropertyTextTransform :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyTextTransform pat = (fmap . fmap) CssPropertyTextTransform (runParser parser pat)
  where
    parser = mkParserEnum cssValueTextTransformDict




-- ------------------------------------------------
--
-- ------------------------------------------------




makeCssPropertyTop :: CssValue -> CssProperty
makeCssPropertyTop v = CssPropertyTop v



makeCssPropertyUnicodeBiDi :: CssValue -> CssProperty
makeCssPropertyUnicodeBiDi v = CssPropertyUnicodeBiDi v




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




makeCssPropertyVerticalAlign :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyVerticalAlign pat = (fmap . fmap) CssPropertyVerticalAlign (runParser parser pat)
  where
    parser = mkParserEnum cssValueVerticalAlignDict




-- ------------------------------------------------
-- Visibility (visibility)
-- ------------------------------------------------




makeCssPropertyVisibility :: CssValue -> CssProperty
makeCssPropertyVisibility v = CssPropertyVisibility v




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




makeCssPropertyWhitespace :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyWhitespace pat = (fmap . fmap) CssPropertyWhitespace (runParser parser pat)
  where
    parser = mkParserEnum cssValueWhitespaceDict




-- ------------------------------------------------
-- Width (width)
-- https://www.w3.org/TR/CSS22/visudet.html#propdef-width
-- ------------------------------------------------




data CssValueWidth
  = CssValueWidthDistance CssDistance
  deriving (Data, Eq, Show)




-- TODO: CSS2.2 says: "Negative values for 'width' are illegal.". Implement this.
makeCssPropertyWidth :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyWidth pat = (fmap . fmap) CssPropertyWidth (runParser parser pat)
  where
    parser = Parser (interpretTokensAsLength False CssValueWidthDistance)
             <|> Parser (interpretTokensAsAuto CssValueWidthDistance)




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




makeCssPropertyWordSpacing :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyWordSpacing pat = (fmap . fmap) CssPropertyWordSpacing (runParser parser pat)
  where
    parser = mkParserEnum cssValueWordSpacingDict
             <|> Parser (interpretTokensAsLength False CssValueWordSpacingDistance)




-- ------------------------------------------------
--
-- ------------------------------------------------




makeCssPropertyZIndex :: CssValue -> CssProperty
makeCssPropertyZIndex v = CssPropertyZIndex v




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




makeCssPropertyInvalid :: CssValue -> CssProperty
makeCssPropertyInvalid _ = CssPropertyInvalid




-- ------------------------------------------------
-- End of properties
-- ------------------------------------------------



{-
-- TODO: this implementation can correctly parse all values only if they
-- appear in input CSS in the same order as they appear in a list of ctors.
-- The example in CSS2.2 for "background" property suggests that values in
-- input CSS string may appear in any order. This function should be able to
-- handle this situation.
parseDeclarationMultiple :: (CssParser, CssToken) -> [PropertyCtor] -> ((CssParser, CssToken), [CssProperty])
parseDeclarationMultiple patArg propCtors = L.foldl f (patArg, []) propCtors
  where
    f (pat, acc) propCtor = case propCtor pat of
                              Just (pat', decl) -> (pat', acc ++ [decl])
                              Nothing           -> (pat, acc)
-}




-- I know that result of the parser will be ignored because it is used in
-- conjunction with *> or <* operators. But the "ignore" in the name should
-- stress that this parser can't be used in conjunction with operators other
-- than these two. The parser returns a constant dummy value that must not be
-- passed to next steps.
ignoreSpace :: Parser (CssParser, CssToken) ()
ignoreSpace = Parser $ \ (p, t) -> case (p, t) of
                                           -- The fact that I'm using () in
                                           -- returned value doesn't really
                                           -- matter since it will be ignored
                                           -- anyway.
                                           (p', CssTokWS) -> Just (nextToken p', ())
                                           _              -> Nothing




-- Parse 1, 2, 3 or 4 component values, where the component values are
-- specifying values for all sides of a box at once, or for t-b, r-l sides,
-- or for t, r-l, b sides, or for top, right, bottom, left sides.
--
-- See this part of CSS spec
-- (https://drafts.csswg.org/css-backgrounds-3/#propdef-border-style): "If
-- there is only one component value, it applies to all sides. If there are
-- two values, the top and bottom are set to the first value and the right
-- and left are set to the second [...]"
parse1234 :: Parser state [v] -> state -> (val -> prop) -> (v -> v -> v -> v -> val) -> Maybe (state, prop)
parse1234 parser pat propCtor valueCtor =
  case runParser parser pat of
    Just (pat', [top, right, bottom, left]) -> Just (pat', propCtor $ valueCtor top right bottom left)
    Just (pat', [top, rl, bottom])          -> Just (pat', propCtor $ valueCtor top rl    bottom rl)
    Just (pat', [tb, rl])                   -> Just (pat', propCtor $ valueCtor tb  rl    tb     rl)
    Just (pat', [v])                        -> Just (pat', propCtor $ valueCtor v   v     v      v)
    _                                       -> Nothing



