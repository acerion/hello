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
    CssProperty (..)
  , CssDeclaration (..)

  , CssValueBackgroundAttachment (..)
  , CssValueBackgroundColor (..)
  , CssValueBackgroundImage (..)
  , CssValueBackgroundPosition (..)
  , CssValueBackgroundRepeat (..)
  , CssValueBorderTRBL (..)
  , CssValueBorderCollapse (..)
  , CssValueBorderColor (..)
  , CssValueBorderSpacing (..)
  , CssValueBorderStyle (..)
  , CssValueBorderWidth (..)
  , CssValueColor (..)
  , CssValueContent (..)
  , CssValueDisplay (..)
  , CssValueCursor (..)
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

  , makeCssPropertyBackground
  , makeCssPropertyBackgroundAttachment
  , makeCssPropertyBackgroundColor
  , makeCssPropertyBackgroundImage
  , makeCssPropertyBackgroundPosition
  , makeCssPropertyBackgroundRepeat

  , makeCssPropertyBorder
  , makeCssPropertyBorderWidth
  , makeCssPropertyBorderColor
  , makeCssPropertyBorderStyle

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
  , makeCssPropertyFont
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

  , initialValueListStyleType
  , initialValueListStylePosition
  , initialValueListStyleImage

  , ShorthandPropertyCtor
  , PropertyCtor
  )
where




--import Debug.Trace

import Control.Applicative (Alternative(..), many)

import Data.Data
import Data.List as L
import Data.Text as T

import Hello.Css.Distance
import Hello.Css.Parser.Combinators
import Hello.Css.ParserHelpers
import Hello.Css.Tokenizer
import Hello.Css.Value

import Hello.Utils.Parser




-- I could make the two types equal by turning 'Maybe CssProperty' into
-- '[CssProperty]' in type of "normal" constructor. The normal constructor
-- would then return one-element list if parsing was successfull, and empty
-- list on non-successful parse. But I don't know if a list is as efficient
-- (in terms of resources) as Maybe.
type ShorthandPropertyCtor = (CssParser, CssToken) -> ((CssParser, CssToken), [CssProperty])
type PropertyCtor = (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)


type PropertyValueCtor a = (CssParser, CssToken) -> Maybe ((CssParser, CssToken), a)

-- TODO: this is too similar to PropertyCtor
type CssPropertyParser = MyParser (CssParser, CssToken) CssProperty





-- https://www.w3.org/TR/css-syntax-3/#declaration: "Conceptually,
-- declarations are a particular instance of associating a property or
-- descriptor name with a value. Syntactically, a declaration has a name, a
-- value consisting of a list of component values, and an important flag
-- which is initially unset."
data CssDeclaration = CssDeclaration
  { property  :: CssProperty
  , important :: Bool
  } deriving (Show, Eq)




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
  = CssPropertyBackgroundAttachment CssValueBackgroundAttachment      -- 0    parsing is unit-tested
  | CssPropertyBackgroundColor CssValueBackgroundColor                -- 1    parsing is unit-tested
  | CssPropertyBackgroundImage CssValueBackgroundImage                -- 2    This property is barely unit-tested because some decisions need to be made first.
  | CssPropertyBackgroundPosition CssValueBackgroundPosition          -- 3    There are some unit tests, but they don't really test much.
  | CssPropertyBackgroundRepeat CssValueBackgroundRepeat              -- 4
  | CssPropertyBorderTop CssValueBorderTRBL
  | CssPropertyBorderRight CssValueBorderTRBL
  | CssPropertyBorderBottom CssValueBorderTRBL
  | CssPropertyBorderLeft CssValueBorderTRBL
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
-- Background (background)
-- This is a shorthand property.
--
-- https://www.w3.org/TR/CSS22/colors.html#propdef-background
-- https://www.w3.org/TR/css-backgrounds-3/#background
-- ------------------------------------------------




-- TODO: this behaviour from CSS2.2 should be implemented:
--
-- "Given a valid declaration, the 'background' property first sets all the
-- individual background properties to their initial values, then assigns
-- explicit values given in the declaration."




makeCssPropertyBackground :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssProperty])
makeCssPropertyBackground pat = parseDeclarationMultiple
                                   pat
                                   [ makeCssPropertyBackgroundColor
                                   , makeCssPropertyBackgroundImage
                                   , makeCssPropertyBackgroundRepeat
                                   , makeCssPropertyBackgroundAttachment
                                   , makeCssPropertyBackgroundPosition
                                   ]




-- ------------------------------------------------
-- Background attachment (background-attachment)
-- ------------------------------------------------




data CssValueBackgroundAttachment
  = CssValueBackgroundAttachmentScroll
  | CssValueBackgroundAttachmentFixed
  deriving (Enum, Eq, Show, Data)




cssValueBackgroundAttachmentDict = [ ("scroll",  CssValueBackgroundAttachmentScroll)
                                   , ("fixed",   CssValueBackgroundAttachmentFixed)
                                   ]



fmapSnd ctor x = fmap (\(a, b) -> (a, ctor b)) x


makeCssPropertyBackgroundAttachment :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBackgroundAttachment pat = fmapSnd CssPropertyBackgroundAttachment (parser pat)
  where
    parser = interpretTokensAsEnum cssValueBackgroundAttachmentDict




-- ------------------------------------------------
-- Background color (background-color)
-- ------------------------------------------------




data CssValueBackgroundColor
  = CssValueBackgroundColorInherit
  | CssValueBackgroundColorColor Int -- TODO: Int or Color?
  deriving (Eq, Show, Data)




cssValueBackgroundColorDict = [ ("inherit",    CssValueBackgroundColorInherit)
                              ]




makeCssPropertyBackgroundColor :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBackgroundColor pat = fmapSnd CssPropertyBackgroundColor (runParser parser pat)
  where
    parser = (Parser $ interpretTokensAsEnum cssValueBackgroundColorDict)
             <|> (Parser $ interpretTokensAsColor CssValueBackgroundColorColor)




-- ------------------------------------------------
-- Background image (background-image)
-- ------------------------------------------------




data CssValueBackgroundImage
 = CssValueBackgroundImageUri T.Text -- TODO: change from T.Text to URI abstract type
 deriving (Data, Eq, Show)




makeCssPropertyBackgroundImage :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBackgroundImage pat = fmapSnd CssPropertyBackgroundImage (parser pat)
  where
    parser = interpretTokensAsURI CssValueBackgroundImageUri




-- ------------------------------------------------
-- Background position (background-position)
-- ------------------------------------------------




data CssValueBackgroundPosition
 = CssValueBackgroundPositionXY Int Int
 deriving (Data, Eq, Show)




makeCssPropertyBackgroundPosition :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBackgroundPosition pat = fmapSnd CssPropertyBackgroundPosition (parser pat)
  where
    parser = interpretTokensAsBgPosition CssValueBackgroundPositionXY




-- ------------------------------------------------
-- Background repeat (background-repeat)
-- ------------------------------------------------




data CssValueBackgroundRepeat
  = CssValueBackgroundRepeatRepeat
  | CssValueBackgroundRepeatRepeatX
  | CssValueBackgroundRepeatRepeatY
  | CssValueBackgroundRepeatNoRepeat
  deriving (Data, Enum, Eq, Show)




cssValueBackgroundRepeatDict = [ ("repeat",     CssValueBackgroundRepeatRepeat)
                               , ("repeat-x",   CssValueBackgroundRepeatRepeatX)
                               , ("repeat-y",   CssValueBackgroundRepeatRepeatY)
                               , ("no-repeat",  CssValueBackgroundRepeatNoRepeat)
                               ]




makeCssPropertyBackgroundRepeat :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBackgroundRepeat pat = fmapSnd CssPropertyBackgroundRepeat (parser pat)
  where
    parser = interpretTokensAsEnum cssValueBackgroundRepeatDict




-- ------------------------------------------------
-- Border (border)
-- This is a shorthand property.
-- ------------------------------------------------




-- Parse "{ border = X Y Z }" CSS declaration. Expand the single "border"
-- declaration into a series of "border-top-width", "border-left-color" etc.
-- properties with their values. Return the list of the expanded
-- declarations.
--
-- TODO: this implementation can correctly parse all value tokens only when
-- they appear in one specific order: first widht, then style, then color.
-- The function should be able to handle the tokens in any order.
makeCssPropertyBorder :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssProperty])
makeCssPropertyBorder pat0 = run constructors pat0 []
  where
    run :: [(CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)] -- constructors
        -> (CssParser, CssToken)  -- pat
        -> [CssProperty]          -- acc
        -> ((CssParser, CssToken), [CssProperty])
    run _  pat@(_, CssTokEnd) acc = (pat, acc)
    run (ctor:ctors) pat acc = case ctor pat of
                                 Just (_, prop) -> run ctors pat (acc ++ [prop])
                                 Nothing        -> case ctor (nextToken . fst $ pat) of
                                                     -- The 'case' above checks for possible method of recovery from Nothing.
                                                     Just _  -> (run (ctor:ctors) (nextToken . fst $ pat) acc) -- Recover by re-running current ctor with next pat.
                                                     Nothing -> (run ctors pat acc) -- Recover by passing current pat with next ctor.

    run []           pat acc = (pat, acc)

    constructors = [ makeCssPropertyBorderTopWidth
                   , makeCssPropertyBorderRightWidth
                   , makeCssPropertyBorderBottomWidth
                   , makeCssPropertyBorderLeftWidth

                   , makeCssPropertyBorderTopStyle
                   , makeCssPropertyBorderRightStyle
                   , makeCssPropertyBorderBottomStyle
                   , makeCssPropertyBorderLeftStyle

                   , makeCssPropertyBorderTopColor
                   , makeCssPropertyBorderRightColor
                   , makeCssPropertyBorderBottomColor
                   , makeCssPropertyBorderLeftColor
                   ]




-- ------------------------------------------------
-- Border width (border-width)
-- This is a shorthand property.
-- ------------------------------------------------




makeCssPropertyBorderWidth :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssProperty])
makeCssPropertyBorderWidth pat = parseDeclaration4321trbl
                                    pat
                                    [ CssPropertyBorderTopWidth
                                    , CssPropertyBorderRightWidth
                                    , CssPropertyBorderBottomWidth
                                    , CssPropertyBorderLeftWidth ]
                                    parseTokensAsBorderWidthValue




-- ------------------------------------------------
-- Border color (border-color)
-- This is a shorthand property.
-- ------------------------------------------------




makeCssPropertyBorderColor :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssProperty])
makeCssPropertyBorderColor pat = parseDeclaration4321trbl
                                    pat
                                    [ CssPropertyBorderTopColor
                                    , CssPropertyBorderRightColor
                                    , CssPropertyBorderBottomColor
                                    , CssPropertyBorderLeftColor ]
                                    parseTokensAsBorderColorValue




-- ------------------------------------------------
-- Border style (border-style)
-- This is a shorthand property.
-- ------------------------------------------------




makeCssPropertyBorderStyle :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssProperty])
makeCssPropertyBorderStyle pat = parseDeclaration4321trbl
                                    pat
                                    [ CssPropertyBorderTopStyle
                                    , CssPropertyBorderRightStyle
                                    , CssPropertyBorderBottomStyle
                                    , CssPropertyBorderLeftStyle ]
                                    parseTokensAsBorderStyleValue




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
defaultBorderTRBLWidth = CssValueBorderWidthMedium
defaultBorderTRBLStyle = CssValueBorderStyleNone
defaultBorderTRBLColor = cssValueCurrentColor




-- Parser of "border-top" property.
makeCssPropertyBorderTop :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBorderTop pat = fmapSnd CssPropertyBorderTop (combinatorOneOrMoreUnordered2 defaultValueBorderTRBL fs pat)
  where
    fs = [ wrapper1
         , wrapper2
         , wrapper3
         ]

wrapper1 pat acc = fmapSnd (\ x -> acc { borderTRBLWidth = x }) $ parseTokensAsBorderWidthValue pat
wrapper2 pat acc = fmapSnd (\ x -> acc { borderTRBLStyle = x }) $ parseTokensAsBorderStyleValue pat
wrapper3 pat acc = fmapSnd (\ x -> acc { borderTRBLColor = x }) $ parseTokensAsBorderColorValue pat




-- ------------------------------------------------
-- Border right (border-right)
-- This is a shorthand property.
-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-right
-- Unit-tested: yes
-- ------------------------------------------------




-- Parser of "border-right" property.
makeCssPropertyBorderRight :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBorderRight pat = fmapSnd CssPropertyBorderRight (combinatorOneOrMoreUnordered2 defaultValueBorderTRBL fs pat)
  where
    fs = [ wrapper1
         , wrapper2
         , wrapper3
         ]




-- ------------------------------------------------
-- Border bottom (border-bottom)
-- This is a shorthand property.
-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-bottom
-- Unit-tested: yes
-- ------------------------------------------------




-- Parser of "border-bottom" property.
makeCssPropertyBorderBottom :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBorderBottom pat = fmapSnd CssPropertyBorderBottom (combinatorOneOrMoreUnordered2 defaultValueBorderTRBL fs pat)
  where
    fs = [ wrapper1
         , wrapper2
         , wrapper3
         ]




-- ------------------------------------------------
-- Border left (border-left)
-- This is a shorthand property.
-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-left
-- Unit-tested: yes
-- ------------------------------------------------




-- Parser of "border-left" property.
makeCssPropertyBorderLeft :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBorderLeft pat = fmapSnd CssPropertyBorderLeft (combinatorOneOrMoreUnordered2 defaultValueBorderTRBL fs pat)
  where
    fs = [ wrapper1
         , wrapper2
         , wrapper3
         ]




-- ------------------------------------------------
-- Border collapse (border-collapse)
-- ------------------------------------------------




data CssValueBorderCollapse
  = CssValueBorderCollapseSeparate
  | CssValueBorderCollapseCollapse
  deriving (Bounded, Data, Enum, Eq, Show)




cssValueBorderCollapseDict = [ ("separate",   CssValueBorderCollapseSeparate)
                             , ("collapse",   CssValueBorderCollapseCollapse)
                             ]




makeCssPropertyBorderCollapse :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBorderCollapse pat = fmapSnd CssPropertyBorderCollapse (parser pat)
  where
    parser = interpretTokensAsEnum cssValueBorderCollapseDict




-- ------------------------------------------------
--
-- ------------------------------------------------




data CssValueBorderSpacing
 = CssValueBorderSpacingDistance CssDistance
 deriving (Eq, Show, Data)




makeCssPropertyBorderSpacing :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBorderSpacing pat = fmapSnd CssPropertyBorderSpacing (parser pat)
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
parseTokensAsBorderColorValue pat = runParser parser pat
  where
    parser = (Parser $ interpretTokensAsEnum cssValueBorderColorDict)
             <|> (Parser $ interpretTokensAsColor CssValueBorderColor)




makeCssPropertyBorderXColor :: (CssValueBorderColor -> CssProperty) -> (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBorderXColor propCtor pat = fmapSnd propCtor (parser pat)
  where
    parser = parseTokensAsBorderColorValue




makeCssPropertyBorderTopColor    = makeCssPropertyBorderXColor CssPropertyBorderTopColor
makeCssPropertyBorderRightColor  = makeCssPropertyBorderXColor CssPropertyBorderRightColor
makeCssPropertyBorderBottomColor = makeCssPropertyBorderXColor CssPropertyBorderBottomColor
makeCssPropertyBorderLeftColor   = makeCssPropertyBorderXColor CssPropertyBorderLeftColor




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
parseTokensAsBorderStyleValue pat = parser pat
  where
    parser = interpretTokensAsEnum cssValueBorderStyleDict




makeCssPropertyBorderXStyle :: (CssValueBorderStyle -> CssProperty) -> (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBorderXStyle propCtor pat = fmapSnd propCtor (parser pat)
  where
    parser = parseTokensAsBorderStyleValue




makeCssPropertyBorderTopStyle    = makeCssPropertyBorderXStyle CssPropertyBorderTopStyle
makeCssPropertyBorderRightStyle  = makeCssPropertyBorderXStyle CssPropertyBorderRightStyle
makeCssPropertyBorderBottomStyle = makeCssPropertyBorderXStyle CssPropertyBorderBottomStyle
makeCssPropertyBorderLeftStyle   = makeCssPropertyBorderXStyle CssPropertyBorderLeftStyle




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
parseTokensAsBorderWidthValue pat = runParser parser pat
  where
    parser = (Parser $ interpretTokensAsEnum cssValueBorderWidthDict)
             <|> (Parser $ interpretTokensAsLength False CssValueBorderWidthDistance)




makeCssPropertyBorderXWidth :: (CssValueBorderWidth -> CssProperty) -> (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyBorderXWidth propCtor pat = fmapSnd propCtor (parser pat)
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




makeCssPropertyBottom v = CssPropertyBottom v
makeCssPropertyCaptionSide v = CssPropertyCaptionSide v
makeCssPropertyClear v = CssPropertyClear v
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
makeCssPropertyColor pat = fmapSnd CssPropertyColor (runParser parser pat)
  where
    parser = (Parser $ interpretTokensAsEnum cssValueColorDict)
             <|> (Parser $ interpretTokensAsColor CssValueColor)




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
makeCssPropertyContent pat = fmapSnd CssPropertyContent (parser pat)
  where
    parser = interpretTokensAsString CssValueContent




-- ------------------------------------------------
--
-- ------------------------------------------------




makeCssPropertyCounterIncrement v = CssPropertyCounterIncrement v
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
makeCssPropertyCursor pat = fmapSnd CssPropertyCursor (parser pat)
  where
    parser = interpretTokensAsEnum cssValueCursorDict





-- ------------------------------------------------
--
-- ------------------------------------------------




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
makeCssPropertyDisplay pat = fmapSnd CssPropertyDisplay (parser pat)
  where
    parser = interpretTokensAsEnum cssValueDisplayDict




-- ------------------------------------------------
--
-- ------------------------------------------------




makeCssPropertyEmptyCells v = CssPropertyEmptyCells v
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
fontEnum2 pat = case parseEnum cssValueFontDict pat of
                  Just (pat', _) -> (pat', Just []) -- TODO correctly handle enum values
                  Nothing        -> (pat, Nothing)




parseEnum :: [(T.Text, b)] -> (CssParser, CssToken) -> Maybe ((CssParser, CssToken), b)
parseEnum dictionary pat = parser pat
  where
    parser = interpretTokensAsEnum dictionary




-- ------------------------------------------------
-- Font family (font-family)
-- ------------------------------------------------




data CssValueFontFamily
  = CssValueFontFamilyList [T.Text]
  deriving (Eq, Show, Data)




makeCssPropertyFontFamily :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyFontFamily pat = fmapSnd CssPropertyFontFamily (parser pat)
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
makeCssPropertyFontSize pat = fmapSnd CssPropertyFontSize (runParser parser pat)
  where
    parser = (Parser $ interpretTokensAsEnum cssValueFontSizeDict)
             -- TODO: do we allow "1.0" (i.e. without unit) to be a valid value of font size?
             <|> (Parser $ interpretTokensAsLength False CssValueFontSizeDistance)




-- ------------------------------------------------
--
-- ------------------------------------------------




makeCssPropertyFontSizeAdjust v = CssPropertyFontSizeAdjust v
makeCssPropertyFontStretch v = CssPropertyFontStretch v




-- ------------------------------------------------
-- Font style (font-style)
-- ------------------------------------------------




data CssValueFontStyle
  = CssValueFontStyleNormal
  | CssValueFontStyleItalic
  | CssValueFontStyleOblique
 deriving (Eq, Show, Data, Enum)




cssValueFontStyleDict = [ ("normal",  CssValueFontStyleNormal)
                        , ("italic",  CssValueFontStyleItalic)
                        , ("oblique", CssValueFontStyleOblique)
                        ]




makeCssPropertyFontStyle :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyFontStyle pat = fmapSnd CssPropertyFontStyle (parser pat)
  where
    parser = interpretTokensAsEnum cssValueFontStyleDict




-- ------------------------------------------------
-- Font variant (font-variant)
-- ------------------------------------------------




data CssValueFontVariant
  = CssValueFontVariantNormal
  | CssValueFontVariantSmallCaps
 deriving (Eq, Show, Data, Enum)




cssValueFontVariantDict = [ ("normal",  CssValueFontVariantNormal)
                          , ("small-caps",  CssValueFontVariantSmallCaps)
                          ]




makeCssPropertyFontVariant :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyFontVariant pat = fmapSnd CssPropertyFontVariant (parser pat)
  where
    parser = interpretTokensAsEnum cssValueFontVariantDict




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




cssValueFontWeightDict = [ ("normal",  CssValueFontWeightNormal)
                         , ("bold",    CssValueFontWeightBold)
                         , ("bolder",  CssValueFontWeightBolder)
                         , ("lighter", CssValueFontWeightLighter)
                         ] -- dillo also included "light" value in this list.




makeCssPropertyFontWeight :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyFontWeight pat = fmapSnd CssPropertyFontWeight (runParser parser pat)
  where
    parser = (Parser $ interpretTokensAsEnum cssValueFontWeightDict)
             <|> (Parser $ interpretTokensAsInteger CssValueFontWeightInt (100, 900))




-- ------------------------------------------------
-- Height (height)
-- https://www.w3.org/TR/CSS22/visudet.html#propdef-height
-- ------------------------------------------------




data CssValueHeight
  = CssValueHeightDistance CssDistance
  deriving (Data, Eq, Show)




-- TODO: CSS2.2 says: "Negative values for 'height' are illegal.". Implement this.
makeCssPropertyHeight :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyHeight pat = fmapSnd CssPropertyHeight (runParser parser pat)
  where
    parser = (Parser $ interpretTokensAsLength False CssValueHeightDistance)
             <|> (Parser $ interpretTokensAsAuto CssValueHeightDistance)




-- ------------------------------------------------
--
-- ------------------------------------------------




makeCssPropertyLeft v = CssPropertyLeft v




-- ------------------------------------------------
-- Letter spacing (letter-spacing)
-- ------------------------------------------------




data CssValueLetterSpacing
  = CssValueLetterSpacingNormal
  | CssValueLetterSpacingDistance CssDistance
  deriving (Data, Eq, Show)




cssValueLetterSpacingDict = [ ("normal",    CssValueLetterSpacingNormal)
                            ]




makeCssPropertyLetterSpacing :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyLetterSpacing pat = fmapSnd CssPropertyLetterSpacing (runParser parser pat)
  where
    parser = (Parser $ interpretTokensAsEnum cssValueLetterSpacingDict)
             <|> (Parser $ interpretTokensAsLength False CssValueLetterSpacingDistance)




-- ------------------------------------------------
-- Line height (line-height)
-- ------------------------------------------------




data CssValueLineHeight
  = CssValueLineHeightNormal
  | CssValueLineHeightDistance CssDistance
  deriving (Data, Eq, Show)




cssValueLineHeightDict = [ ("normal",    CssValueLineHeightNormal)
                         ]




makeCssPropertyLineHeight :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyLineHeight pat = fmapSnd CssPropertyLineHeight (runParser parser pat)
  where
    parser = (Parser $ interpretTokensAsEnum cssValueLineHeightDict)
             -- True: Original dillo code allowed unitless numeric values for
             -- zero and for values of type "length/percent/number". Line
             -- height was one of the properties that had this type.
             <|> (Parser $ interpretTokensAsLength True CssValueLineHeightDistance)




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

initialValueListStyle = CssValueListStyle initialValueListStyleType initialValueListStylePosition initialValueListStyleImage




-- Parser of "list-style" property.
ctorCssPropertyListStyle :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
ctorCssPropertyListStyle pat = fmapSnd CssPropertyListStyle (combinatorOneOrMoreUnordered2 initialValueListStyle fs pat)
  where
    fs = [fnType, fnPosition, fnImage]

    fnType :: (CssParser, CssToken) -> CssValueListStyle -> Maybe ((CssParser, CssToken), CssValueListStyle)
    fnType pat' acc     = fmapSnd (\ x -> acc { listStyleType = x }) $ ctorValueListStyleType pat'
    fnPosition pat' acc = fmapSnd (\ x -> acc { listStylePosition = x }) $ ctorValueListStylePosition pat'
    fnImage pat' acc    = fmapSnd (\ x -> acc { listStyleImage = x }) $ ctorValueListStyleImage pat'




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

initialValueListStyleImage = CssValueListStyleImageNone




ctorCssPropertyListStyleImage :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
ctorCssPropertyListStyleImage pat = fmapSnd CssPropertyListStyleImage (ctorValueListStyleImage pat)




ctorValueListStyleImage :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssValueListStyleImage)
ctorValueListStyleImage pat = runParser parserValueListStyleImage pat




parserValueListStyleImage :: Parser (CssParser, CssToken) CssValueListStyleImage
parserValueListStyleImage = Parser $ (\ _ -> Nothing) -- TODO: implement parsing of the value




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

initialValueListStylePosition = CssValueListStylePositionOutside




cssValueListStylePositionDict :: [(T.Text, CssValueListStylePosition)]
cssValueListStylePositionDict = [ ("inside",               CssValueListStylePositionInside)
                                , ("outside",              CssValueListStylePositionOutside)
                                ]




ctorCssPropertyListStylePosition :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
ctorCssPropertyListStylePosition pat = fmapSnd CssPropertyListStylePosition (ctorValueListStylePosition pat)




ctorValueListStylePosition pat = runParser parserValueListStylePosition pat




parserValueListStylePosition :: Parser (CssParser, CssToken) CssValueListStylePosition
parserValueListStylePosition = Parser $ interpretTokensAsEnum cssValueListStylePositionDict




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
ctorCssPropertyListStyleType pat = fmapSnd CssPropertyListStyleType (ctorValueListStyleType pat)




ctorValueListStyleType pat = runParser parserValueListStyleType pat




parserValueListStyleType :: Parser (CssParser, CssToken) CssValueListStyleType
parserValueListStyleType = Parser $ interpretTokensAsEnum cssValueListStyleTypeDict




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
makeCssPropertyMargin pat =
  case runParser parser pat of
    Just (x, [top, right, bottom, left]) -> Just (x, CssPropertyMargin $ CssValueMargin top right bottom left)
    Just (x, [top, rl, bottom])          -> Just (x, CssPropertyMargin $ CssValueMargin top rl    bottom rl)
    Just (x, [tb, rl])                   -> Just (x, CssPropertyMargin $ CssValueMargin tb  rl    tb     rl)
    Just (x, [v])                        -> Just (x, CssPropertyMargin $ CssValueMargin v   v     v      v)
    _                                    -> Nothing
  where
    -- TODO: check if we should use 'many' or 'some' for space parsers.
    parser :: Parser (CssParser, CssToken) [CssValueMarginX]
    parser = some (many ignoreSpace *> marginValueParser <* many ignoreSpace)

    -- TODO: use ignoreSpace'
    ignoreSpace :: Parser (CssParser, CssToken) CssValueMarginX
    ignoreSpace = Parser $ \ (p, t) -> case (p, t) of
                                         -- The fact that I'm using CssValueMarginXDistance in
                                         -- returned value doesn't really matter since it will
                                         -- be ignored anyway.
                                         (p', CssTokWS) -> Just ((nextToken p'), CssValueMarginXDistance CssDistanceAuto)
                                         _             -> Nothing






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
makeCssPropertyMarginX propCtor pat = fmapSnd propCtor (parser pat)
  where
    parser = parseTokensAsMarginValue



marginValueParser :: Parser (CssParser, CssToken) CssValueMarginX
marginValueParser = (Parser $ interpretTokensAsLength False CssValueMarginXDistance)
                    <|> (Parser $ interpretTokensAsAuto CssValueMarginXDistance)




parseTokensAsMarginValue :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssValueMarginX)
parseTokensAsMarginValue pat = runParser marginValueParser pat




makeCssPropertyMarginTop    = makeCssPropertyMarginX CssPropertyMarginTop
makeCssPropertyMarginRight  = makeCssPropertyMarginX CssPropertyMarginRight
makeCssPropertyMarginBottom = makeCssPropertyMarginX CssPropertyMarginBottom
makeCssPropertyMarginLeft   = makeCssPropertyMarginX CssPropertyMarginLeft




-- ------------------------------------------------
--
-- ------------------------------------------------




makeCssPropertyMarkerOffset v = CssPropertyMarkerOffset v
makeCssPropertyMarks v = CssPropertyMarks v
makeCssPropertyMaxHeight v = CssPropertyMaxHeight v
makeCssPropertyMaxWidth v = CssPropertyMaxWidth v
makeCssPropertyMinHeight v = CssPropertyMinHeight v
makeCssPropertyMinWidth v = CssPropertyMinWidth v
makeCssPropertyOutlineColor v = CssPropertyOutlineColor v
makeCssPropertyOutlineStyle v = CssPropertyOutlineStyle v
makeCssPropertyOutlineWidth v = CssPropertyOutlineWidth v
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
makeCssPropertyPadding pat =
  case runParser parser pat of
    Just (x, [top, right, bottom, left]) -> Just (x, CssPropertyPadding $ CssValuePadding top right bottom left)
    Just (x, [top, rl, bottom])          -> Just (x, CssPropertyPadding $ CssValuePadding top rl    bottom rl)
    Just (x, [tb, rl])                   -> Just (x, CssPropertyPadding $ CssValuePadding tb  rl    tb     rl)
    Just (x, [v])                        -> Just (x, CssPropertyPadding $ CssValuePadding v   v     v      v)
    _                                    -> Nothing
  where
    -- TODO: check if we should use 'many' or 'some' for space parsers.
    parser :: Parser (CssParser, CssToken) [CssValuePaddingX]
    parser = some (many ignoreSpace *> paddingValueParser <* many ignoreSpace)

    ignoreSpace = ignoreSpace' (CssValuePaddingX CssDistanceAuto)




ignoreSpace' :: a -> Parser (CssParser, CssToken) a
ignoreSpace' dummy = Parser $ \ (p, t) -> case (p, t) of
                                            -- The fact that I'm using dummy in
                                            -- returned value doesn't really matter since it will
                                            -- be ignored anyway.
                                            (p', CssTokWS) -> Just ((nextToken p'), dummy)
                                            _              -> Nothing




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
makeCssPropertyPaddingX propCtor pat = fmapSnd propCtor (parser pat)
  where
    parser = parseTokensAsPaddingValue




parseTokensAsPaddingValue :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssValuePaddingX)
parseTokensAsPaddingValue pat = runParser paddingValueParser pat




 -- TODO: do we allow "1.0" (i.e. without unit) to be a valid value of padding?
paddingValueParser :: Parser (CssParser, CssToken) CssValuePaddingX
paddingValueParser = Parser $ interpretTokensAsLength False CssValuePaddingX




makeCssPropertyPaddingTop    = makeCssPropertyPaddingX CssPropertyPaddingTop
makeCssPropertyPaddingRight  = makeCssPropertyPaddingX CssPropertyPaddingRight
makeCssPropertyPaddingBottom = makeCssPropertyPaddingX CssPropertyPaddingBottom
makeCssPropertyPaddingLeft   = makeCssPropertyPaddingX CssPropertyPaddingLeft





-- ------------------------------------------------
--
-- ------------------------------------------------




makeCssPropertyPosition v = CssPropertyPosition v
makeCssPropertyQuotes v = CssPropertyQuotes v
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




cssValueTextAlignDict = [ ("left",    CssValueTextAlignLeft)
                        , ("right",   CssValueTextAlignRight)
                        , ("center",  CssValueTextAlignCenter)
                        , ("justify", CssValueTextAlignJustify)
                        ]




makeCssPropertyTextAlign :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyTextAlign pat = fmapSnd CssPropertyTextAlign (parser pat)
  where
    parser = interpretTokensAsEnum cssValueTextAlignDict




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




cssValueTextDecorationDict = [ ("underline",     CssValueTextDecorationUnderline)
                             , ("overline",      CssValueTextDecorationOverline)
                             , ("line-through",  CssValueTextDecorationLineThrough)
                             , ("blink",         CssValueTextDecorationBlink)
                             ]




makeCssPropertyTextDecoration :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyTextDecoration pat = fmapSnd CssPropertyTextDecoration (parser pat)
  where
    parser = interpretTokensAsMultiEnum cssValueTextDecorationDict




-- ------------------------------------------------
-- Text indent (text-indent)
-- ------------------------------------------------




data CssValueTextIndent
 = CssValueTextIndentDistance CssDistance
 deriving (Data, Eq, Show)




makeCssPropertyTextIndent :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyTextIndent pat = fmapSnd CssPropertyTextIndent (parser pat)
  where
    parser = interpretTokensAsLength False CssValueTextIndentDistance




-- ------------------------------------------------
-- Text shadow (text-shadow)
-- ------------------------------------------------




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




cssValueTextTransformDict = [ ("none",       CssValueTextTransformNone)
                            , ("capitalize", CssValueTextTransformCapitalize)
                            , ("uppercase",  CssValueTextTransformUppercase)
                            , ("lowercase",  CssValueTextTransformLowercase)
                            ]




makeCssPropertyTextTransform :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyTextTransform pat = fmapSnd CssPropertyTextTransform (parser pat)
  where
    parser = interpretTokensAsEnum cssValueTextTransformDict




-- ------------------------------------------------
--
-- ------------------------------------------------




makeCssPropertyTop v = CssPropertyTop v
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
makeCssPropertyVerticalAlign pat = fmapSnd CssPropertyVerticalAlign (parser pat)
  where
    parser = interpretTokensAsEnum cssValueVerticalAlignDict




-- ------------------------------------------------
-- Visibility (visibility)
-- ------------------------------------------------



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
makeCssPropertyWhitespace pat = fmapSnd CssPropertyWhitespace (parser pat)
  where
    parser = interpretTokensAsEnum cssValueWhitespaceDict




-- ------------------------------------------------
-- Width (width)
-- https://www.w3.org/TR/CSS22/visudet.html#propdef-width
-- ------------------------------------------------




data CssValueWidth
  = CssValueWidthDistance CssDistance
  deriving (Data, Eq, Show)




-- TODO: CSS2.2 says: "Negative values for 'width' are illegal.". Implement this.
makeCssPropertyWidth :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyWidth pat = fmapSnd CssPropertyWidth (runParser parser pat)
  where
    parser = (Parser $ interpretTokensAsLength False CssValueWidthDistance)
             <|> (Parser $ interpretTokensAsAuto CssValueWidthDistance)




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




makeCssPropertyWordSpacing :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssProperty)
makeCssPropertyWordSpacing pat = fmapSnd CssPropertyWordSpacing (runParser parser pat)
  where
    parser = (Parser $ interpretTokensAsEnum cssValueWordSpacingDict)
             <|> (Parser $ interpretTokensAsLength False CssValueWordSpacingDistance)




-- ------------------------------------------------
--
-- ------------------------------------------------




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




makeCssPropertyInvalid _ = CssPropertyInvalid




-- ------------------------------------------------
-- End of properties
-- ------------------------------------------------




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




-- Parse 4, 3, 2 or 1 tokens, specifying values for top, right, bottom, left,
-- or for t, r-l, b, or for t-b, r-l, or for all of them at once.
parseDeclaration4321trbl :: (CssParser, CssToken) -> [b -> CssProperty] -> PropertyValueCtor b -> ((CssParser, CssToken), [CssProperty])
parseDeclaration4321trbl pat (propCtorT:propCtorR:propCtorB:propCtorL:_) propValueCtor = (pat', ds)
  where
    ds = case propertyValues of
           [top, right, bottom, left] -> [ propCtorT top, propCtorR right, propCtorB bottom, propCtorL left ]
           [top, rl, bottom]          -> [ propCtorT top, propCtorR rl,    propCtorB bottom, propCtorL rl   ]
           [tb, rl]                   -> [ propCtorT tb,  propCtorR rl,    propCtorB tb,     propCtorL rl   ]
           [v]                        -> [ propCtorT v,   propCtorR v,     propCtorB v,      propCtorL v    ]
           _                          -> []
    (pat', propertyValues) = matchOrderedTokens pat propValueCtor []
parseDeclaration4321trbl pat _ _ = (pat, [])




-- Value tokens must be in proper order. Example: if property is
-- "border-color", and there are four value tokens, then tokens must
-- represent colors of "top","right","bottom","left" borders.
matchOrderedTokens :: (CssParser, CssToken) -> PropertyValueCtor b -> [b] -> ((CssParser, CssToken), [b])
matchOrderedTokens pat propValueCtor propertyValues =
  case propValueCtor pat of
    Just (pat', v) -> matchOrderedTokens pat' propValueCtor (propertyValues ++ [v])
    Nothing        -> (pat, propertyValues)




