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
  , makeCssDeclarationBackgroundAttachment
  , makeCssDeclarationBackgroundColor
  , makeCssDeclarationBackgroundImage
  , makeCssDeclarationBackgroundPosition
  , makeCssDeclarationBackgroundRepeat
  , makeCssDeclarationBorderBottomColor
  , makeCssDeclarationBorderBottomStyle
  , makeCssDeclarationBorderBottomWidth
  , makeCssDeclarationBorderCollapse
  , makeCssDeclarationBorderLeftColor
  , makeCssDeclarationBorderLeftStyle
  , makeCssDeclarationBorderLeftWidth
  , makeCssDeclarationBorderRightColor
  , makeCssDeclarationBorderRightStyle
  , makeCssDeclarationBorderRightWidth
  , makeCssDeclarationBorderSpacing
  , makeCssDeclarationBorderTopColor
  , makeCssDeclarationBorderTopStyle
  , makeCssDeclarationBorderTopWidth
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
  , makeCssDeclarationXLink
  , makeCssDeclarationXColSpan
  , makeCssDeclarationXRowSpan
  , makeCssDeclarationXLang
  , makeCssDeclarationXImg
  , makeCssDeclarationXTooltip
  , makeCssDeclaration_LAST
  )
where




import Debug.Trace

import Data.Data
import Data.Text as T

import Hello.Css.Value




-- A property in css declaration, but with a value.
data CssDeclaration
  = CssDeclarationBackgroundAttachment CssValue         -- 0
  | CssDeclarationBackgroundColor CssValue              -- 1
  | CssDeclarationBackgroundImage CssValue              -- 2
  | CssDeclarationBackgroundPosition CssValue           -- 3
  | CssDeclarationBackgroundRepeat CssValue             -- 4
  | CssDeclarationBorderBottomColor CssValue            -- 5
  | CssDeclarationBorderBottomStyle CssValue            -- 6
  | CssDeclarationBorderBottomWidth CssValue            -- 7
  | CssDeclarationBorderCollapse CssValue               -- 8
  | CssDeclarationBorderLeftColor CssValue              -- 9
  | CssDeclarationBorderLeftStyle CssValue              -- 10
  | CssDeclarationBorderLeftWidth CssValue              -- 11
  | CssDeclarationBorderRightColor CssValue             -- 12
  | CssDeclarationBorderRightStyle CssValue             -- 13
  | CssDeclarationBorderRightWidth CssValue             -- 14
  | CssDeclarationBorderSpacing CssValue                -- 15
  | CssDeclarationBorderTopColor CssValue               -- 16
  | CssDeclarationBorderTopStyle CssValue               -- 17
  | CssDeclarationBorderTopWidth CssValue               -- 18
  | CssDeclarationBottom CssValue                       -- 19
  | CssDeclarationCaptionSide CssValue                  -- 20
  | CssDeclarationClear CssValue                        -- 21
  | CssDeclarationClip CssValue                         -- 22
  | CssDeclarationColor CssValue                        -- 23
  | CssDeclarationContent CssValue                      -- 24
  | CssDeclarationCounterIncrement CssValue             -- 25
  | CssDeclarationCounterReset CssValue                 -- 26
  | CssDeclarationCursor CssValue                       -- 27
  | CssDeclarationDirection CssValue                    -- 28
  | CssDeclarationDisplay CssValue                      -- 29
  | CssDeclarationEmptyCells CssValue                   -- 30
  | CssDeclarationFloat CssValue                        -- 31
  | CssDeclarationFontFamily CssValue                   -- 32
  | CssDeclarationFontSize CssValue                     -- 33
  | CssDeclarationFontSizeAdjust CssValue               -- 34
  | CssDeclarationFontStretch CssValue                  -- 35
  | CssDeclarationFontStyle CssValue                    -- 36
  | CssDeclarationFontVariant CssValue                  -- 37
  | CssDeclarationFontWeight CssValue                   -- 38
  | CssDeclarationHeight CssValue                       -- 39
  | CssDeclarationLeft CssValue                         -- 40
  | CssDeclarationLetterSpacing CssValue                -- 41
  | CssDeclarationLineHeight CssValue                   -- 42
  | CssDeclarationListStyleImage CssValue               -- 43
  | CssDeclarationListStylePosition CssValue            -- 44
  | CssDeclarationListStyleType CssValue                -- 45
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
  | CssDeclarationPaddingBottom CssValue                -- 60
  | CssDeclarationPaddingLeft CssValue                  -- 61
  | CssDeclarationPaddingRight CssValue                 -- 62
  | CssDeclarationPaddingTop CssValue                   -- 63
  | CssDeclarationPosition CssValue                     -- 64
  | CssDeclarationQuotes CssValue                       -- 65
  | CssDeclarationRight CssValue                        -- 66
  | CssDeclarationTextAlign CssValue                    -- 67
  | CssDeclarationTextDecoration CssValue               -- 68
  | CssDeclarationTextIndent CssValue                   -- 69
  | CssDeclarationTextShadow CssValue                   -- 70
  | CssDeclarationTextTransform CssValue                -- 71
  | CssDeclarationTop CssValue                          -- 72
  | CssDeclarationUnicodeBiDi CssValue                  -- 73
  | CssDeclarationVerticalAlign CssValue                -- 74
  | CssDeclarationVisibility CssValue                   -- 75
  | CssDeclarationWhitespace CssValue                   -- 76
  | CssDeclarationWidth CssValue                        -- 77
  | CssDeclarationWordSpacing CssValue                  -- 78
  | CssDeclarationZIndex CssValue                       -- 79

  -- Pseudo-property used internally by dillo/hello. Without it following
  -- a/href links won't work.
  | CssDeclarationXLink CssValue                        -- 80

  | CssDeclarationXColSpan CssValue                     -- 81
  | CssDeclarationXRowSpan CssValue                     -- 82

  -- Pseudo-property for "lang" or "xml:lang" attribute of html element.
  | CssDeclarationXLang CssValue                        -- 83

  -- Pseudo-property used (probably) to index images in a html document.
  | CssDeclarationXImg CssValue                         -- 84
  | CssDeclarationXTooltip CssValue                     -- 85

  | CssDeclaration_LAST                                 -- 86
  deriving (Eq, Show, Data)





makeCssDeclarationBackgroundAttachment v = CssDeclarationBackgroundAttachment v
makeCssDeclarationBackgroundColor v = CssDeclarationBackgroundColor v
makeCssDeclarationBackgroundImage v = CssDeclarationBackgroundImage v
makeCssDeclarationBackgroundPosition v = CssDeclarationBackgroundPosition v
makeCssDeclarationBackgroundRepeat v = CssDeclarationBackgroundRepeat v
makeCssDeclarationBorderBottomColor v = CssDeclarationBorderBottomColor v
makeCssDeclarationBorderBottomStyle v = CssDeclarationBorderBottomStyle v
makeCssDeclarationBorderBottomWidth v = CssDeclarationBorderBottomWidth v
makeCssDeclarationBorderCollapse v = CssDeclarationBorderCollapse v
makeCssDeclarationBorderLeftColor v = CssDeclarationBorderLeftColor v
makeCssDeclarationBorderLeftStyle v = CssDeclarationBorderLeftStyle v
makeCssDeclarationBorderLeftWidth v = CssDeclarationBorderLeftWidth v
makeCssDeclarationBorderRightColor v = CssDeclarationBorderRightColor v
makeCssDeclarationBorderRightStyle v = CssDeclarationBorderRightStyle v
makeCssDeclarationBorderRightWidth v = CssDeclarationBorderRightWidth v
makeCssDeclarationBorderSpacing v = CssDeclarationBorderSpacing v
makeCssDeclarationBorderTopColor v = CssDeclarationBorderTopColor v
makeCssDeclarationBorderTopStyle v = CssDeclarationBorderTopStyle v
makeCssDeclarationBorderTopWidth v = CssDeclarationBorderTopWidth v
makeCssDeclarationBottom v = CssDeclarationBottom v
makeCssDeclarationCaptionSide v = CssDeclarationCaptionSide v
makeCssDeclarationClear v = CssDeclarationClear v
makeCssDeclarationClip v = CssDeclarationClip v
makeCssDeclarationColor v = CssDeclarationColor v
makeCssDeclarationContent v = CssDeclarationContent v
makeCssDeclarationCounterIncrement v = CssDeclarationCounterIncrement v
makeCssDeclarationCounterReset v = CssDeclarationCounterReset v
makeCssDeclarationCursor v = CssDeclarationCursor v
makeCssDeclarationDirection v = CssDeclarationDirection v
makeCssDeclarationDisplay v = CssDeclarationDisplay v
makeCssDeclarationEmptyCells v = CssDeclarationEmptyCells v
makeCssDeclarationFloat v = CssDeclarationFloat v
makeCssDeclarationFontFamily v = CssDeclarationFontFamily v
makeCssDeclarationFontSize v = CssDeclarationFontSize v
makeCssDeclarationFontSizeAdjust v = CssDeclarationFontSizeAdjust v
makeCssDeclarationFontStretch v = CssDeclarationFontStretch v
makeCssDeclarationFontStyle v = CssDeclarationFontStyle v
makeCssDeclarationFontVariant v = CssDeclarationFontVariant v
makeCssDeclarationFontWeight v = CssDeclarationFontWeight v
makeCssDeclarationHeight v = CssDeclarationHeight v
makeCssDeclarationLeft v = CssDeclarationLeft v
makeCssDeclarationLetterSpacing v = CssDeclarationLetterSpacing v
makeCssDeclarationLineHeight v = CssDeclarationLineHeight v
makeCssDeclarationListStyleImage v = CssDeclarationListStyleImage v
makeCssDeclarationListStylePosition v = CssDeclarationListStylePosition v
makeCssDeclarationListStyleType v = CssDeclarationListStyleType v
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
makeCssDeclarationPaddingBottom v = CssDeclarationPaddingBottom v
makeCssDeclarationPaddingLeft v = CssDeclarationPaddingLeft v
makeCssDeclarationPaddingRight v = CssDeclarationPaddingRight v
makeCssDeclarationPaddingTop v = CssDeclarationPaddingTop v
makeCssDeclarationPosition v = CssDeclarationPosition v
makeCssDeclarationQuotes v = CssDeclarationQuotes v
makeCssDeclarationRight v = CssDeclarationRight v
makeCssDeclarationTextAlign v = CssDeclarationTextAlign v
makeCssDeclarationTextDecoration v = CssDeclarationTextDecoration v
makeCssDeclarationTextIndent v = CssDeclarationTextIndent v
makeCssDeclarationTextShadow v = CssDeclarationTextShadow v
makeCssDeclarationTextTransform v = CssDeclarationTextTransform v
makeCssDeclarationTop v = CssDeclarationTop v
makeCssDeclarationUnicodeBiDi v = CssDeclarationUnicodeBiDi v
makeCssDeclarationVerticalAlign v = CssDeclarationVerticalAlign v
makeCssDeclarationVisibility v = CssDeclarationVisibility v
makeCssDeclarationWhitespace v = CssDeclarationWhitespace v
makeCssDeclarationWidth v = CssDeclarationWidth v
makeCssDeclarationWordSpacing v = CssDeclarationWordSpacing v
makeCssDeclarationZIndex v = CssDeclarationZIndex v
makeCssDeclarationXLink v = CssDeclarationXLink v
makeCssDeclarationXColSpan v = CssDeclarationXColSpan v
makeCssDeclarationXRowSpan v = CssDeclarationXRowSpan v
makeCssDeclarationXLang v = CssDeclarationXLang v
makeCssDeclarationXImg v = CssDeclarationXImg v
makeCssDeclarationXTooltip v = CssDeclarationXTooltip v
makeCssDeclaration_LAST _ = CssDeclaration_LAST


