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




module Hello.Css.Declaration
  (
    CssDeclaration (..)
  )
where




import Debug.Trace




-- A property in css declaration, but with a value.
data CssDeclaration
  = CssDeclarationBackgroundAttachment         -- 0
  | CssDeclarationBackgroundColor              -- 1
  | CssDeclarationBackgroundImage              -- 2
  | CssDeclarationBackgroundPosition           -- 3
  | CssDeclarationBackgroundRepeat             -- 4
  | CssDeclarationBorderBottomColor            -- 5
  | CssDeclarationBorderBottomStyle            -- 6
  | CssDeclarationBorderBottomWidth            -- 7
  | CssDeclarationBorderCollapse               -- 8
  | CssDeclarationBorderLeftColor              -- 9
  | CssDeclarationBorderLeftStyle              -- 10
  | CssDeclarationBorderLeftWidth              -- 11
  | CssDeclarationBorderRightColor             -- 12
  | CssDeclarationBorderRightStyle             -- 13
  | CssDeclarationBorderRightWidth             -- 14
  | CssDeclarationBorderSpacing                -- 15
  | CssDeclarationBorderTopColor               -- 16
  | CssDeclarationBorderTopStyle               -- 17
  | CssDeclarationBorderTopWidth               -- 18
  | CssDeclarationBottom                       -- 19
  | CssDeclarationCaptionSide                  -- 20
  | CssDeclarationClear                        -- 21
  | CssDeclarationClip                         -- 22
  | CssDeclarationColor                        -- 23
  | CssDeclarationContent                      -- 24
  | CssDeclarationCounterIncrement             -- 25
  | CssDeclarationCounterReset                 -- 26
  | CssDeclarationCursor                       -- 27
  | CssDeclarationDirection                    -- 28
  | CssDeclarationDisplay                      -- 29
  | CssDeclarationEmptyCells                   -- 30
  | CssDeclarationFloat                        -- 31
  | CssDeclarationFontFamily                   -- 32
  | CssDeclarationFontSize                     -- 33
  | CssDeclarationFontSizeAdjust               -- 34
  | CssDeclarationFontStretch                  -- 35
  | CssDeclarationFontStyle                    -- 36
  | CssDeclarationFontVariant                  -- 37
  | CssDeclarationFontWeight                   -- 38
  | CssDeclarationHeight                       -- 39
  | CssDeclarationLeft                         -- 40
  | CssDeclarationLetterSpacing                -- 41
  | CssDeclarationLineHeight                   -- 42
  | CssDeclarationListStyleImage               -- 43
  | CssDeclarationListStylePosition            -- 44
  | CssDeclarationListStyleType                -- 45
  | CssDeclarationMarginBottom                 -- 46
  | CssDeclarationMarginLeft                   -- 47
  | CssDeclarationMarginRight                  -- 48
  | CssDeclarationMarginTop                    -- 49
  | CssDeclarationMarkerOffset                 -- 50
  | CssDeclarationMarks                        -- 51
  | CssDeclarationMaxHeight                    -- 52
  | CssDeclarationMaxWidth                     -- 53
  | CssDeclarationMinHeight                    -- 54
  | CssDeclarationMinWidth                     -- 55
  | CssDeclarationOutlineColor                 -- 56
  | CssDeclarationOutlineStyle                 -- 57
  | CssDeclarationOutlineWidth                 -- 58
  | CssDeclarationOverflow                     -- 59
  | CssDeclarationPaddingBottom                -- 60
  | CssDeclarationPaddingLeft                  -- 61
  | CssDeclarationPaddingRight                 -- 62
  | CssDeclarationPaddingTop                   -- 63
  | CssDeclarationPosition                     -- 64
  | CssDeclarationQuotes                       -- 65
  | CssDeclarationRight                        -- 66
  | CssDeclarationTextAlign                    -- 67
  | CssDeclarationTextDecoration               -- 68
  | CssDeclarationTextIndent                   -- 69
  | CssDeclarationTextShadow                   -- 70
  | CssDeclarationTextTransform                -- 71
  | CssDeclarationTop                          -- 72
  | CssDeclarationUnicodeBiDi                  -- 73
  | CssDeclarationVerticalAlign                -- 74
  | CssDeclarationVisibility                   -- 75
  | CssDeclarationWhitespace                   -- 76
  | CssDeclarationWidth                        -- 77
  | CssDeclarationWordSpacing                  -- 78
  | CssDeclarationZIndex                       -- 79

  -- Pseudo-property used internally by dillo/hello. Without it following
  -- a/href links won't work.
  | CssDeclarationXLink                        -- 80

  | CssDeclarationXColSpan                     -- 81
  | CssDeclarationXRowSpan                     -- 82

  -- Pseudo-property for "lang" or "xml:lang" attribute of html element.
  | CssDeclarationXLang                        -- 83

  -- Pseudo-property used (probably) to index images in a html document.
  | CssDeclarationXImg                         -- 84
  | CssDeclarationXTooltip                     -- 85

  | CssDeclaration_LAST                        -- 86
  deriving (Eq, Show)

