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

This file is derived from dillo-3.0.5/src/cssparser.cc.
Copyright assignments from that file:
Copyright 2004 Sebastian Geerken <sgeerken@dillo.org>
Copyright 2008-2009 Johannes Hofmann <Johannes.Hofmann@gmx.de>
Additional note in cssparser.cc:
"This file is heavily based on the CSS parser of dillo-0.8.0-css-3 -
a dillo1 based CSS prototype written by Sebastian Geerken."
-}




{-# LANGUAGE OverloadedStrings #-}




{-
Code related to CSS rule.
-}




module Hello.Css.Rule
  (
    CssRule (..)
  , defaultCssRule

  , CssRule2 (..)
  , CssParsedStyleRule (..)

  , getTopCompound
  )
where



import qualified Data.Text as T
-- import Debug.Trace

import Hello.Chain
import Hello.Css.Declaration
import Hello.Css.Selector
import Hello.Css.Tokenizer




data CssRule2
  = CssStyleRule CssRule Bool
  | CssMediaRule [CssToken]
  | CssImportRule [CssToken]
  | CssInvalidRule T.Text
  deriving (Eq, Show)





data CssRule = CssRule {
    complexSelector :: CssComplexSelector
  , declarationSet  :: CssDeclarationSet
  , specificity     :: Int
  , position        :: Int
  } deriving (Eq)

defaultCssRule = CssRule
  { complexSelector = mkComplexSelector [WrapCompound defaultCssCompoundSelector { selectorTagName = CssTypeSelectorUnknown }]
  , declarationSet  = defaultCssDeclarationSet
  , specificity     = -1
  , position        = -1
  }


instance Show CssRule where
  show (CssRule cs ds s p) = "Rule { " ++
                             "complexSelector = " ++ show cs ++ "\n" ++
                             "declSet = "         ++ show ds ++ "\n" ++
                             "spec = " ++ show s  ++ "\n" ++
                             "pos = "  ++ show p  ++ "}\n"




-- Get top compound selector
getTopCompound :: CssRule -> CssCompoundSelector
getTopCompound rule = chainGetFirstDatum . complexSelector $ rule




-- A helper data type
--
-- https://www.w3.org/TR/css-syntax-3/#style-rules
--
-- "A style rule is a qualified rule that associates a selector list with a
-- list of property declarations and possibly a list of nested rules."
data CssParsedStyleRule = CssParsedStyleRule
  { -- "The prelude of the qualified rule is parsed as a <selector-list>. If
    -- this returns failure, the entire style rule is invalid."
    prelude :: [CssParsedComplexSelector]

    -- "The content of the qualified rule’s block is parsed as a style
    -- block’s contents."
  , content :: CssDeclarationSets
  } deriving (Show, Eq)



