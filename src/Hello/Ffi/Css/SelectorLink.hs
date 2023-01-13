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




{- Code for CssComplexSelectorLink helper type, used only in FFI code as an
intermediate between c_css_complex_selector_link_t C structure and
CssComplexSelector Haskell type. -}




{-# LANGUAGE ScopedTypeVariables #-}




module Hello.Ffi.Css.SelectorLink
  (
    CssComplexSelectorLink (..)
  , defaultComplexSelectorLink
  , linksToChain
  , chainToLinks
  )
where




import Hello.Chain
import Hello.Css.Selector




data CssComplexSelectorLink = CssComplexSelectorLink
  { compound   :: CssCompoundSelector
  , combinator :: Maybe CssCombinator
  } deriving (Show, Eq)




defaultComplexSelectorLink :: CssComplexSelectorLink
defaultComplexSelectorLink = CssComplexSelectorLink
  { compound   = defaultCssCompoundSelector

  -- Combinator that combines this compound selector and the previous one
  -- (previous one == compound selector to the left of current compound
  -- selector). For a compound selector that is first on the list (or the only
  -- on the list), the combinator will be None.
  , combinator = Nothing
  }




chainToLinks :: CssComplexSelector -> [CssComplexSelectorLink] -> [CssComplexSelectorLink]
chainToLinks (Chain compo combi remainder) acc = chainToLinks remainder (defaultComplexSelectorLink { compound = compo, combinator = Just combi } : acc)
chainToLinks (Last compo) acc                  = defaultComplexSelectorLink { compound = compo, combinator = Nothing } : acc




linksToChain :: [CssComplexSelectorLink] -> CssComplexSelector
linksToChain = linksToChain' . reverse




linksToChain' :: [CssComplexSelectorLink] -> CssComplexSelector
linksToChain' (x:xs) = case combinator x of
                         Nothing    -> Last . compound $ x -- no combinator
                         Just combi -> Chain (compound x) combi (linksToChain' xs)
linksToChain' []     = Last defaultCssCompoundSelector






