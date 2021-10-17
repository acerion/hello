{-
Copyright (C) 2021 Kamil Ignacak acerion@wp.pl

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

This file is derived from dillo-3.0.5/src/css.cc (and doctree.hh).
Copyright assignments from css.cc file:
Copyright 2008-2014 Johannes Hofmann <Johannes.Hofmann@gmx.de>
-}


module Css
  (
    selectorSpecificity
  )
  where




import qualified Data.Text as T
import qualified Data.Map as M
import Data.Bits
import Debug.Trace
import Hello.Css.Parser
import Hello.Css.Selector
import Hello.Css.DoctreeNode
import Hello.Ffi.Css.DoctreeNode





{-
Return the specificity of the selector.

The specificity of a CSS selector is defined in
http://www.w3.org/TR/CSS21/cascade.html#specificity
-}
selectorSpecificity :: CssComplexSelector -> Int
selectorSpecificity complex = selectorSpecificity' complex 0
  where
    selectorSpecificity' :: CssComplexSelector -> Int -> Int
    selectorSpecificity' (Link (Datum c1) combinator remainder) acc = selectorSpecificity' remainder (acc + (compoundSelectorSpecificity c1))
    selectorSpecificity' (Datum c1)                             acc =                                 acc + (compoundSelectorSpecificity c1)




-- Return the specificity of compound selector
compoundSelectorSpecificity :: CssCompoundSelector -> Int
compoundSelectorSpecificity compound = (fromId compound) + (fromClass compound) + (fromPseudoClass compound) + (fromElement compound)
  where
    fromId compound          = if (not . T.null . selectorId $ compound) then (1 `shiftL` 20) else 0
    fromClass compound       = (length . selectorClass $ compound) `shiftL` 10
    fromPseudoClass compound = if (not . null . selectorPseudoClass $ compound) then (1 `shiftL` 10) else 0 -- Remember that C/C++ code can use only first pseudo code.
    fromElement compound     = if compoundHasUniversalType compound then 0 else 1




