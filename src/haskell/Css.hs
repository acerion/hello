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


module Css( DoctreeNode (..)
          , simpleSelectorMatches
          , selectorSpecificity
          )
  where




import qualified Data.Text as T
import qualified Data.Map as M
import Data.Bits
import Debug.Trace
import CssParser




data DoctreeNode = DoctreeNode {
    uniqueNum      :: Int -- unique ascending id
  , htmlElementIdx :: Int -- Index to html.cc::Tags

  , selPseudoClass  :: T.Text
  , selId           :: T.Text
  , selClass        :: [T.Text]

  , parent    :: DoctreeNode
  , sibling   :: DoctreeNode
  , lastChild :: DoctreeNode
  } deriving (Show)




{-
Return whether simple selector matches at a given node of the document tree.

Right now this is a naive re-write of simple_selector_matches() C function.

TODO: in C++ code the string comparisons were case-insensitive.
-}
simpleSelectorMatches :: CssSimpleSelector -> DoctreeNode -> Bool
simpleSelectorMatches simSel dtn | mismatchOnElement simSel dtn     = False
                                 | mismatchOnPseudoClass simSel dtn = False
                                 | mismatchOnId simSel dtn          = False
                                 | mismatchOnClass simSel dtn       = False
                                 | otherwise                        = True
  where
    mismatchOnElement :: CssSimpleSelector -> DoctreeNode -> Bool
    mismatchOnElement simSel dtn = (selectorElement simSel) /= cssSimpleSelectorElementAny && (selectorElement simSel) /= (htmlElementIdx dtn)
    -- if (selector->c_selector_element != CssSimpleSelectorElementAny && selector->c_selector_element != dtn->c_html_element_idx)
    --     return false;

    -- C/C++ code can use only first pseudo class
    mismatchOnPseudoClass :: CssSimpleSelector -> DoctreeNode -> Bool
    mismatchOnPseudoClass simSel dtn = (length . selectorPseudoClass $ simSel) > 0
                                    && ((T.null . selPseudoClass $ dtn) || ((head . selectorPseudoClass $ simSel) /= selPseudoClass dtn))
    -- if (selector->c_selector_pseudo_class_size > 0 &&
    --     (dtn->c_element_selector_pseudo_class == NULL || dStrAsciiCasecmp (selector->c_selector_pseudo_class[0], dtn->c_element_selector_pseudo_class) != 0))
    --     return false;

    mismatchOnId :: CssSimpleSelector -> DoctreeNode -> Bool
    mismatchOnId simSel dtn = (not . T.null . selectorId $ simSel) && ((T.null . selId $ dtn) || (selectorId simSel /= selId dtn))
    -- if (selector->c_selector_id != NULL && (dtn->c_element_selector_id == NULL || dStrAsciiCasecmp (selector->c_selector_id, dtn->c_element_selector_id) != 0))
    --     return false;

    -- All class items of a simple selector must be found in dtn's class set
    mismatchOnClass :: CssSimpleSelector -> DoctreeNode -> Bool
    mismatchOnClass simSel dtn = not allSimSelClassInNodeClass
      where
        allSimSelClassInNodeClass = and $ map (\x -> elem x (selClass dtn)) (selectorClass simSel)
    -- for (int i = 0; i < selector->c_selector_class_size; i++) {
    -- bool found = false;
    -- for (size_t j = 0; j < dtn->c_element_selector_class_size; j++) {
    --     if (dStrAsciiCasecmp (selector->c_selector_class[i], dtn->c_element_selector_class[j]) == 0) {
    --         found = true;
    --         break;
    --     }
    -- }
    -- if (!found) {
    --     return false;
    -- }



{-
Return the specificity of the selector.

The specificity of a CSS selector is defined in
http://www.w3.org/TR/CSS21/cascade.html#specificity
-}
selectorSpecificity :: CssSelector -> Int
selectorSpecificity sel = selectorSpecificity' (simpleSelectorList sel) 0
  where
    selectorSpecificity' (x:xs) acc = selectorSpecificity' xs (acc + (simpleSelectorSpecificity x))
    selectorSpecificity' []     acc = acc




-- Return the specificity of the simple selector
simpleSelectorSpecificity :: CssSimpleSelector -> Int
simpleSelectorSpecificity simSel = fromId + fromClass + fromPseudoClass + fromElement
  where
    fromId          = if (not . T.null . selectorId $ simSel) then (1 `shiftL` 20) else 0
    fromClass       = (length . selectorClass $ simSel) `shiftL` 10
    fromPseudoClass = if (not . null . selectorPseudoClass $ simSel) then (1 `shiftL` 10) else 0 -- Remember that C/C++ code can use only first pseudo code.
    fromElement     = if ((selectorElement simSel) /= cssSimpleSelectorElementAny) then 1 else 0





