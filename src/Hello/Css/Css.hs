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
          , simpleSelectorMatches'
          , selectorSpecificity
          )
  where




import qualified Data.Text as T
import qualified Data.Map as M
import Data.Bits
import Debug.Trace
import Hello.Css.Parser




data DoctreeNode = DoctreeNode {
    uniqueNum      :: Int -- unique ascending id
  , htmlElementIdx :: Int -- Index to html.cc::Tags

  , selPseudoClass  :: T.Text
  , selId           :: T.Text
  , selClass        :: [T.Text]

  , parent    :: Maybe DoctreeNode
  , sibling   :: Maybe DoctreeNode
  , lastChild :: Maybe DoctreeNode
  } deriving (Show)




{-
Return whether simple selector matches at a given node of the document tree.

Right now this is a naive re-write of simple_selector_matches() C function.

TODO: in C++ code the string comparisons were case-insensitive.
-}

simpleSelectorMatches a b = (simpleSelectorMatches' a b) == 0

simpleSelectorMatches' :: CssSimpleSelector -> DoctreeNode -> Int
simpleSelectorMatches' simSel dtn | mismatchOnElement simSel dtn     = 4
                                  | mismatchOnPseudoClass comSel dtn = 3
                                  | mismatchOnId comSel dtn          = 2
                                  | mismatchOnClass comSel dtn       = 1
                                  | otherwise                        = 0
  where
    mismatchOnElement :: CssSimpleSelector -> DoctreeNode -> Bool
    mismatchOnElement simSel dtn = (selectorTagName simSel) /= Just CssTypeSelectorUniv && (unCssTypeSelector . selectorTagName $ simSel) /= (htmlElementIdx dtn)
    -- if (selector->c_selector_element != CssSimpleSelectorElementAny && selector->c_selector_element != dtn->c_html_element_idx)
    --     return false;

    -- C/C++ code can use only first pseudo class
    mismatchOnPseudoClass :: CssCompoundSelector -> DoctreeNode -> Bool
    mismatchOnPseudoClass comSel dtn = (length . selectorPseudoClass2 $ comSel) > 0
                                    && ((T.null . selPseudoClass $ dtn) || ((head . selectorPseudoClass2 $ comSel) /= (CssPseudoClassSelector . selPseudoClass $ dtn)))
    -- if (selector->c_selector_pseudo_class_size > 0 &&
    --     (dtn->c_element_selector_pseudo_class == NULL || dStrAsciiCasecmp (selector->c_selector_pseudo_class[0], dtn->c_element_selector_pseudo_class) != 0))
    --     return false;

    mismatchOnId :: CssCompoundSelector -> DoctreeNode -> Bool
    mismatchOnId comSel dtn = (not . null . selectorId2 $ comSel) && ((T.null . selId $ dtn) || (selectorId2 comSel /= [CssIdSelector . selId $ dtn]))
    -- if (selector->c_selector_id != NULL && (dtn->c_element_selector_id == NULL || dStrAsciiCasecmp (selector->c_selector_id, dtn->c_element_selector_id) != 0))
    --     return false;

    -- All class items of a simple selector must be found in dtn's class set
    mismatchOnClass :: CssCompoundSelector -> DoctreeNode -> Bool
    mismatchOnClass comSel dtn = not allSimSelClassInNodeClass
      where
        allSimSelClassInNodeClass = and $ map (\x -> elem x classes) (selectorClass2 comSel)
        classes = map CssClassSelector (selClass $ dtn)
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

    comSel = toCompound simSel



{-
Return the specificity of the selector.

The specificity of a CSS selector is defined in
http://www.w3.org/TR/CSS21/cascade.html#specificity
-}
selectorSpecificity :: CssSelector -> Int
selectorSpecificity sel = selectorSpecificity' (simpleSelectors sel) 0
  where
    selectorSpecificity' (x:xs) acc = selectorSpecificity' xs (acc + (compoundSelectorSpecificity . toCompound $ x))
    selectorSpecificity' []     acc = acc




-- Return the specificity of compound selector
compoundSelectorSpecificity :: CssCompoundSelector -> Int
compoundSelectorSpecificity csel = (fromId csel) + (fromClass csel) + (fromPseudoClass csel) + (fromElement csel)
  where
    fromId csel          = if (not . null . selectorId2 $ csel) then (1 `shiftL` 20) else 0
    fromClass csel       = (length . selectorClass2 $ csel) `shiftL` 10
    fromPseudoClass csel = if (not . null . selectorPseudoClass2 $ csel) then (1 `shiftL` 10) else 0 -- Remember that C/C++ code can use only first pseudo code.

    fromElement (CssCompoundSelector (CssTypeSelectorUniv, _)) = 0
    fromElement (CssCompoundSelector (_, _))                   = 1


