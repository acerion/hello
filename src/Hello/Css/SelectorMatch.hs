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

This file is derived from dillo-3.0.5/src/css.cc.
Copyright assignments from css.cc file:
Copyright 2008-2014 Johannes Hofmann <Johannes.Hofmann@gmx.de>
-}




{-
Code in this file is implementing matching of CSS complex selector against
doctree elements.

References:

-}




{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}




module Hello.Css.SelectorMatch
  (
    complexSelectorMatches

    -- Only for tests.
  , compoundSelectorMatches'
  , CssCompoundSelectorMatch (..)
  )
  where




import qualified Data.Text as T
--import Debug.Trace

import Hello.Chain
import Hello.Css.MatchCache
import Hello.Css.Selector
import Hello.Html.Doctree
import Hello.Html.DoctreeNode




complexSelectorMatches :: CssCachedComplexSelector -> Doctree -> Maybe DoctreeNode -> CssMatchCache -> (Bool, CssMatchCache)
complexSelectorMatches cachedComplexSelector doctree mDtn matchCache = (isMatch, matchCache2)
  where
    (isMatch, matchCache2) = complexSelectorMatches' (chain cachedComplexSelector) mDtn doctree matchCache cacheOffset
    cacheOffset            = matchCacheOffset cachedComplexSelector




complexSelectorMatches' :: CssComplexSelector -> Maybe DoctreeNode -> Doctree -> CssMatchCache -> Int -> (Bool, CssMatchCache)
complexSelectorMatches' _                                     Nothing    _       mc _           = (False, mc)
complexSelectorMatches' (Last compound)                       (Just dtn) doctree mc cacheOffset = (compoundSelectorMatches compound dtn, mc)
complexSelectorMatches' (Chain compound combinator remainder) (Just dtn) doctree mc cacheOffset =
  if compoundSelectorMatches compound dtn
  then matchCombinatorAndRemainder combinator remainder dtn doctree mc cacheOffset
  else (False, mc)




-- Test whether a pair of <combinator> + <remainder of complex selector>
-- matches a doctree.
matchCombinatorAndRemainder :: CssCombinator -> CssComplexSelector -> DoctreeNode -> Doctree -> CssMatchCache -> Int -> (Bool, CssMatchCache)
matchCombinatorAndRemainder CssCombinatorDescendant      complex dtn doctree mc cacheOffset = matchDescendant         complex (getDtnParent doctree dtn)  doctree mc cacheOffset
matchCombinatorAndRemainder CssCombinatorChild           complex dtn doctree mc cacheOffset = complexSelectorMatches' complex (getDtnParent doctree dtn)  doctree mc cacheOffset
matchCombinatorAndRemainder CssCombinatorAdjacentSibling complex dtn doctree mc cacheOffset = complexSelectorMatches' complex (getDtnSibling doctree dtn) doctree mc cacheOffset




-- Go upwards of DocTree looking for a matching parent (because of Descendant
-- combinator), and then try to match remainder of Complex Selector.
findMatchingParentAndFollowers  :: CssComplexSelector -> Maybe DoctreeNode -> Doctree -> CssMatchCache -> Int -> Int -> (Bool, CssMatchCache)
findMatchingParentAndFollowers _       Nothing    _       mc _                 _           = (False, mc)
findMatchingParentAndFollowers complex (Just dtn) doctree mc dtnNumForCompound cacheOffset =
  if uniqueNum dtn > dtnNumForCompound
  then case complexSelectorMatches' complex (Just dtn) doctree mc cacheOffset of
         -- This dtn node matched innermost Compound of Complex, and the rest
         -- of tree matched remainder of Complex.
         (True, mc2)  -> (True, mc2)
         -- Go up the tree searching for another candidate node that would
         -- match the innermost Compound of Complex (and the rest of tree
         -- would also match the remainder of Complex).
         (False, mc2) -> findMatchingParentAndFollowers complex parentDtn doctree mc2 dtnNumForCompound cacheOffset
  else (False, mc)

  where
    parentDtn = getDtnParent doctree dtn




-- Try to match inntermost Compound of Complex agains given node (which is a
-- Parent of some other node). If this fails, try to match agains parent, and
-- grandparent, until you find a match. On success, try to match remainder of
-- Complex against remainder of tree.
matchDescendant :: CssComplexSelector -> Maybe DoctreeNode -> Doctree -> CssMatchCache -> Int -> (Bool, CssMatchCache)
matchDescendant complex mDtn doctree mc cacheOffset =
  case findMatchingParentAndFollowers complex mDtn doctree mc dtnNumForCompound cacheOffset of
    (True, mc2)  -> (True, mc2)
    (False, mc2) -> case mDtn of
                      Nothing  -> (False, mc2)
                      Just dtn -> (False, matchCacheSetItem mc2 (uniqueNum dtn) compoundOffset)
  where
    compoundIdx       = (chainDatumLength complex) - 1
    compoundOffset    = cacheOffset + compoundIdx
    dtnNumForCompound = matchCacheGetItem mc compoundOffset





{-
Return whether compound selector matches at a given node of the document tree.

Right now this is a naive re-write of simple_selector_matches() C function.

TODO: in C++ code the string comparisons were case-insensitive.
-}

compoundSelectorMatches :: CssCompoundSelector -> DoctreeNode -> Bool
compoundSelectorMatches compound dtn = ((compoundSelectorMatches' compound dtn) == CssCompoundSelectorMatch)




data CssCompoundSelectorMatch
  = CssCompoundSelectorMatch
  | CssCompoundSelectorMismatchElement
  | CssCompoundSelectorMismatchPseudoClass
  | CssCompoundSelectorMismatchId
  | CssCompoundSelectorMismatchClass
  deriving (Eq)




compoundSelectorMatches' :: CssCompoundSelector -> DoctreeNode -> CssCompoundSelectorMatch
compoundSelectorMatches' compound dtnArg | mismatchOnElement compound dtnArg     = CssCompoundSelectorMismatchElement
                                         | mismatchOnPseudoClass compound dtnArg = CssCompoundSelectorMismatchPseudoClass
                                         | mismatchOnId compound dtnArg          = CssCompoundSelectorMismatchId
                                         | mismatchOnClass compound dtnArg       = CssCompoundSelectorMismatchClass
                                         | otherwise                             = CssCompoundSelectorMatch
  where
    mismatchOnElement :: CssCompoundSelector -> DoctreeNode -> Bool
    mismatchOnElement csel dtn = (compoundTagName csel) /= CssTypeSelectorUniv && (unCssTypeSelector . compoundTagName $ csel) /= (htmlElementIdx dtn)
    -- if (selector->c_selector_element != CssSimpleSelectorElementAny && selector->c_selector_element != dtn->c_html_element_idx)
    --     return false;

    -- C/C++ code can use only first pseudo class
    mismatchOnPseudoClass :: CssCompoundSelector -> DoctreeNode -> Bool
    mismatchOnPseudoClass csel dtn = (length . compoundPseudoClass $ csel) > 0
                                     && ((T.null . selPseudoClass $ dtn) || ((head . compoundPseudoClass $ csel) /= (CssPseudoClassSelector . selPseudoClass $ dtn)))
    -- if (selector->c_selector_pseudo_class_size > 0 &&
    --     (dtn->c_element_selector_pseudo_class == NULL || dStrAsciiCasecmp (selector->c_selector_pseudo_class[0], dtn->c_element_selector_pseudo_class) != 0))
    --     return false;

    mismatchOnId :: CssCompoundSelector -> DoctreeNode -> Bool
    mismatchOnId csel dtn = (not . null . compoundId $ csel) && ((T.null . selId $ dtn) || ((compoundId $ csel) /= [CssIdSelector . selId $ dtn]))
    -- if (selector->c_selector_id != NULL && (dtn->c_element_selector_id == NULL || dStrAsciiCasecmp (selector->c_selector_id, dtn->c_element_selector_id) != 0))
    --     return false;

    -- All class items of a compound selector must be found in dtn's class set
    mismatchOnClass :: CssCompoundSelector -> DoctreeNode -> Bool
    mismatchOnClass csel dtn = not allCompoundClassInNodeClass
      where
        allCompoundClassInNodeClass = and $ map (\x -> elem x classes) (compoundClass $ csel)
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




