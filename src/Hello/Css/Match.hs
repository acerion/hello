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

This file is derived from dillo-3.0.5/src/css.cc.
Copyright assignments from css.cc file:
Copyright 2008-2014 Johannes Hofmann <Johannes.Hofmann@gmx.de>
-}




module Hello.Css.Match
  (
    cssComplexSelectorMatches
  )
  where




import Data.Maybe
import qualified Data.Text as T
import Debug.Trace

import Hello.Css.StyleSheet
import Hello.Css.Selector
import Hello.Css.DoctreeNode
import Hello.Utils




cssComplexSelectorMatches :: CssComplexSelector -> Maybe DoctreeNode -> Doctree -> CssMatchCache -> Int -> (Bool, CssMatchCache)
cssComplexSelectorMatches (Datum compound)                       mDtn tree mc cacheOffset = (compoundSelectorMatches compound (fromJust mDtn), mc)
cssComplexSelectorMatches (Link (Datum compound) combinator rem) mDtn tree mc cacheOffset =
  if not $ compoundSelectorMatches compound (fromJust mDtn)
  then (False, mc)
  else cssComplexSelectorMatches' rem combinator mDtn tree mc cacheOffset




cssComplexSelectorMatches' :: CssComplexSelector -> CssCombinator -> Maybe DoctreeNode -> Doctree -> CssMatchCache -> Int -> (Bool, CssMatchCache)
cssComplexSelectorMatches' complex combinator mDtn tree mc cacheOffset =
  case combinator of
    CssCombinatorDescendant      -> onCombinatorDescendant    complex (getDtnParent tree mDtn)  tree mc cacheOffset
    CssCombinatorChild           -> onCombinatorNonDescendant complex (getDtnParent tree mDtn)  tree mc cacheOffset
    CssCombinatorAdjacentSibling -> onCombinatorNonDescendant complex (getDtnSibling tree mDtn) tree mc cacheOffset




onCombinatorNonDescendant :: CssComplexSelector -> Maybe DoctreeNode -> Doctree -> CssMatchCache -> Int -> (Bool, CssMatchCache)
onCombinatorNonDescendant _                                      Nothing    _    mc _           = (False, mc)
onCombinatorNonDescendant (Link (Datum compound) combinator rem) (Just dtn) tree mc cacheOffset =
  if compoundSelectorMatches compound dtn
  then cssComplexSelectorMatches' rem combinator (Just dtn) tree mc cacheOffset
  else (False, mc)
onCombinatorNonDescendant (Datum compound) (Just dtn) tree mc cacheOffset =
  (compoundSelectorMatches compound dtn, mc)




onCombinatorDescendantLoop :: CssComplexSelector -> Maybe DoctreeNode -> Doctree -> CssMatchCache -> Int -> Int -> (Bool, CssMatchCache)
onCombinatorDescendantLoop _                                               Nothing   _    mc _               _           = (False, mc)
onCombinatorDescendantLoop complex@(Link (Datum compound) combinator rem) (Just dtn) tree mc matchCacheEntry cacheOffset =
  if uniqueNum dtn > matchCacheEntry
  then if thisCompoundMatches && followingCompoundsMatch
       then (True, mc2)
       else onCombinatorDescendantLoop complex parentDtn tree mc2 matchCacheEntry cacheOffset
  else (False, mc)

  where
    parentDtn = getDtnParent tree (Just dtn)
    thisCompoundMatches = compoundSelectorMatches compound dtn
    (followingCompoundsMatch, mc2) = cssComplexSelectorMatches' rem combinator (Just dtn) tree mc cacheOffset

onCombinatorDescendantLoop (Datum compound) (Just dtn) tree mc matchCacheEntry cacheOffset =
  if uniqueNum dtn > matchCacheEntry
  then if compoundSelectorMatches compound dtn
       then (True, mc)
       else onCombinatorDescendantLoop (Datum compound) parentDtn tree mc matchCacheEntry cacheOffset
  else (False, mc)

  where
    parentDtn = getDtnParent tree (Just dtn)




-- I'm not totally sure that we need to patterm match on the first arg.
onCombinatorDescendant :: CssComplexSelector -> Maybe DoctreeNode -> Doctree -> CssMatchCache -> Int -> (Bool, CssMatchCache)
onCombinatorDescendant complex@(Link (Datum cpd) cmb rem) mDtn tree mc cacheOffset =
  if wholeComplexMatches
  then (True, mc2)
  else case mDtn of
         Nothing  -> (False, mc2)
         Just dtn -> (False, updateMatchCache mc2 dtn elemIdx)
  where
    compoundIdx = (chainLength complex) - 1
    elemIdx = cacheOffset + compoundIdx
    matchCacheEntry = mc !! elemIdx
    (wholeComplexMatches, mc2) = onCombinatorDescendantLoop complex mDtn tree mc matchCacheEntry cacheOffset

onCombinatorDescendant (Datum cpd) mDtn tree mc cacheOffset =
  if wholeComplexMatches
  then (True, mc2)
  else case mDtn of
         Nothing  -> (False, mc2)
         Just dtn -> (False, updateMatchCache mc2 dtn elemIdx)
  where
    compoundIdx = (chainLength (Datum cpd)) - 1
    elemIdx = cacheOffset + compoundIdx
    matchCacheEntry = mc !! elemIdx
    (wholeComplexMatches, mc2) = onCombinatorDescendantLoop (Datum cpd) mDtn tree mc matchCacheEntry cacheOffset




updateMatchCache :: CssMatchCache -> DoctreeNode -> Int -> CssMatchCache
updateMatchCache mc dtn elemIdx = listReplaceElem mc (uniqueNum dtn) elemIdx




{-
Return whether compound selector matches at a given node of the document tree.

Right now this is a naive re-write of simple_selector_matches() C function.

TODO: in C++ code the string comparisons were case-insensitive.
-}

compoundSelectorMatches :: CssCompoundSelector -> DoctreeNode -> Bool
compoundSelectorMatches compound dtn = ((compoundSelectorMatches' compound dtn) == 0)




compoundSelectorMatches' :: CssCompoundSelector -> DoctreeNode -> Int
compoundSelectorMatches' compound dtn | mismatchOnElement compound dtn     = 4
                                      | mismatchOnPseudoClass compound dtn = 3
                                      | mismatchOnId compound dtn          = 2
                                      | mismatchOnClass compound dtn       = 1
                                      | otherwise                          = 0
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




