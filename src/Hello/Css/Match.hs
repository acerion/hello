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




cssComplexSelectorMatches :: CssCachedComplexSelector -> Maybe DoctreeNode -> Doctree -> Int -> CssCombinator -> CssMatchCache -> (Bool, CssMatchCache)
cssComplexSelectorMatches _ _ _                   (-1)        _          mc  = (True, mc)
cssComplexSelectorMatches cachedComplex mDtn tree compoundIdx combinator mc =
  case combinator of
    CssCombinatorNone            -> onCombinatorNonDescendant cachedComplex mDtn                      tree compoundIdx mc
    CssCombinatorDescendant      -> onCombinatorDescendant    cachedComplex (getDtnParent tree mDtn)  tree compoundIdx mc
    CssCombinatorChild           -> onCombinatorNonDescendant cachedComplex (getDtnParent tree mDtn)  tree compoundIdx mc
    CssCombinatorAdjacentSibling -> onCombinatorNonDescendant cachedComplex (getDtnSibling tree mDtn) tree compoundIdx mc




onCombinatorNonDescendant :: CssCachedComplexSelector -> Maybe DoctreeNode -> Doctree -> Int -> CssMatchCache -> (Bool, CssMatchCache)
onCombinatorNonDescendant cachedComplex mDtn tree compoundIdx mc =
    case mDtn of
      Nothing  -> (False, mc)
      Just dtn -> if compoundSelectorMatches compound dtn
                  then cssComplexSelectorMatches cachedComplex mDtn tree (compoundIdx - 1) comb mc
                  else (False, mc)
        where
          compound = takeNthCompound (chain cachedComplex) compoundIdx

          links = chainToLinks (chain cachedComplex) []
          link = links !! compoundIdx
          comb = combinator link




onCombinatorDescendantLoop :: CssCachedComplexSelector -> Maybe DoctreeNode -> Doctree -> Int -> CssMatchCache -> Int -> (Bool, CssMatchCache)
onCombinatorDescendantLoop cachedComplex mDtn tree compoundIdx mc matchCacheEntry =
  if isJust mDtn && (uniqueNum . fromJust $ mDtn) > matchCacheEntry
  then case wholeComplexMatchesTuple of
         (True, mc2) -> (True, mc2)
         (False, mc2) -> onCombinatorDescendantLoop cachedComplex (getDtnParent tree mDtn) tree compoundIdx mc2 matchCacheEntry
  else (False, mc)

  where
    links = chainToLinks (chain cachedComplex) []
    link = links !! compoundIdx
    comb = combinator link
    comp = compound link

    thisCompoundMatches = compoundSelectorMatches comp dtn

    (followingCompoundsMatch, mc2) = cssComplexSelectorMatches cachedComplex mDtn tree (compoundIdx - 1) comb mc

    wholeComplexMatchesTuple = ((thisCompoundMatches && followingCompoundsMatch), mc2)

    dtn = fromJust mDtn




onCombinatorDescendant :: CssCachedComplexSelector -> Maybe DoctreeNode -> Doctree -> Int -> CssMatchCache -> (Bool, CssMatchCache)
onCombinatorDescendant cachedComplex mDtn tree compoundIdx mc =
  case wholeComplexMatchesTuple of
    (True, mc2)  -> (True, mc2)
    (False, mc2) -> case mDtn of
                      Nothing  -> (False, mc2)
                      Just dtn -> (False, updateMatchCache mc2 dtn elemIdx)
  where
    elemIdx = matchCacheOffset cachedComplex + compoundIdx
    match_cache_entry = mc !! elemIdx
    wholeComplexMatchesTuple = onCombinatorDescendantLoop cachedComplex mDtn tree compoundIdx mc match_cache_entry




updateMatchCache :: CssMatchCache -> DoctreeNode -> Int -> CssMatchCache
updateMatchCache mc dtn elemIdx = listReplaceElem mc (uniqueNum dtn) elemIdx




takeNthCompound :: CssComplexSelector -> Int -> CssCompoundSelector
takeNthCompound complex idx = compound link
  where
    links = chainToLinks complex []
    link = links !! idx




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




