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



{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}


module Hello.Css.Match
  (
    cssComplexSelectorMatches2
  , compoundSelectorMatches' -- For tests code

  , applyCssRule
  , cssGetMinSpecIndex
  , applyMatchingRules
  , cssStyleSheetApplyStyleSheet

  , cssContextApplyCssContext

  , minSpecificityForRulesLists
  , CssSpecificityState (..)
  )
  where




import Data.Bits
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import Debug.Trace

import Hello.Css.Parser
import Hello.Css.StyleSheet
import Hello.Css.Selector
import Hello.Html.Doctree
import Hello.Html.DoctreeNode
import Hello.Utils




cssComplexSelectorMatches :: CssComplexSelector -> Maybe DoctreeNode -> DoctreeItems -> CssMatchCache -> Int -> (Bool, CssMatchCache)
cssComplexSelectorMatches _                                      Nothing    _    mc _           = (False, mc)
cssComplexSelectorMatches (Datum compound)                       (Just dtn) tree mc cacheOffset = (compoundSelectorMatches compound dtn, mc)
cssComplexSelectorMatches (Link (Datum compound) combinator rem) (Just dtn) tree mc cacheOffset =
  if compoundSelectorMatches compound dtn
  then matchCombinatorAndRemainder combinator rem dtn tree mc cacheOffset
  else (False, mc)




-- TODO: remove duplication between cssComplexSelectorMatches and cssComplexSelectorMatches2
cssComplexSelectorMatches2 :: CssCachedComplexSelector -> Doctree -> Maybe DoctreeNode -> CssMatchCache -> (Bool, CssMatchCache)
cssComplexSelectorMatches2 cachedComplexSelector doctree mDtn matchCache = (isMatch, matchCache2)
  where
    (isMatch, matchCache2) = cssComplexSelectorMatches (chain cachedComplexSelector) mDtn ns matchCache cacheOffset
    cacheOffset = matchCacheOffset cachedComplexSelector
    ns = nodes doctree



-- Test whether a pair of <combinator> + <remainder of complex selector>
-- matches a doctree.
matchCombinatorAndRemainder :: CssCombinator -> CssComplexSelector -> DoctreeNode -> DoctreeItems -> CssMatchCache -> Int -> (Bool, CssMatchCache)
matchCombinatorAndRemainder combinator complex dtn tree mc cacheOffset =
  case combinator of
    CssCombinatorDescendant      -> matchDescendant    complex (getDtnParent tree dtn)  tree mc cacheOffset
    CssCombinatorChild           -> matchNonDescendant complex (getDtnParent tree dtn)  tree mc cacheOffset
    CssCombinatorAdjacentSibling -> matchNonDescendant complex (getDtnSibling tree dtn) tree mc cacheOffset




-- Try to match inntermost Compound of Complex agains given node (which is
-- either Sibling or Parent of some other node). On success, try to match
-- remainder of Complex against remainder of tree.
matchNonDescendant :: CssComplexSelector -> Maybe DoctreeNode -> DoctreeItems -> CssMatchCache -> Int -> (Bool, CssMatchCache)
matchNonDescendant = cssComplexSelectorMatches




-- Go upwards of DocTree looking for a matching parent (because of Descendant
-- combinator), and then try to match remainder of Complex Selector.
findMatchingDescendantAndFollowers  :: CssComplexSelector -> Maybe DoctreeNode -> DoctreeItems -> CssMatchCache -> Int -> Int -> (Bool, CssMatchCache)
findMatchingDescendantAndFollowers _       Nothing    _    mc _               _           = (False, mc)
findMatchingDescendantAndFollowers complex (Just dtn) tree mc matchCacheEntry cacheOffset =
  if uniqueNum dtn > matchCacheEntry
  then case cssComplexSelectorMatches complex (Just dtn) tree mc cacheOffset of
         -- This dtn node matched innermost Compound of Complex, and the rest
         -- of tree matched remainder of Complex.
         (True, mc2)  -> (True, mc2)
         -- Go up the tree searching for another candidate node that would
         -- match the innermost Compound of Complex (and the rest of tree
         -- would also match the remainder of Complex).
         (False, mc2) -> findMatchingDescendantAndFollowers complex parentDtn tree mc2 matchCacheEntry cacheOffset
  else (False, mc)

  where
    parentDtn = getDtnParent tree dtn




-- Try to match inntermost Compound of Complex agains given node (which is a
-- Parent of some other node). If this fails, try to match agains parent, and
-- grandparent, until you find a match. On success, try to match remainder of
-- Complex against remainder of tree.
matchDescendant :: CssComplexSelector -> Maybe DoctreeNode -> DoctreeItems -> CssMatchCache -> Int -> (Bool, CssMatchCache)
matchDescendant complex mDtn tree mc cacheOffset =
  case findMatchingDescendantAndFollowers complex mDtn tree mc matchCacheEntry cacheOffset of
    (True, mc2)  -> (True, mc2)
    (False, mc2) -> case mDtn of
                      Nothing  -> (False, mc2)
                      Just dtn -> (False, updateMatchCache mc2 dtn elemIdx)
  where
    updateMatchCache :: CssMatchCache -> DoctreeNode -> Int -> CssMatchCache
    updateMatchCache mc dtn elemIdx = listReplaceElem mc (uniqueNum dtn) elemIdx

    compoundIdx = (chainLength complex) - 1
    elemIdx = cacheOffset + compoundIdx
    matchCacheEntry = mc !! elemIdx




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



type CssSpecificityState = (Int, Int, Int)




minSpecificityForRulesLists :: [[CssRule]] -> Int -> [Int] -> CssSpecificityState -> CssSpecificityState
minSpecificityForRulesLists []       listIdx index state = state
minSpecificityForRulesLists (rl:rls) listIdx index state = minSpecificityForRulesLists rls (listIdx + 1) index state2
  where
    state2 = minSpecificityForRulesList rl listIdx index state




minSpecificityForRulesList :: [CssRule] -> Int -> [Int] -> CssSpecificityState -> CssSpecificityState
minSpecificityForRulesList rulesList listIdx index state =
  if length rulesList <= ruleIdx
  then state
  else (minSpecificityForRule rule listIdx state)
  where
    rule = rulesList !! ruleIdx
    ruleIdx = index !! listIdx




minSpecificityForRule :: CssRule -> Int -> CssSpecificityState -> CssSpecificityState
minSpecificityForRule rule listIdx state =
  if ((specificity rule < minSpec) || (specificity rule == minSpec && position rule < minPos))
  then (specificity rule, position rule, listIdx)
  else (minSpec, minPos, minSpecIndex)
  where (minSpec, minPos, minSpecIndex) = state




applyCssRule :: CssDeclarationSet -> CssMatchCache -> Doctree -> Maybe DoctreeNode -> CssRule -> (CssDeclarationSet, CssMatchCache)
applyCssRule targetDeclSet matchCache doctree mDtn rule =
  case match of
    True      -> (targetDeclSet', matchCache2)
    otherwise -> (targetDeclSet,  matchCache2)

  where
    targetDeclSet'       = declarationsSetAppend targetDeclSet (declarationSet rule)
    (match, matchCache2) = cssComplexSelectorMatches2 (complexSelector rule) doctree mDtn matchCache





cssGetMinSpecIndex :: [[CssRule]] -> [Int] -> Int -> IO Int
cssGetMinSpecIndex rulesLists index minSpecIndex = do

  -- TODO: why shifting by 30 and not 31?
  let minSpec :: Int = 1 `shift` 30;
  let minPos  :: Int = 1 `shift` 30;

  let state = minSpecificityForRulesLists rulesLists 0 index (minSpec, minPos, minSpecIndex)

  let minSpec'      = triplet1st state
  let minPos'       = triplet2nd state
  let minSpecIndex' = triplet3rd state

  putStrLn ("minSpec = " ++ (show minSpec') ++ ", minPos = " ++ (show minPos') ++ ", minSpecIndex = " ++ (show minSpecIndex'))

  return minSpecIndex'





applyMatchingRules :: Doctree -> Maybe DoctreeNode -> CssDeclarationSet -> CssMatchCache -> [[CssRule]] -> IO (CssDeclarationSet, CssMatchCache)
applyMatchingRules = applyMatchingRules' index
  where
    index = take 32 $ repeat 0




-- Apply potentially matching rules from rules_lists[0-numLists] with
-- ascending specificity. If specificity is equal, rules are applied in
-- order of appearance. Each rules_list is sorted already.
applyMatchingRules' :: [Int] -> Doctree -> Maybe DoctreeNode -> CssDeclarationSet -> CssMatchCache -> [[CssRule]] -> IO (CssDeclarationSet, CssMatchCache)
applyMatchingRules' index doctree mDtn targetDeclSet matchCache rulesLists = do
  let minSpecIndex = -1
  minSpecIndex' <- cssGetMinSpecIndex rulesLists index minSpecIndex
  if minSpecIndex' >= 0
    then
    do
      let rulesList = rulesLists !! minSpecIndex'
      let idx = index !! minSpecIndex'
      let rule = rulesList !! idx

      let (targetDeclSet', matchCache') = applyCssRule targetDeclSet matchCache doctree mDtn rule

      let oldElem = index !! minSpecIndex'
      let index2 = listReplaceElem index (oldElem + 1) minSpecIndex'

      putStrLn . show $ targetDeclSet'
      putStrLn . show $ V.fromList $ index2

      applyMatchingRules' index2 doctree mDtn targetDeclSet' matchCache' rulesLists
    else return (targetDeclSet, matchCache)




-- Apply a stylesheet to a list of declarations.
--
-- The declarations (list property+value) are set as defined by the rules in
-- the stylesheet that match at the given node in the document tree.
cssStyleSheetApplyStyleSheet :: CssStyleSheet -> CssDeclarationSet -> CssMatchCache -> Doctree -> DoctreeNode -> IO (CssDeclarationSet, CssMatchCache)
cssStyleSheetApplyStyleSheet styleSheet targetDeclSet  matchCache doctree dtn = do
    let rulesLists :: [[CssRule]] = buildRulesListsForDtn styleSheet dtn
    (targetDeclSet', matchCache') <- applyMatchingRules doctree (Just dtn) targetDeclSet matchCache rulesLists
    return (targetDeclSet', matchCache')




buildRulesListsForDtn :: CssStyleSheet -> DoctreeNode -> [[CssRule]]
buildRulesListsForDtn styleSheet dtn = reverse rulesLists
  where
    rulesLists = byAnyElement . byElementId . byClass . bySelId $ []


    {-
    if (dtn->c_element_selector_id) {
       rules_lists[numLists] = hll_rulesMapGetList(style_sheet->c_rules_by_id, dtn->c_element_selector_id);
       if (rules_lists[numLists]) {
          numLists++;
       }
    }
    -}
    bySelId lists = if T.null . selId $ dtn
                    then lists
                    else case M.lookup (selId dtn) (rulesById styleSheet) of
                           Just l    -> l:lists
                           otherwise -> lists


    {-
    for (int i = 0; i < dtn->c_element_selector_class_size; i++) {
       if (i >= maxLists - 4) {
          MSG_WARN("Maximum number of classes per element exceeded.\n");
          break;
       }

       rules_lists[numLists] = hll_rulesMapGetList(style_sheet->c_rules_by_class, dtn->c_element_selector_class[i]);
       if (rules_lists[numLists]) {
          numLists++;
       }
    }
    -}
    byClass lists = getSelectorClassLists (selClass dtn) lists
      where
        getSelectorClassLists []     acc = acc
        getSelectorClassLists (c:cs) acc = case M.lookup c (rulesByClass styleSheet) of
                                             Just l    -> getSelectorClassLists cs (l:acc)
                                             otherwise -> getSelectorClassLists cs acc


    {-
    rules_lists[numLists] = style_sheet->c_rules_by_type[dtn->c_html_element_idx];
    if (rules_lists[numLists])
       numLists++;
    -}
    byElementId lists = if htmlElementIdx dtn == (-1)
                        then lists
                        else ((rulesByType styleSheet) !! (htmlElementIdx dtn)):lists


    {-
    rules_lists[numLists] = style_sheet->c_rules_by_any_element;
    if (rules_lists[numLists])
       numLists++;
    -}
    byAnyElement lists = if null . rulesByAnyElement $ styleSheet
                         then lists
                         else (rulesByAnyElement styleSheet):lists





-- Apply a CSS context to a property list.
--
-- The stylesheets in the context are applied one after the other in the
-- ordering defined by CSS 2.1. Stylesheets that are applied later can
-- overwrite properties set by previous stylesheets. This allows e.g. user
-- styles to overwrite author styles.
cssContextApplyCssContext :: CssContext -> Doctree -> DoctreeNode -> CssDeclarationSet -> CssDeclarationSet -> CssDeclarationSet -> IO (CssDeclarationSet, CssMatchCache)
cssContextApplyCssContext context doctree dtn mainDeclSet importantDeclSet nonCssDeclSet = do

  let cssPrimaryUserAgent       = 0
  let cssPrimaryUser            = 1
  let cssPrimaryAuthor          = 2
  let cssPrimaryAuthorImportant = 3
  let cssPrimaryUserImportant   = 4

  let targetDeclSet1 = defaultCssDeclarationSet
  let matchCache1    = matchCache context

  (targetDeclSet2, matchCache2) <- cssStyleSheetApplyStyleSheet (sheets context !! cssPrimaryUserAgent) targetDeclSet1 matchCache1 doctree dtn

  (targetDeclSet3, matchCache3) <- cssStyleSheetApplyStyleSheet (sheets context !! cssPrimaryUser) targetDeclSet2 matchCache2 doctree dtn

  let targetDeclSet4 = declarationsSetAppend targetDeclSet3 nonCssDeclSet

  (targetDeclSet5, matchCache5) <- cssStyleSheetApplyStyleSheet (sheets context !! cssPrimaryAuthor) targetDeclSet4 matchCache3 doctree dtn

  let targetDeclSet6 = declarationsSetAppend targetDeclSet5 mainDeclSet

  (targetDeclSet7, matchCache7) <- cssStyleSheetApplyStyleSheet (sheets context !! cssPrimaryAuthorImportant) targetDeclSet6 matchCache5 doctree dtn

  let targetDeclSet8 = declarationsSetAppend targetDeclSet7 importantDeclSet

  (targetDeclSet9, matchCache9) <- cssStyleSheetApplyStyleSheet (sheets context !! cssPrimaryUserImportant) targetDeclSet8 matchCache7 doctree dtn

  return (targetDeclSet9, matchCache9)

