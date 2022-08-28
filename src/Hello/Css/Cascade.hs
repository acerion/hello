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
Code in this file is implementing two things:
1. CSS cascading.
2. Matching of CSS selectors.

References:

CSS 2.2 spec
https://www.w3.org/TR/CSS22/cascade.html#cascade
-}




{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}




module Hello.Css.Cascade
  (
    cssContextApplyCssContext

    -- For tests code
  , compoundSelectorMatches'
  , CssCompoundSelectorMatch (..)
  )
  where




import Data.Bits
import Data.Maybe
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import Debug.Trace

import Hello.Css.MatchCache
import Hello.Css.Parser
import Hello.Css.StyleNode
import Hello.Css.StyleSheet
import Hello.Css.Selector
import Hello.Html.Doctree
import Hello.Html.DoctreeNode
import Hello.Utils




cssComplexSelectorMatches :: CssComplexSelector -> Maybe DoctreeNode -> Doctree -> CssMatchCache -> Int -> (Bool, CssMatchCache)
cssComplexSelectorMatches _                                      Nothing    _    mc _           = (False, mc)
cssComplexSelectorMatches (Datum compound)                       (Just dtn) doctree mc cacheOffset = (compoundSelectorMatches compound dtn, mc)
cssComplexSelectorMatches (Link (Datum compound) combinator rem) (Just dtn) doctree mc cacheOffset =
  if compoundSelectorMatches compound dtn
  then matchCombinatorAndRemainder combinator rem dtn doctree mc cacheOffset
  else (False, mc)




-- TODO: remove duplication between cssComplexSelectorMatches and cssComplexSelectorMatches2
cssComplexSelectorMatches2 :: CssCachedComplexSelector -> Doctree -> Maybe DoctreeNode -> CssMatchCache -> (Bool, CssMatchCache)
cssComplexSelectorMatches2 cachedComplexSelector doctree mDtn matchCache = (isMatch, matchCache2)
  where
    (isMatch, matchCache2) = cssComplexSelectorMatches (chain cachedComplexSelector) mDtn doctree matchCache cacheOffset
    cacheOffset            = matchCacheOffset cachedComplexSelector



-- Test whether a pair of <combinator> + <remainder of complex selector>
-- matches a doctree.
matchCombinatorAndRemainder :: CssCombinator -> CssComplexSelector -> DoctreeNode -> Doctree -> CssMatchCache -> Int -> (Bool, CssMatchCache)
matchCombinatorAndRemainder CssCombinatorDescendant      complex dtn doctree mc cacheOffset = matchDescendant           complex (getDtnParent doctree dtn)  doctree mc cacheOffset
matchCombinatorAndRemainder CssCombinatorChild           complex dtn doctree mc cacheOffset = cssComplexSelectorMatches complex (getDtnParent doctree dtn)  doctree mc cacheOffset
matchCombinatorAndRemainder CssCombinatorAdjacentSibling complex dtn doctree mc cacheOffset = cssComplexSelectorMatches complex (getDtnSibling doctree dtn) doctree mc cacheOffset




-- Go upwards of DocTree looking for a matching parent (because of Descendant
-- combinator), and then try to match remainder of Complex Selector.
findMatchingParentAndFollowers  :: CssComplexSelector -> Maybe DoctreeNode -> Doctree -> CssMatchCache -> Int -> Int -> (Bool, CssMatchCache)
findMatchingParentAndFollowers _       Nothing    _       mc _                 _           = (False, mc)
findMatchingParentAndFollowers complex (Just dtn) doctree mc dtnNumForCompound cacheOffset =
  if uniqueNum dtn > dtnNumForCompound
  then case cssComplexSelectorMatches complex (Just dtn) doctree mc cacheOffset of
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
    compoundIdx       = (chainLength complex) - 1
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
compoundSelectorMatches' compound dtn | mismatchOnElement compound dtn     = CssCompoundSelectorMismatchElement
                                      | mismatchOnPseudoClass compound dtn = CssCompoundSelectorMismatchPseudoClass
                                      | mismatchOnId compound dtn          = CssCompoundSelectorMismatchId
                                      | mismatchOnClass compound dtn       = CssCompoundSelectorMismatchClass
                                      | otherwise                          = CssCompoundSelectorMatch
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




-- (minSpec, minPos, minSpecIndex)
type CssSpecificityState = (Int, Int, Int)

-- The data structure with nested lists is a copy of solution from C, where
-- we had an array of pointers to lists of rules. Perhaps we don't really
-- need the list of lists, and all matching rules can be put into a single
-- list?
type MatchingRulesIndices = [Int]
type MatchingRulesGroup   = [[CssRule]]




data MatchingRules = MatchingRules
  {
    rules   :: MatchingRulesGroup
  , indices :: MatchingRulesIndices
  }




minSpecificityForRulesLists :: MatchingRulesGroup -> MatchingRulesIndices -> Int -> CssSpecificityState -> CssSpecificityState
minSpecificityForRulesLists (rl:rls) (ruleIdx:rix) rulesListIdx state = minSpecificityForRulesLists rls rix (rulesListIdx + 1) state2
  where
    state2 = minSpecificityForRulesList rl ruleIdx rulesListIdx state
minSpecificityForRulesLists _        _             _            state = state




minSpecificityForRulesList :: [CssRule] -> Int -> Int -> CssSpecificityState -> CssSpecificityState
minSpecificityForRulesList rulesList ruleIdx rulesListIdx state =
  if length rulesList <= ruleIdx
  then state
  else (minSpecificityForRule rule rulesListIdx state)
  where
    rule = rulesList !! ruleIdx




minSpecificityForRule :: CssRule -> Int -> CssSpecificityState -> CssSpecificityState
minSpecificityForRule rule rulesListIdx state =
  if ((specificity rule < minSpec) || (specificity rule == minSpec && position rule < minPos))
  then (specificity rule, position rule, rulesListIdx) -- update state with index of rules with lowest specificity
  else (minSpec, minPos, minSpecIndex)
  where (minSpec, minPos, minSpecIndex) = state




applyCssRule :: CssCachedDeclarationSet -> Doctree -> Maybe DoctreeNode -> CssRule -> CssCachedDeclarationSet
applyCssRule cachedDeclSet doctree mDtn rule =
  case match of
    True      -> (targetDeclSet', matchCache2)
    otherwise -> (fst cachedDeclSet,  matchCache2)

  where
    targetDeclSet'       = declarationsSetAppend (fst cachedDeclSet) (declarationSet rule)
    (match, matchCache2) = cssComplexSelectorMatches2 (complexSelector rule) doctree mDtn (snd cachedDeclSet)





cssGetMinSpecState :: MatchingRules -> CssSpecificityState
cssGetMinSpecState matchingRules = minSpecificityForRulesLists (rules matchingRules) (indices matchingRules) 0 (minSpec, minPos, minSpecIndex)
  where
    -- Get maximal value of integer as a starting value. This function will be
    -- looking for a minimum. TODO: why shifting by 30 and not 31?
    minSpec :: Int = 1 `shift` 30;
    minPos  :: Int = 1 `shift` 30;

    minSpecIndex = -1



getSomeRule matchingRules minSpecIndex = rulesList !! idx
  where
    rulesList = (rules matchingRules) !! minSpecIndex
    idx       = (indices matchingRules) !! minSpecIndex




updateMatchingRulesIndices matchingRulesIndices minSpecIndex = listReplaceElem matchingRulesIndices (oldElem + 1) minSpecIndex
  where
    oldElem = matchingRulesIndices !! minSpecIndex




-- Apply potentially matching rules from matchingRules with ascending
-- specificity. If specificity is equal, rules are applied in order of
-- appearance. Each matchingRules is sorted already.
applyMatchingRules :: MatchingRules -> Doctree -> Maybe DoctreeNode -> CssCachedDeclarationSet -> IO CssCachedDeclarationSet
applyMatchingRules matchingRules doctree mDtn cachedDeclSet = do
  let state = cssGetMinSpecState matchingRules
  putStrLn ("minSpec = " ++ (show . triplet1st $ state) ++ ", minPos = " ++ (show . triplet2nd $ state) ++ ", minSpecIndex = " ++ (show . triplet3rd $ state))
  let minSpecIndex = triplet3rd state
  if minSpecIndex >= 0
    then
    do
      let rule = getSomeRule matchingRules minSpecIndex

      let cachedDeclSet' = applyCssRule cachedDeclSet doctree mDtn rule

      let matchingRules' = matchingRules { indices = updateMatchingRulesIndices (indices matchingRules) minSpecIndex }

      putStrLn . show $ (fst cachedDeclSet')
      putStrLn . show $ V.fromList . indices $ matchingRules'

      applyMatchingRules matchingRules' doctree mDtn cachedDeclSet'
    else return cachedDeclSet




-- Apply a stylesheet to a list of declarations.
--
-- The declarations (list property+value) are set as defined by the rules in
-- the stylesheet that match at the given node in the document tree.
cssStyleSheetApplyStyleSheet :: CssStyleSheet -> CssCachedDeclarationSet -> Doctree -> DoctreeNode -> IO CssCachedDeclarationSet
cssStyleSheetApplyStyleSheet styleSheet cachedDeclSet doctree dtn = do

  let matchingRules = MatchingRules
        {
          rules = buildMatchingRulesGroupForDtn styleSheet dtn

          -- 'indices' is a list of indices for sub-listss in 'rules'. It
          -- should have the same length as 'rules' list.
        , indices = take (L.length . rules $ matchingRules) $ repeat 0
        }

  cachedDeclSet' <- applyMatchingRules matchingRules doctree (Just dtn) cachedDeclSet
  return cachedDeclSet'




-- This function appears to be implementing (in its own way) this part of CSS
-- 2.2 cascading: "Find all declarations that apply to the element and
-- property in question...".
--
-- The function doesn't seem to fully implement this part: "Declarations
-- apply if the associated selector matches the element in question" because
-- matching of complex selector is not done here. Only a first selector in a
-- complex selector of a rule is compared, and if it matches, the rule is
-- appended to result.
--
-- So this function pre-selects rules, and full matching of complex selectors
-- is done elsewhere (in cssComplexSelectorMatches*?).
buildMatchingRulesGroupForDtn :: CssStyleSheet -> DoctreeNode -> MatchingRulesGroup
buildMatchingRulesGroupForDtn styleSheet dtn = reverse rulesLists
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




type CssCachedDeclarationSet = (CssDeclarationSet, CssMatchCache)




declarationsSetAppend' (targetDs, cache) ds = (declarationsSetAppend targetDs ds, cache)





-- Apply a CSS context to a property list.
--
-- The stylesheets in the context are applied one after the other in the
-- ordering defined by CSS 2.1. Stylesheets that are applied later can
-- overwrite properties set by previous stylesheets. This allows e.g. user
-- styles to overwrite author styles.
cssContextApplyCssContext :: CssContext -> Doctree -> DoctreeNode -> StyleNode -> IO (CssDeclarationSet, CssMatchCache)
cssContextApplyCssContext context doctree dtn styleNode = do

  let cachedDeclSet1 = (defaultCssDeclarationSet, matchCache context)

  cachedDeclSet2 <- cssStyleSheetApplyStyleSheet (getSheet context CssPrimaryUserAgent) cachedDeclSet1 doctree dtn

  cachedDeclSet3 <- cssStyleSheetApplyStyleSheet (getSheet context CssPrimaryUser) cachedDeclSet2 doctree dtn

  let cachedDeclSet4 = declarationsSetAppend' cachedDeclSet3 (nonCssDeclSet styleNode)

  cachedDeclSet5 <- cssStyleSheetApplyStyleSheet (getSheet context CssPrimaryAuthor) cachedDeclSet4 doctree dtn

  let cachedDeclSet6 = declarationsSetAppend' cachedDeclSet5 (mainDeclSet styleNode)

  cachedDeclSet7 <- cssStyleSheetApplyStyleSheet (getSheet context CssPrimaryAuthorImportant) cachedDeclSet6 doctree dtn

  let cachedDeclSet8 = declarationsSetAppend' cachedDeclSet7 (importantDeclSet styleNode)

  cachedDeclSet9 <- cssStyleSheetApplyStyleSheet (getSheet context CssPrimaryUserImportant) cachedDeclSet8 doctree dtn

  return cachedDeclSet9





