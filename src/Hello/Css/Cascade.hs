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

This file is derived from dillo-3.0.5/src/css.cc.
Copyright assignments from css.cc file:
Copyright 2008-2014 Johannes Hofmann <Johannes.Hofmann@gmx.de>
-}




{-
Code in this file is implementing CSS cascading.

References:

CSS 2.2 spec
https://www.w3.org/TR/CSS22/cascade.html#cascade
-}




{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}




module Hello.Css.Cascade
  (
    cssContextApplyCssContext
  )
where




import Data.Bits
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
--import Debug.Trace

import System.IO

import Hello.Css.MatchCache
import Hello.Css.Parser.Declaration
import Hello.Css.Rule
import Hello.Css.StyleNode
import Hello.Css.StyleSheet
import Hello.Css.SelectorMatch
import Hello.Html.Doctree
import Hello.Html.DoctreeNode
import Hello.Utils




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
  } deriving (Show)




minSpecificityForRulesLists :: MatchingRulesGroup -> MatchingRulesIndices -> Int -> CssSpecificityState -> CssSpecificityState
minSpecificityForRulesLists (rl:rls) (ruleIdx:rix) rulesListIdx state = minSpecificityForRulesLists rls rix (rulesListIdx + 1) state2
  where
    state2 = minSpecificityForRulesList rl ruleIdx rulesListIdx state
minSpecificityForRulesLists _        _             _            state = state




minSpecificityForRulesList :: [CssRule] -> Int -> Int -> CssSpecificityState -> CssSpecificityState
minSpecificityForRulesList rulesList ruleIdx rulesListIdx state =
  if length rulesList <= ruleIdx
  then state
  else minSpecificityForRule rule rulesListIdx state
  where
    rule = rulesList !! ruleIdx




-- This function is calculating minimal specificity of a rule, but this document:
-- https://www.w3.org/TR/css-cascade-3/#cascade-specificity
-- says "The declaration with the highest specificity wins."
minSpecificityForRule :: CssRule -> Int -> CssSpecificityState -> CssSpecificityState
minSpecificityForRule rule rulesListIdx state =
  if (specificity rule < minSpec) || (specificity rule == minSpec && position rule < minPos)
  then (specificity rule, position rule, rulesListIdx) -- update state with index of rules with lowest specificity
  else (minSpec, minPos, minSpecIndex)
  where (minSpec, minPos, minSpecIndex) = state




applyCssRule :: CssCachedDeclarationSet -> Doctree -> DoctreeNode -> CssRule -> CssCachedDeclarationSet
applyCssRule cachedDeclSet doctree dtn rule =
  if match
  then (targetDeclSet', matchCache2)
  else (fst cachedDeclSet,  matchCache2)

  where
    targetDeclSet'       = declarationsSetAppend (fst cachedDeclSet) (declarationSet rule)
    (match, matchCache2) = complexSelectorMatches (complexSelector rule) doctree dtn (snd cachedDeclSet)




cssGetMinSpecState :: MatchingRules -> CssSpecificityState
cssGetMinSpecState matchingRules = minSpecificityForRulesLists (rules matchingRules) (indices matchingRules) 0 (minSpec, minPos, minSpecIndex)
  where
    -- Get maximal value of integer as a starting value. This function will be
    -- looking for a minimum. TODO: why shifting by 30 and not 31?
    minSpec :: Int = 1 `shift` 30;
    minPos  :: Int = 1 `shift` 30;

    minSpecIndex = -1




getSomeRule :: MatchingRules -> Int -> CssRule
getSomeRule matchingRules minSpecIndex = rulesList !! idx
  where
    rulesList = rules matchingRules !! minSpecIndex
    idx       = indices matchingRules !! minSpecIndex




updateMatchingRulesIndices :: [Int] -> Int -> [Int]
updateMatchingRulesIndices matchingRulesIndices minSpecIndex = listReplaceElem matchingRulesIndices (oldElem + 1) minSpecIndex
  where
    oldElem = matchingRulesIndices !! minSpecIndex




-- Apply potentially matching rules from matchingRules with ascending
-- specificity. If specificity is equal, rules are applied in order of
-- appearance. Each matchingRules is sorted already.
applyMatchingRules :: Handle -> MatchingRules -> Doctree -> DoctreeNode -> CssCachedDeclarationSet -> IO CssCachedDeclarationSet
applyMatchingRules fHandle matchingRules doctree dtn cachedDeclSet = do
  let state = cssGetMinSpecState matchingRules

  let debugString1 = "minSpec = " ++ (show . triplet1st $ state) ++ ", minPos = " ++ (show . triplet2nd $ state) ++ ", minSpecIndex = " ++ (show . triplet3rd $ state) ++ "\n"
  hPutStr fHandle debugString1

  let minSpecIndex = triplet3rd state
  if minSpecIndex >= 0
    then
    do
      let rule = getSomeRule matchingRules minSpecIndex

      let cachedDeclSet' = applyCssRule cachedDeclSet doctree dtn rule

      let matchingRules' = matchingRules { indices = updateMatchingRulesIndices (indices matchingRules) minSpecIndex }

      let debugString2 = show (fst cachedDeclSet') ++ "\n"
      hPutStr fHandle debugString2
      let debugString3 = show (V.fromList . indices $ matchingRules') ++ "\n"
      hPutStr fHandle debugString3

      applyMatchingRules fHandle matchingRules' doctree dtn cachedDeclSet'
    else return cachedDeclSet




-- Apply a stylesheet to a list of declarations.
--
-- The declarations (list property+value) are set as defined by the rules in
-- the stylesheet that match at the given node in the document tree.
cssStyleSheetApplyStyleSheet :: Handle -> CssStyleSheet -> CssCachedDeclarationSet -> Doctree -> DoctreeNode -> IO CssCachedDeclarationSet
cssStyleSheetApplyStyleSheet fHandle styleSheet cachedDeclSet doctree dtn = do

  let matchingRules = MatchingRules
        {
          rules = buildMatchingRulesGroupForDtn styleSheet dtn

          -- 'indices' is a list of indices for sub-listss in 'rules'. It
          -- should have the same length as 'rules' list.
        , indices = replicate (L.length . rules $ matchingRules) 0
        }

  applyMatchingRules fHandle matchingRules doctree dtn cachedDeclSet




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
-- is done elsewhere (in complexSelectorMatches*?).
buildMatchingRulesGroupForDtn :: CssStyleSheet -> DoctreeNode -> MatchingRulesGroup
buildMatchingRulesGroupForDtn styleSheet dtn = reverse rulesLists
  where
    rulesLists = byAnyElement . byElementId . byClass . bySelId $ []


    {-
    if (dtn->c_element_selector_id) {
       rules_lists[numLists] = ffiRulesMapGetList(style_sheet->c_rules_by_id, dtn->c_element_selector_id);
       if (rules_lists[numLists]) {
          numLists++;
       }
    }
    -}
    bySelId lists = if T.null . selId $ dtn
                    then lists
                    else case M.lookup (selId dtn) (rulesById styleSheet) of
                           Just l  -> l:lists
                           Nothing -> lists


    {-
    for (int i = 0; i < dtn->c_element_selector_class_size; i++) {
       if (i >= maxLists - 4) {
          MSG_WARN("Maximum number of classes per element exceeded.\n");
          break;
       }

       rules_lists[numLists] = ffiRulesMapGetList(style_sheet->c_rules_by_class, dtn->c_element_selector_class[i]);
       if (rules_lists[numLists]) {
          numLists++;
       }
    }
    -}
    byClass lists = getSelectorClassLists (selClass dtn) lists
      where
        getSelectorClassLists []     acc = acc
        getSelectorClassLists (c:cs) acc = case M.lookup c (rulesByClass styleSheet) of
                                             Just l  -> getSelectorClassLists cs (l:acc)
                                             Nothing -> getSelectorClassLists cs acc


    {-
    rules_lists[numLists] = style_sheet->c_rules_by_type[dtn->c_html_element_idx];
    if (rules_lists[numLists])
       numLists++;
    -}
    byElementId lists = if htmlElementIdx dtn == (-1)
                        then lists
                        else (rulesByType styleSheet !! htmlElementIdx dtn):lists


    {-
    rules_lists[numLists] = style_sheet->c_rules_by_any_element;
    if (rules_lists[numLists])
       numLists++;
    -}
    byAnyElement lists = if null . rulesByAnyElement $ styleSheet
                         then lists
                         else rulesByAnyElement styleSheet : lists




type CssCachedDeclarationSet = (CssDeclarationSet, CssMatchCache)




declarationsSetAppend' :: (CssDeclarationSet, CssMatchCache) -> CssDeclarationSet -> (CssDeclarationSet, CssMatchCache)
declarationsSetAppend' (targetDs, cache) ds = (declarationsSetAppend targetDs ds, cache)





-- Apply a CSS context to a property list.
--
-- The stylesheets in the context are applied one after the other in the
-- ordering defined by CSS 2.1. Stylesheets that are applied later can
-- overwrite properties set by previous stylesheets. This allows e.g. user
-- styles to overwrite author styles.
cssContextApplyCssContext :: Handle -> CssContext -> Doctree -> DoctreeNode -> StyleNode -> IO (CssDeclarationSet, CssMatchCache)
cssContextApplyCssContext fHandle context doctree dtn styleNode = do

  let cachedDeclSet1 = (defaultCssDeclarationSet, matchCache context)

  cachedDeclSet2 <- cssStyleSheetApplyStyleSheet fHandle (getSheet context CssPrimaryUserAgent) cachedDeclSet1 doctree dtn

  cachedDeclSet3 <- cssStyleSheetApplyStyleSheet fHandle(getSheet context CssPrimaryUser) cachedDeclSet2 doctree dtn

  let cachedDeclSet4 = declarationsSetAppend' cachedDeclSet3 (nonCssDeclSet styleNode)

  cachedDeclSet5 <- cssStyleSheetApplyStyleSheet fHandle (getSheet context CssPrimaryAuthor) cachedDeclSet4 doctree dtn

  let cachedDeclSet6 = declarationsSetAppend' cachedDeclSet5 (mainDeclSet styleNode)

  cachedDeclSet7 <- cssStyleSheetApplyStyleSheet fHandle (getSheet context CssPrimaryAuthorImportant) cachedDeclSet6 doctree dtn

  let cachedDeclSet8 = declarationsSetAppend' cachedDeclSet7 (importantDeclSet styleNode)

  cssStyleSheetApplyStyleSheet fHandle (getSheet context CssPrimaryUserImportant) cachedDeclSet8 doctree dtn





