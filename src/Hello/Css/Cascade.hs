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




import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
--import Debug.Trace

import System.IO

import Hello.Css.Declaration
import Hello.Css.Rule
import Hello.Css.StyleNode
import Hello.Css.StyleSheet
import Hello.Css.SelectorMatch
import Hello.Html.Doctree
import Hello.Html.DoctreeNode




type MatchingRules = [CssRule]




applyCssRule :: CssDeclarationSet -> Doctree -> DoctreeNode -> CssRule -> CssDeclarationSet
applyCssRule declSet doctree dtn rule =
  if complexSelectorMatches (complexSelector rule) doctree dtn
  then declarationsSetAppend declSet (declarationSet rule)
  else declSet




-- Apply potentially matching rules from matchingRules with ascending
-- specificity. If specificity is equal, rules are applied in order of
-- appearance. Each matchingRules is sorted already.
applyMatchingRules :: Handle -> MatchingRules -> Doctree -> DoctreeNode -> CssDeclarationSet -> IO CssDeclarationSet
applyMatchingRules fHandle matchingRules doctree dtn declSet = do

  -- TODO: uncomment this line and observe value of tree's top node num. It's
  -- constantly increasing, as if the function was called with constantly
  -- updated doctree, each time a new element is added to the doctree. This
  -- may be a great waste of resources: to call matching function on
  -- constantly updated doctree.
  --putStrLn ("Is topNodeNum increasing? " ++ (show . topNodeNum $ doctree))

  let sortedRules = L.sortBy compareRules matchingRules

  let declSet' = foldr (\ rule ds -> applyCssRule ds doctree dtn rule) declSet sortedRules

  let debugString1 = "dtn = " ++ (show dtn) ++ "\n\n"
  hPutStr fHandle debugString1
  let debugString2 = "sorted rules = " ++ (show sortedRules) ++ "\n\n"
  hPutStr fHandle debugString2
  let debugString3 = "updated declSet = " ++ (show declSet') ++ "\n\n\n\n\n"
  hPutStr fHandle debugString3

  return declSet'




-- TODO: check if all the conditions are included, and whether they are used
-- for calculations correctly.
compareRules :: CssRule -> CssRule -> Ordering
compareRules r1 r2 | (specificity r1) < (specificity r2) = GT
                   | (specificity r1) > (specificity r2) = LT
                   | (position r1)    < (position r2)    = GT
                   | (position r1)    > (position r2)    = LT
                   | otherwise                           = EQ




-- Apply a stylesheet to a list of declarations.
--
-- The declarations (list property+value) are set as defined by the rules in
-- the stylesheet that match at the given node in the document tree.
cssStyleSheetApplyStyleSheet :: Handle -> CssStyleSheet -> CssDeclarationSet -> Doctree -> DoctreeNode -> IO CssDeclarationSet
cssStyleSheetApplyStyleSheet fHandle styleSheet declSet doctree dtn = do
  let matchingRules = buildMatchingRulesForDtn styleSheet dtn
  applyMatchingRules fHandle matchingRules doctree dtn declSet




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
buildMatchingRulesForDtn :: CssStyleSheet -> DoctreeNode -> MatchingRules
buildMatchingRulesForDtn styleSheet dtn = concat rulesLists
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




-- Apply a CSS context to a property list.
--
-- The stylesheets in the context are applied one after the other in the
-- ordering defined by CSS 2.1. Stylesheets that are applied later can
-- overwrite properties set by previous stylesheets. This allows e.g. user
-- styles to overwrite author styles.
cssContextApplyCssContext :: Handle -> CssContext -> Doctree -> DoctreeNode -> StyleNode -> IO CssDeclarationSet
cssContextApplyCssContext fHandle context doctree dtn styleNode = do

  let declSet1 = defaultCssDeclarationSet

  declSet2 <- cssStyleSheetApplyStyleSheet fHandle (getSheet context CssPrimaryUserAgent) declSet1 doctree dtn

  declSet3 <- cssStyleSheetApplyStyleSheet fHandle(getSheet context CssPrimaryUser) declSet2 doctree dtn

  let declSet4 = declarationsSetAppend declSet3 (nonCssDeclSet styleNode)

  declSet5 <- cssStyleSheetApplyStyleSheet fHandle (getSheet context CssPrimaryAuthor) declSet4 doctree dtn

  let declSet6 = declarationsSetAppend declSet5 (mainDeclSet styleNode)

  declSet7 <- cssStyleSheetApplyStyleSheet fHandle (getSheet context CssPrimaryAuthorImportant) declSet6 doctree dtn

  let declSet8 = declarationsSetAppend declSet7 (importantDeclSet styleNode)

  cssStyleSheetApplyStyleSheet fHandle (getSheet context CssPrimaryUserImportant) declSet8 doctree dtn





