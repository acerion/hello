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

This file is derived from dillo-3.0.5/src/cssparser.cc.
Copyright assignments from that file:
Copyright 2004 Sebastian Geerken <sgeerken@dillo.org>
Copyright 2008-2009 Johannes Hofmann <Johannes.Hofmann@gmx.de>
Additional note in cssparser.cc:
"This file is heavily based on the CSS parser of dillo-0.8.0-css-3 -
a dillo1 based CSS prototype written by Sebastian Geerken."
-}




module Hello.Css.StyleSheet( CssStyleSheet (..)
                           , CssRulesMap (..)
                           , addRuleToStyleSheet

                           , CssMatchCache (..)
                           , matchCacheResize

                           , CssContext (..)
                           , cssContextAddRule

                           , parseRuleset
                           , rulesetToRulesWithOrigin

                           , CssSheetSelector (..)
                           , getSheetIndex
                           , getSheetSelector
                           ) where




import Prelude
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Sequence as S
import Debug.Trace
import Control.Monad -- when

import Hello.Css.Tokenizer
import Hello.Css.Parser
import Css
import Hello.Utils




type CssRulesMap = M.Map T.Text [CssRule]




data CssStyleSheet = CssStyleSheet {
    rulesById          :: CssRulesMap -- CSS rules, in which topmost compound selector is characterized by its "id".
  , rulesByClass       :: CssRulesMap -- CSS rules, in which topmost compound selector is characterized by its "class".
  -- TODO: list of lists to be replaced with vector indexed by element.
  , rulesByType        :: [[CssRule]] -- CSS rules, in which topmost compound selector is characterized by its "specific html element".
  , rulesByAnyElement  :: [CssRule]   -- CSS rules, in which topmost compound selector is characterized by its "any html element".

  , requiredMatchCache :: Int
  } deriving (Show)




{-
Insert a rule into style sheet.

To improve matching performance the rules are organized into rule lists based
on the topmost compound selector of their selector.
-}
addRuleToStyleSheet :: CssStyleSheet -> CssRule -> CssStyleSheet
addRuleToStyleSheet sheet rule = case insertRuleToStyleSheet rule sheet of
                                   (0, outSheet) -> outSheet
                                   (i, outSheet) -> outSheet { requiredMatchCache = newRequiredMatchCache sheet rule }

  where
    newRequiredMatchCache s r = if getRequiredMatchCache r > requiredMatchCache s
                                -- then trace ("Updating from " ++ (show . requiredMatchCache $ s) ++ " to " ++ (show . getRequiredMatchCache $ r)) (getRequiredMatchCache r)
                                then getRequiredMatchCache r
                                else requiredMatchCache s




{-
The returned int value is needed only in FFI code, so that the FFI code knows
which field of style sheet has changed and which input/output pointer to
poke.
-}
insertRuleToStyleSheet :: CssRule -> CssStyleSheet -> (Int, CssStyleSheet)
insertRuleToStyleSheet rule sheet
  -- Put a rule in a bucket. Decide which bucket to choose by looking at
  -- topmost compound selector in the complex selector of given rule.
  | compoundHasId compound             = (1, sheet { rulesById         = updatedRulesById })
  | compoundHasClass compound          = (2, sheet { rulesByClass      = updatedRulesByClass })
  | compoundHasSpecificType compound   = (3, sheet { rulesByType       = updatedRulesByType })
  | compoundHasUniversalType compound  = (4, sheet { rulesByAnyElement = updatedRulesByAnyElement })
  | compoundHasUnexpectedType compound = (trace ("[NN] insert rule to stylesheet: unexpected element: " ++ (show . selectorTagName $ compound)) (0, sheet))
  | otherwise                          = (0, sheet)

  where
    compound = getTopCompound rule

    updatedRulesById    = updateMapOfLists (rulesById sheet) (selectorId compound) rule
    updatedRulesByClass = updateMapOfLists (rulesByClass sheet) (head . selectorClass $ compound) rule

    updatedThisElementRules = insertRuleInListOfRules (thisElementRules sheet (compoundSpecificType compound)) rule
    updatedRulesByType = listReplaceElem (rulesByType sheet) updatedThisElementRules (unCssTypeSelector . selectorTagName $ compound)

    updatedRulesByAnyElement = insertRuleInListOfRules (rulesByAnyElement sheet) rule

    thisElementRules sheet (Just t) = (rulesByType sheet) !! t
    thisElementRules sheet Nothing  = []




{-
Update map of lists of rules (update by inserting the rule into specific
list).

A map can be indexed by either CSS ID or CSS class.

Value of a map, associated with the index (map key) is a list of rules, each
of the rules having given id/class in topmost compound selector of a complex
selector.
-}
updateMapOfLists :: CssRulesMap -> T.Text -> CssRule -> CssRulesMap
updateMapOfLists map key rule = case M.lookup key map of
                                  Just list -> M.insert key (insertRuleInListOfRules list rule) map
                                  Nothing   -> M.insert key (insertRuleInListOfRules []   rule) map




{-
Insert rule with increasing specificity.

If two rules have the same specificity, the one that was added later will be
added behind the others. This gives later added rules more weight.

TODO:
The goal of proper ordering of rules where newer rules go at the end of slice
of rules with the same specificity can be achieved also with this
implementation:

insertRuleInListOfRules list rule = reverse $ L.insertBy ord rule (reverse list)
  where ord r2 r1 = compare (specificity r1) (specificity r2)

But I don't know which version is more effective: with two reverses or with
span + concat.
-}
insertRuleInListOfRules :: [CssRule] -> CssRule -> [CssRule]
insertRuleInListOfRules list rule = L.concat [smallerOrEqual, [rule], larger]
  where
    (smallerOrEqual, larger) = L.span (\r -> (specificity rule) >= (specificity r)) list




type CssMatchCache = [Int]




data CssContext = CssContext {
    sheets       :: [CssStyleSheet]
  , matchCache   :: CssMatchCache
  , rulePosition :: Int
  } deriving (Show)





data CssSheetSelector =
    CssPrimaryUserAgent       -- = 0
  | CssPrimaryUser            -- = 1
  | CssPrimaryAuthor          -- = 2
  | CssPrimaryAuthorImportant -- = 3
  | CssPrimaryUserImportant   -- = 4
  | CssPrimaryOrderSize       -- = 5 -- TODO: to be removed
  deriving (Eq, Show)




cssRuleIsSafe rule = (not . cssComplexSelectorHasPseudoClass . complexSelector $ rule) || (isSafe . declarationSet $ rule)



-- Does any compound selector in given complex selector have non-empty list
-- of pseudo class simple selectors? Remember that C/C++ code can use only
-- first pseudo class.
cssComplexSelectorHasPseudoClass :: CssComplexSelector1 -> Bool
cssComplexSelectorHasPseudoClass complex = any (\link -> not . null . selectorPseudoClass . compound $ link) (chainToLinks (chain complex) [])




cssContextAddRules :: CssContext -> [(Maybe CssRule, CssSheetSelector)] -> CssContext
cssContextAddRules context []                              = context
cssContextAddRules context ((Just rule, sheetSelector):xs) = cssContextAddRules (cssContextAddRule context rule sheetSelector) xs
cssContextAddRules context ((Nothing, _):xs)               = cssContextAddRules context xs




cssContextAddRule :: CssContext -> CssRule -> CssSheetSelector -> CssContext
cssContextAddRule context rule sheetSelector =
  -- TODO: should we increpement rulePosition in a context, to which a rule
  -- is not being added (in "then" branch)?
  if (sheetSelector == CssPrimaryAuthor || sheetSelector == CssPrimaryAuthorImportant) && (not . cssRuleIsSafe $ rule)
  then trace ("[WW] Ignoring unsafe author style that might reveal browsing history") (context{rulePosition = (rulePosition context) + 1})
  else cssContextAddRule' context ruleWithOffset sheetSelector

  where
    -- Set match cache offset of selector.
    ruleWithOffset :: CssRule
    ruleWithOffset = if (-1) == (matchCacheOffset . complexSelector $ rule)
                     then rule{ complexSelector = newComplexSelector . complexSelector $ rule
                              , position = rulePosition context
                              }
                     else rule{ position = rulePosition context }
      where
        newComplexSelector cplxSel = cplxSel{matchCacheOffset = length . matchCache $ context}




-- Add given rule to a style sheet in given context. The style sheet is
-- selected by 'sheetSelector' argument.
cssContextAddRule' :: CssContext -> CssRule -> CssSheetSelector -> CssContext
cssContextAddRule' context rule sheetSelector = context{ sheets     = listReplaceElem (sheets context) updatedSheet (getSheetIndex sheetSelector)
                                                       , matchCache = if requiredCacheSize > existingCacheSize
                                                                      then matchCacheResize (matchCache context) requiredCacheSize
                                                                      else (matchCache context)
                                                       , rulePosition = (rulePosition context) + 1
                                                       }
  where
    updatedSheet      = addRuleToStyleSheet ((sheets $ context) !! (getSheetIndex sheetSelector)) rule
    existingCacheSize = length . matchCache $ context
    requiredCacheSize = getRequiredMatchCache rule




getSheetIndex sheetSelector = case sheetSelector of
                                CssPrimaryUserAgent       -> 0
                                CssPrimaryUser            -> 1
                                CssPrimaryAuthor          -> 2
                                CssPrimaryAuthorImportant -> 3
                                CssPrimaryUserImportant   -> 4
                                CssPrimaryOrderSize       -> 5

getSheetSelector sheetIndex = case sheetIndex of
                                0 -> CssPrimaryUserAgent
                                1 -> CssPrimaryUser
                                2 -> CssPrimaryAuthor
                                3 -> CssPrimaryAuthorImportant
                                4 -> CssPrimaryUserImportant
                                5 -> CssPrimaryOrderSize




matchCacheResize cache size = newCache
  where
    oldSize = length cache
    newCache = cache ++ (replicate (size - oldSize) (-1))




makeRulePairs :: [CssComplexSelector1] -> CssDeclarationSet -> CssDeclarationSet -> CssOrigin -> [(Maybe CssRule, CssSheetSelector)] -> [(Maybe CssRule, CssSheetSelector)]
makeRulePairs []     _       _          _      acc = reverse acc
makeRulePairs (x:xs) declSet declSetImp origin acc =
  case origin of
    CssOriginUserAgent -> makeRulePairs xs declSet declSetImp origin ((rule, CssPrimaryUserAgent) : acc)
    CssOriginUser      -> makeRulePairs xs declSet declSetImp origin ((ruleImp, CssPrimaryUserImportant) : (rule, CssPrimaryUser) : acc)
    CssOriginAuthor    -> makeRulePairs xs declSet declSetImp origin ((ruleImp, CssPrimaryAuthorImportant) : (rule, CssPrimaryAuthor) : acc)

  where rule    = ruleCtor x declSet
        ruleImp = ruleCtor x declSetImp
        ruleCtor cplxSel decls = if not . S.null . items $ decls
                                 then Just CssRule { complexSelector = cplxSel
                                                   , declarationSet = decls
                                                   , specificity = selectorSpecificity cplxSel
                                                   , position = 0 } -- Position of a rule will be set at the moment of inserting the rule to CSS context
                                 else Nothing -- Rule with zero declarations would be useless rule.




-- Given list of selectors, and given declaration sets (regular and
-- important), for each of the selectors create one rule and add it to
-- context.
--
-- Each rule can have only one selector, so this function works like this:
-- "for each selector create a rule with given selector and some
-- declarations, and put it in appropriate style sheet in the context".
constructAndAddRules :: CssContext -> [CssComplexSelector1] -> CssDeclarationSet -> CssDeclarationSet -> CssOrigin -> CssContext
constructAndAddRules context []           declSet declSetImp _      = context
constructAndAddRules context selectorList declSet declSetImp origin = updatedContext
  where
    updatedContext = cssContextAddRules context rulePairs
    rulePairs = makeRulePairs selectorList declSet declSetImp origin []




readDeclarations parser token = ((p3, t3), (declSet, declSetImp))
  where
    ((p2, t2), (declSet, declSetImp)) = case token of
                                          CssTokEnd -> ((parser, token), (declSet, declSetImp))
                                          otherwise -> readDeclarations' ((nextToken1 parser{ inBlock = True }), (defaultCssDeclarationSet, defaultCssDeclarationSet))
    (p3, t3) = case t2 of
                 CssTokBraceCurlyClose -> nextToken1 p2{ inBlock = False }
                 _                     -> (p2{ inBlock = False }, t2)



readDeclarations' ((parser, token), (declSet, declSetImp)) =
  case token of
    CssTokEnd             -> ((parser, token), (declSet, declSetImp))
    CssTokBraceCurlyClose -> ((parser, token), (declSet, declSetImp)) -- TODO: this should be (nextToken parser)
                             -- instead of (parser, token): ensure that '}' that is part of "declartions" block
                             -- is handled and consumed, so that the next part of code doesn't have to handle it.
    otherwise             -> readDeclarations' (parseDeclarationWrapper2 (parser, token) (declSet, declSetImp))




rulesetToRulesWithOrigin parser token = ((p3, t3), rulesWithOrigin)
  where
    ((p2, t2), selectorList) = readSelectorList (parser, token)
    ((p3, t3), (declSet, declSetImp)) = readDeclarations p2 t2
    rulesWithOrigin = makeRulePairs selectorList declSet declSetImp (cssOrigin parser) []




parseRuleset parser token context = (p2, t2, updatedContext)
  where
    updatedContext = cssContextAddRules context rulesWithOrigin
    ((p2, t2), rulesWithOrigin) = rulesetToRulesWithOrigin parser token
