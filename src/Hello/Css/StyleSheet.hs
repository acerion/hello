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

This file is derived from dillo-3.0.5/src/cssparser.cc.
Copyright assignments from that file:
Copyright 2004 Sebastian Geerken <sgeerken@dillo.org>
Copyright 2008-2009 Johannes Hofmann <Johannes.Hofmann@gmx.de>
Additional note in cssparser.cc:
"This file is heavily based on the CSS parser of dillo-0.8.0-css-3 -
a dillo1 based CSS prototype written by Sebastian Geerken."
-}




{-# LANGUAGE OverloadedStrings #-}




module Hello.Css.StyleSheet
  (
    CssStyleSheet (..)
  , insertRuleToStyleSheet
  , defaultStyleSheet

  , CssStyleSheets

  , CssRulesMap

  , CssContext (..)
  , defaultCssContext
  , cssContextAddRule
  , getSheet
  , setSheet

  , CssSheetSelector (..)

  , parseCss
  )
where




import Prelude
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.List as L
import Debug.Trace

--import Hello.Chain
import Hello.Css.Declaration
import Hello.Css.Tokenizer
import Hello.Css.Parser.Rule
import Hello.Css.Rule
import Hello.Css.Selector
import Hello.Utils




type CssRulesMap = M.Map T.Text [CssRule]




data CssStyleSheet = CssStyleSheet {
    rulesById          :: CssRulesMap -- CSS rules, in which topmost compound selector is characterized by its "id".
  , rulesByClass       :: CssRulesMap -- CSS rules, in which topmost compound selector is characterized by its "class".
  -- TODO: list of lists to be replaced with vector indexed by element.
  , rulesByType        :: [[CssRule]] -- CSS rules, in which topmost compound selector is characterized by its "specific html element".
  , rulesByAnyElement  :: [CssRule]   -- CSS rules, in which topmost compound selector is characterized by its "any html element".
  , rulesInvalid       :: [CssRule]   -- Rules that aren't valid style rules
  }


instance Show CssStyleSheet where
  show (CssStyleSheet i c t a inv) = "CssStyleSheet {\n" ++
                                     "rulesById { "     ++ show i ++ " }\n\n" ++
                                     "rulesByClass { "  ++ show c ++ " }\n\n" ++
                                     "rulesByType  { "  ++ show t ++ " }\n\n" ++
                                     "rulesByAny { "    ++ show a ++ " }\n" ++
                                     "rulesInvalid { "  ++ show inv ++ " }\n" ++
                                     "}\n\n\n"



defaultStyleSheet :: CssStyleSheet
defaultStyleSheet = CssStyleSheet { rulesById         = M.empty
                                  , rulesByClass      = M.empty
                                  , rulesByType       = replicate styleSheetElementCount []
                                  , rulesByAnyElement = []
                                  , rulesInvalid      = []
                                  }




{-
Insert a rule into style sheet

To improve matching performance the rules are organized into rule lists based
on the topmost compound selector of their selector.
-}
insertRuleToStyleSheet :: CssStyleSheet -> CssRule -> CssStyleSheet
insertRuleToStyleSheet sheet rule
  -- Put a rule in a bucket. Decide which bucket to choose by looking at
  -- topmost compound selector in the complex selector of given rule.
  | compoundHasId compound             = sheet { rulesById         = updatedRulesById }
  | compoundHasClass compound          = sheet { rulesByClass      = updatedRulesByClass }
  | compoundHasSpecificType compound   = sheet { rulesByType       = updatedRulesByType }
  | compoundHasUniversalType compound  = sheet { rulesByAnyElement = updatedRulesByAnyElement }
  | compoundHasUnexpectedType compound = trace ("[NN] insert rule to stylesheet: unexpected element: " ++ (show . selectorTagName $ compound)) sheet { rulesInvalid = updatedRulesInvalid }
  | otherwise                          = sheet

  where
    compound = getTopCompound rule

    updatedRulesById    = updateMapOfLists (rulesById sheet) (selectorId compound) rule
    updatedRulesByClass = updateMapOfLists (rulesByClass sheet) (head . selectorClass $ compound) rule

    updatedThisElementRules = insertRuleInListOfRules (thisElementRules sheet (compoundSpecificType compound)) rule
    updatedRulesByType = listReplaceElem (rulesByType sheet) updatedThisElementRules (unCssTypeSelector . selectorTagName $ compound)

    updatedRulesByAnyElement = insertRuleInListOfRules (rulesByAnyElement sheet) rule

    updatedRulesInvalid = rule:(rulesInvalid sheet)

    thisElementRules sheetArg (Just t) = rulesByType sheetArg !! t
    thisElementRules _        Nothing  = []




{-
Update map of lists of rules (update by inserting the rule into specific
list).

A map can be indexed by either CSS ID or CSS class.

Value of a map, associated with the index (map key) is a list of rules, each
of the rules having given id/class in topmost compound selector of a complex
selector.
-}
updateMapOfLists :: CssRulesMap -> T.Text -> CssRule -> CssRulesMap
updateMapOfLists rulesMap key rule = case M.lookup key rulesMap of
                                       Just list -> M.insert key (insertRuleInListOfRules list rule) rulesMap
                                       Nothing   -> M.insert key (insertRuleInListOfRules []   rule) rulesMap




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
    (smallerOrEqual, larger) = L.span (\r -> specificity rule >= specificity r) list




-- This type could have been a Data.Map, but I decided to go for a record.
data CssStyleSheets = CssStyleSheets
  { sheetUserAgent       :: CssStyleSheet
  , sheetUser            :: CssStyleSheet
  , sheetAuthor          :: CssStyleSheet
  , sheetAuthorImportant :: CssStyleSheet
  , sheetUserImportant   :: CssStyleSheet
  } deriving (Show)




defaultStyleSheets :: CssStyleSheets
defaultStyleSheets = CssStyleSheets
  { sheetUserAgent       = defaultStyleSheet
  , sheetUser            = defaultStyleSheet
  , sheetAuthor          = defaultStyleSheet
  , sheetAuthorImportant = defaultStyleSheet
  , sheetUserImportant   = defaultStyleSheet
  }




data CssContext = CssContext {
    sheets       :: CssStyleSheets
  , rulePosition :: Int
  , cssOrigin    :: CssOrigin
  } deriving (Show)




defaultCssContext :: CssContext
defaultCssContext = CssContext { sheets       = defaultStyleSheets
                               , rulePosition = 0
                               , cssOrigin    = CssOriginUserAgent
                               }




data CssSheetSelector =
    CssPrimaryUserAgent
  | CssPrimaryUser
  | CssPrimaryAuthor
  | CssPrimaryAuthorImportant
  | CssPrimaryUserImportant
  deriving (Eq, Show)




-- Update one of sheets in a container of sheets.
updateSheet :: CssStyleSheets -> CssSheetSelector -> CssStyleSheet -> CssStyleSheets
updateSheet sheetsArg selector sheet = case selector of
                                         CssPrimaryUserAgent       -> sheetsArg { sheetUserAgent       = sheet }
                                         CssPrimaryUser            -> sheetsArg { sheetUser            = sheet }
                                         CssPrimaryAuthor          -> sheetsArg { sheetAuthor          = sheet }
                                         CssPrimaryAuthorImportant -> sheetsArg { sheetAuthorImportant = sheet }
                                         CssPrimaryUserImportant   -> sheetsArg { sheetUserImportant   = sheet }




cssRuleIsSafe :: CssRule -> Bool
cssRuleIsSafe rule = (not . cssComplexSelectorHasPseudoClass . complexSelector $ rule) || (isSafe . declarationSet $ rule)




-- Does any compound selector in given complex selector have non-empty list
-- of pseudo class simple selectors? Remember that C/C++ code can use only
-- first pseudo class.
cssComplexSelectorHasPseudoClass :: CssComplexSelector -> Bool
cssComplexSelectorHasPseudoClass complex = any f complex
  where
    f x = case x of
            WrapCompound c -> not . null . selectorPseudoClass $ c
            _ -> False




cssContextAddRules :: CssContext -> [(CssRule, Bool)] -> CssContext
cssContextAddRules context []     = context
cssContextAddRules context ((rule, isImportant):xs) = cssContextAddRules (cssContextAddRule context (rule, sheetSelector)) xs
  where
    sheetSelector = case cssOrigin context of
                      CssOriginAuthor    -> if isImportant then CssPrimaryAuthorImportant else CssPrimaryAuthor
                      CssOriginUser      -> if isImportant then CssPrimaryUserImportant else CssPrimaryUser
                      CssOriginUserAgent -> CssPrimaryUserAgent




cssContextAddRule :: CssContext -> (CssRule, CssSheetSelector) -> CssContext
cssContextAddRule context (rule, sheetSelector)  =
  -- TODO: should we increment rulePosition in a context, to which a rule
  -- is not being added (in "then" branch)?
  if (cssOrigin context == CssOriginAuthor) && (not . cssRuleIsSafe $ rule)
  then trace "[WW] Ignoring unsafe author style that might reveal browsing history" (context{rulePosition = rulePosition context + 1})
  else cssContextAddRule' . ruleSetPosition $ (context, (rule, sheetSelector))




ruleSetPosition :: (CssContext, (CssRule, CssSheetSelector)) -> (CssContext, (CssRule, CssSheetSelector))
ruleSetPosition (context, (rule, sheetSelector)) = (context, (rule { position = rulePosition context }, sheetSelector))





-- Add given rule to a style sheet in given context. The style sheet is
-- selected by 'sheetSelector' argument.
cssContextAddRule' :: (CssContext, (CssRule, CssSheetSelector)) -> CssContext
cssContextAddRule' (context, (rule, sheetSelector)) = context { sheets       = updateSheet (sheets context) sheetSelector updatedSheet
                                                              , rulePosition = rulePosition context + 1
                                                              }
  where
    updatedSheet = insertRuleToStyleSheet (getSheet context sheetSelector) rule



getSheet :: CssContext -> CssSheetSelector -> CssStyleSheet
getSheet context selector = case selector of
                              CssPrimaryUserAgent       -> sheetUserAgent . sheets $ context
                              CssPrimaryUser            -> sheetUser . sheets $ context
                              CssPrimaryAuthor          -> sheetAuthor . sheets $ context
                              CssPrimaryAuthorImportant -> sheetAuthorImportant . sheets $ context
                              CssPrimaryUserImportant   -> sheetUserImportant . sheets $ context




-- TODO: this function duplicates updateSheet
setSheet :: CssSheetSelector -> CssStyleSheet -> CssContext -> CssContext
setSheet selector sheet context = case selector of
                                    CssPrimaryUserAgent       -> context { sheets = (sheets context) { sheetUserAgent       = sheet } }
                                    CssPrimaryUser            -> context { sheets = (sheets context) { sheetUser            = sheet } }
                                    CssPrimaryAuthor          -> context { sheets = (sheets context) { sheetAuthor          = sheet } }
                                    CssPrimaryAuthorImportant -> context { sheets = (sheets context) { sheetAuthorImportant = sheet } }
                                    CssPrimaryUserImportant   -> context { sheets = (sheets context) { sheetUserImportant   = sheet } }



{-
makeRulePairs :: [CssComplexSelector] -> CssDeclarationSets -> [(CssRule, Bool)] -> [(CssRule, Bool)]
makeRulePairs []     _        acc = reverse acc
makeRulePairs (x:xs) declSets acc | addBoth      = makeRulePairs xs declSets ((ruleImp, True) : (rule, False) : acc)
                                  | addRegular   = makeRulePairs xs declSets ((rule, False) : acc)
                                  | addImportant = makeRulePairs xs declSets ((ruleImp, True) : acc)
                                  | otherwise    = makeRulePairs xs declSets acc

  where rule    = ruleCtor x (fst declSets)
        ruleImp = ruleCtor x (snd declSets)
        ruleCtor cplxSel decls = CssRule { complexSelector = cplxSel
                                         , declarationSet  = decls
                                         , specificity     = selectorSpecificity cplxSel
                                         , position        = 0 -- Position of a rule will be set at the moment of inserting the rule to CSS context
                                         }
        addRegular   = not . S.null . items . fst $ declSets  -- Should add a regular rule to accumulator?
        addImportant = not . S.null . items . snd $ declSets  -- Should add an important rule to accumulator?
        addBoth      = addRegular && addImportant             -- Should add both regular and imporant rules to accumulator?




rulesetToRulesWithImportance :: (CssParser, CssToken) -> ((CssParser, CssToken), [(CssRule, Bool)])
rulesetToRulesWithImportance (parser, token) = case parseStyleRule (parser, token) of
                                                 (pat', Nothing)        -> (pat', [])
                                                 (pat', Just parsedStyleRule) -> (pat', rulesWithImportance)
                                                   where
                                                     rulesWithImportance = makeRulePairs selectors (content parsedStyleRule) []

                                                     -- Notice that only at this stage of parsing we turn lists of
                                                     -- compound-selectors+combinators into lists of true
                                                     -- complex selectors.
                                                     selectors = fmap mkComplexSelector (prelude parsedStyleRule)




parseRuleset :: ((CssParser, CssToken), CssContext) -> ((CssParser, CssToken), CssContext)
parseRuleset (pat, context) = ((p2, t2), updatedContext)
  where
    updatedContext = cssContextAddRules context rulesWithImportance
    ((p2, t2), rulesWithImportance) = rulesetToRulesWithImportance pat
-}




{-
Parse style rules and add them to context.
"p.sth > h1.other { color: red; width: 10px !important; }"      ->     [(CssRule, Bool)]
-}
parseCss :: (CssParser, CssContext) -> CssContext
parseCss (parser, context) = cssContextAddRules context rules
  where
    (_pat, rules) = parseCssRules . startTokenizer $ parser



