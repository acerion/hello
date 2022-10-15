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

  , CssMatchCache

  , CssContext (..)
  , defaultCssContext
  , cssContextAddRule
  , getSheet
  , setSheet
  , increaseMatchCacheSize


  , parseRuleset
  , rulesetToRulesWithOrigin

  , CssSheetSelector (..)

  , parseCss
  )
where




import Prelude
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Sequence as S
import Debug.Trace

import Hello.Chain
import Hello.Css.Tokenizer
import Hello.Css.Parser
import Hello.Css.Selector
import Hello.Css.MatchCache
import Hello.Css.MediaQuery
import Hello.Utils




type CssRulesMap = M.Map T.Text [CssRule]




data CssStyleSheet = CssStyleSheet {
    rulesById          :: CssRulesMap -- CSS rules, in which topmost compound selector is characterized by its "id".
  , rulesByClass       :: CssRulesMap -- CSS rules, in which topmost compound selector is characterized by its "class".
  -- TODO: list of lists to be replaced with vector indexed by element.
  , rulesByType        :: [[CssRule]] -- CSS rules, in which topmost compound selector is characterized by its "specific html element".
  , rulesByAnyElement  :: [CssRule]   -- CSS rules, in which topmost compound selector is characterized by its "any html element".
  }


instance Show CssStyleSheet where
  show (CssStyleSheet i c t a) = "CssStyleSheet {\n" ++
                                 "rulesById { "     ++ show i ++ " }\n\n" ++
                                 "rulesByClass { "  ++ show c ++ " }\n\n" ++
                                 "rulesByType  { "  ++ show t ++ " }\n\n" ++
                                 "rulesByAny { "    ++ show a ++ " }\n" ++
                                 "}\n\n\n"



defaultStyleSheet = CssStyleSheet { rulesById         = M.empty
                                  , rulesByClass      = M.empty
                                  , rulesByType       = replicate styleSheetElementCount []
                                  , rulesByAnyElement = []
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
  | compoundHasUnexpectedType compound = trace ("[NN] insert rule to stylesheet: unexpected element: " ++ (show . selectorTagName $ compound)) sheet
  | otherwise                          = sheet

  where
    compound = getTopCompound rule

    updatedRulesById    = updateMapOfLists (rulesById sheet) (selectorId compound) rule
    updatedRulesByClass = updateMapOfLists (rulesByClass sheet) (head . selectorClass $ compound) rule

    updatedThisElementRules = insertRuleInListOfRules (thisElementRules sheet (compoundSpecificType compound)) rule
    updatedRulesByType = listReplaceElem (rulesByType sheet) updatedThisElementRules (unCssTypeSelector . selectorTagName $ compound)

    updatedRulesByAnyElement = insertRuleInListOfRules (rulesByAnyElement sheet) rule

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




defaultStyleSheets = CssStyleSheets
  { sheetUserAgent       = defaultStyleSheet
  , sheetUser            = defaultStyleSheet
  , sheetAuthor          = defaultStyleSheet
  , sheetAuthorImportant = defaultStyleSheet
  , sheetUserImportant   = defaultStyleSheet
  }




data CssContext = CssContext {
    sheets       :: CssStyleSheets
  , matchCache   :: CssMatchCache
  , rulePosition :: Int
  } deriving (Show)




defaultCssContext = CssContext { sheets       = defaultStyleSheets
                               , matchCache   = matchCacheFromList []
                               , rulePosition = 0
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




cssRuleIsSafe rule = (not . cssComplexSelectorHasPseudoClass . complexSelector $ rule) || (isSafe . declarationSet $ rule)




-- Does any compound selector in given complex selector have non-empty list
-- of pseudo class simple selectors? Remember that C/C++ code can use only
-- first pseudo class.
cssComplexSelectorHasPseudoClass :: CssCachedComplexSelector -> Bool
cssComplexSelectorHasPseudoClass complex = chainAnyDatum (not . null . selectorPseudoClass) (chain complex)




cssContextAddRules :: CssContext -> [(Maybe CssRule, CssSheetSelector)] -> CssContext
cssContextAddRules context []                              = context
cssContextAddRules context ((Just rule, sheetSelector):xs) = cssContextAddRules (cssContextAddRule context sheetSelector rule) xs
cssContextAddRules context ((Nothing, _):xs)               = cssContextAddRules context xs




cssContextAddRule :: CssContext -> CssSheetSelector -> CssRule -> CssContext
cssContextAddRule context sheetSelector rule  =
  -- TODO: should we increment rulePosition in a context, to which a rule
  -- is not being added (in "then" branch)?
  if (sheetSelector == CssPrimaryAuthor || sheetSelector == CssPrimaryAuthorImportant) && (not . cssRuleIsSafe $ rule)
  then trace ("[WW] Ignoring unsafe author style that might reveal browsing history") (context{rulePosition = rulePosition context + 1})
  else cssContextAddRule' . ruleSetOffsetAndPosition $ (context, sheetSelector, rule)




ruleSetOffsetAndPosition (context, ss, rule) = ( context
                                               , ss
                                               , rule { complexSelector = newComplexSelector . complexSelector $ rule
                                                      , position = rulePosition context
                                                      }
                                               )
  where
    newComplexSelector cplxSel = cplxSel { matchCacheOffset = matchCacheSize . matchCache $ context}




-- Add given rule to a style sheet in given context. The style sheet is
-- selected by 'sheetSelector' argument.
cssContextAddRule' :: (CssContext, CssSheetSelector, CssRule) -> CssContext
cssContextAddRule' (context, sheetSelector, rule) = context { sheets       = updateSheet (sheets context) sheetSelector updatedSheet
                                                            , matchCache   = matchCacheIncreaseBy (matchCache context) delta
                                                            , rulePosition = rulePosition context + 1
                                                            }
  where
    updatedSheet = insertRuleToStyleSheet (getSheet context sheetSelector) rule
    delta        = chainDatumLength . chain . complexSelector $ rule




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




increaseMatchCacheSize :: Int -> CssContext -> CssContext
increaseMatchCacheSize size context = context { matchCache = matchCacheIncreaseTo (matchCache context) size }




makeRulePairs :: [CssCachedComplexSelector] -> CssDeclarationSet -> CssDeclarationSet -> CssOrigin -> [(Maybe CssRule, CssSheetSelector)] -> [(Maybe CssRule, CssSheetSelector)]
makeRulePairs []     _       _          _      acc = reverse acc
makeRulePairs (x:xs) declSet declSetImp origin acc =
  case origin of
    CssOriginUserAgent -> makeRulePairs xs declSet declSetImp origin ((rule,    CssPrimaryUserAgent) : acc)
    CssOriginUser      -> makeRulePairs xs declSet declSetImp origin ((ruleImp, CssPrimaryUserImportant) : (rule, CssPrimaryUser) : acc)
    CssOriginAuthor    -> makeRulePairs xs declSet declSetImp origin ((ruleImp, CssPrimaryAuthorImportant) : (rule, CssPrimaryAuthor) : acc)

  where rule    = ruleCtor x declSet
        ruleImp = ruleCtor x declSetImp
        ruleCtor cplxSel decls = if not . S.null . items $ decls
                                 then Just CssRule { complexSelector = cplxSel
                                                   , declarationSet = decls
                                                   , specificity = selectorSpecificity . chain $ cplxSel
                                                   , position = 0 } -- Position of a rule will be set at the moment of inserting the rule to CSS context
                                 else Nothing -- Rule with zero declarations would be useless rule.



{-
-- Given list of selectors, and given declaration sets (regular and
-- important), for each of the selectors create one rule and add it to
-- context.
--
-- Each rule can have only one selector, so this function works like this:
-- "for each selector create a rule with given selector and some
-- declarations, and put it in appropriate style sheet in the context".
constructAndAddRules :: CssContext -> [CssCachedComplexSelector] -> CssDeclarationSet -> CssDeclarationSet -> CssOrigin -> CssContext
constructAndAddRules context []           _       _          _      = context
constructAndAddRules context selectorList declSet declSetImp origin = updatedContext
  where
    updatedContext = cssContextAddRules context rulePairs
    rulePairs = makeRulePairs selectorList declSet declSetImp origin []
-}



readDeclarations parser token = ((p3, t3), (declSet, declSetImp))
  where
    ((p2, t2), (declSet, declSetImp)) = case token of
                                          CssTokEnd -> ((parser, token), (declSet, declSetImp))
                                          _         -> readDeclarations' (nextToken1 parser{ inBlock = True }, (defaultCssDeclarationSet, defaultCssDeclarationSet))
    (p3, t3) = case t2 of
                 CssTokBraceCurlyClose -> nextToken1 p2{ inBlock = False }
                 _                     -> (p2{ inBlock = False }, t2)



readDeclarations' ((parser, token), (declSet, declSetImp)) =
  case token of
    CssTokEnd             -> ((parser, token), (declSet, declSetImp))
    CssTokBraceCurlyClose -> ((parser, token), (declSet, declSetImp)) -- TODO: this should be (nextToken parser)
                             -- instead of (parser, token): ensure that '}' that is part of "declartions" block
                             -- is handled and consumed, so that the next part of code doesn't have to handle it.
    _                     -> readDeclarations' (parseDeclarationWrapper (parser, token) (declSet, declSetImp))




rulesetToRulesWithOrigin parser token = ((p3, t3), rulesWithOrigin)
  where
    ((p2, t2), selectorList) = readSelectorList (parser, token)
    ((p3, t3), (declSet, declSetImp)) = readDeclarations p2 t2
    rulesWithOrigin = makeRulePairs selectorList declSet declSetImp (cssOrigin parser) []




parseRuleset ((parser, token), context) = ((p2, t2), updatedContext)
  where
    updatedContext = cssContextAddRules context rulesWithOrigin
    ((p2, t2), rulesWithOrigin) = rulesetToRulesWithOrigin parser token




{-
TODO: look how much is still to be implemented in the parser. Only the part
in first branch of #if is implemented.

void parseCss(DilloHtml *html, const DilloUrl * baseUrl, c_css_context_t * context, const char * buf, int buflen, CssOrigin origin)
{
#if 1
   CssParser parser_(origin, baseUrl, buf, buflen);
   hll_parseCss(&parser_.m_parser, &parser_.m_token, context);
#else
   CssParser parser_(origin, baseUrl, buf, buflen);
   bool importsAreAllowed = true;
   c_css_token_t * token = &parser_.m_token;
   c_css_parser_t * parser = &parser_.m_parser;
   while (token->c_type != CSS_TOKEN_TYPE_END) {
      if (token->c_type == CSS_TOKEN_TYPE_CHAR &&
          token->c_value[0] == '@') {
         nextToken(parser, token);
         if (token->c_type == CSS_TOKEN_TYPE_IDENT) {
            if (dStrAsciiCasecmp(token->c_value, "import") == 0 &&
                html != NULL &&
                importsAreAllowed) {
               //fprintf(stderr, "MEAS: PARSE IMPORT\n");
               parseImport(html, parser, token, parser_.m_base_url);
            } else if (dStrAsciiCasecmp(token->c_value, "media") == 0) {
               //fprintf(stderr, "MEAS: PARSE MEDIA\n");
               parseMedia(parser, token, context);
            } else {
               hll_ignoreStatement(parser, token);
            }
         } else {
            hll_ignoreStatement(parser, token);
         }
      } else {
         importsAreAllowed = false;
         hll_cssParseRuleset(parser, token, context);
      }
   }
#endif
}
-}
parseCss :: ((CssParser, CssToken), CssContext) -> ((CssParser, CssToken), CssContext)
parseCss ((parser, token), context) =
  case token of
    -- TODO: check whether string comparison of "import" or "media" should be
    -- case-sensitive or not.
    -- TODO: handle CssTokAtKeyword tokens with values other than "media"/"import".
    CssTokAtKeyword "import" -> parseCss . parseImportRule $ ((parser, token), context)
    CssTokAtKeyword "media"  -> parseCss . parseMediaRule $ ((parser, token), context)
    CssTokNone               -> parseCss (nextToken2 parser, context) -- Kick-start parsing of tokens stream.
    CssTokEnd                -> ((parser, token), context)
    _                        -> parseCss . parseRuleset $ ((parser, token), context) -- TODO: set flag "let importsAreAllowed = False"




-- TODO: reimplement "void parseImport(DilloHtml *html, c_css_parser_t * parser, c_css_token_t * token, const DilloUrl * base_url)"
parseImportRule :: ((CssParser, CssToken), CssContext) -> ((CssParser, CssToken), CssContext)
parseImportRule ((parser, token), context) = trace ("[DD] @import detected") (ignoreStatement parser, context)




parseMediaRule :: ((CssParser, CssToken), CssContext) -> ((CssParser, CssToken), CssContext)
parseMediaRule ((parser, token), context) = ((p3, t3), c3)
  where
    ((p2, _t2), media) = parseMediaQuery (parser, token)
    (_syntaxOk, mediaMatch) = case media of
                                Just m  -> (True, mediaMatchesParser parser m)
                                Nothing -> (False, False)

    ((p3, t3), c3) = if mediaMatch
                     then parseMediaBlock (nextToken1 p2, context) -- nextToken skips opening brace of a block
                     else (ignoreBlock p2, context)




parseMediaBlock ((parser, token), context) = case parseRuleset ((parser, token), context) of
                                               ((p2, CssTokEnd), c2)             -> ((p2, CssTokEnd), c2)
                                               ((p2, CssTokBraceCurlyClose), c2) -> (nextToken1 p2, c2) -- Consume closing brace of media block
                                               ((p2, t2), c2)                    -> parseRuleset ((p2, t2), c2)



{-
         nextToken(parser, token);
         if (token->c_type == CSS_TOKEN_TYPE_IDENT) {
            if (dStrAsciiCasecmp(token->c_value, "import") == 0 &&
                html != NULL &&
                importsAreAllowed) {
               parseImport(html, parser, token, parser_.m_base_url);
            } else if (dStrAsciiCasecmp(token->c_value, "media") == 0) {
               parseMedia(parser, token, context);
            } else {
               hll_ignoreStatement(parser, token);
            }
         } else {
            hll_ignoreStatement(parser, token);
         }
-}


