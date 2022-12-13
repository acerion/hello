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

This file is derived from dillo-3.0.5/src/cssparser.cc.
Copyright assignments from that file:
Copyright 2004 Sebastian Geerken <sgeerken@dillo.org>
Copyright 2008-2009 Johannes Hofmann <Johannes.Hofmann@gmx.de>
Additional note in cssparser.cc:
"This file is heavily based on the CSS parser of dillo-0.8.0-css-3 -
a dillo1 based CSS prototype written by Sebastian Geerken."
-}




{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}




module Hello.Css.Parser.Rule
  (
    ignoreBlock
  , consumeBlock
  , ignoreStatement

  , declValueAsURI

  , CssCombinator (..)

  , parseDeclarationWrapper
  , takePropertyNameToken

  , parseElementStyleAttribute
  , parseAllDeclarations
  , parseSingleDeclaration

  , declarationsSetUpdateOrAdd
  , declarationsSetAppend
  , CssDeclarationSet (..)
  , defaultCssDeclarationSet

  , CssRule (..)
  , parseStyleRule

  , getTopCompound
  )
where




import Data.Data (toConstr)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Sequence as S
--import Debug.Trace

import Hello.Chain
import Hello.Css.Declaration
import Hello.Css.Parser.Selector
import Hello.Css.ParserHelpers
import Hello.Css.Tokenizer
import Hello.Css.Selector
import Hello.Css.Value
import Hello.Utils.Parser




-- Mapping between name of non-shorthand property and a constructor of the
-- property.
--
-- Only a subset of CSS2.2 properties is supported by this implementation.
cssPropertyCtors = M.fromList [
     ("background-attachment",  makeCssPropertyBackgroundAttachment)
   , ("background-color",       makeCssPropertyBackgroundColor)
   , ("background-image",       makeCssPropertyBackgroundImage)
   , ("background-position",    makeCssPropertyBackgroundPosition)
   , ("background-repeat",      makeCssPropertyBackgroundRepeat)

   , ("border-collapse",        makeCssPropertyBorderCollapse)
   , ("border-spacing",         makeCssPropertyBorderSpacing)

   , ("border-top",             makeCssPropertyBorderTop)
   , ("border-right",           makeCssPropertyBorderRight)
   , ("border-bottom",          makeCssPropertyBorderBottom)
   , ("border-left",            makeCssPropertyBorderLeft)

   , ("border-color",           ctorCssPropertyBorderColor)
   , ("border-style",           ctorCssPropertyBorderStyle)
   , ("border-width",           ctorCssPropertyBorderWidth)

   , ("border-top-color",       makeCssPropertyBorderTopColor)
   , ("border-right-color",     makeCssPropertyBorderRightColor)
   , ("border-bottom-color",    makeCssPropertyBorderBottomColor)
   , ("border-left-color",      makeCssPropertyBorderLeftColor)

   , ("border-top-style",       makeCssPropertyBorderTopStyle)
   , ("border-right-style",     makeCssPropertyBorderRightStyle)
   , ("border-bottom-style",    makeCssPropertyBorderBottomStyle)
   , ("border-left-style",      makeCssPropertyBorderLeftStyle)

   , ("border-top-width",       makeCssPropertyBorderTopWidth)
   , ("border-right-width",     makeCssPropertyBorderRightWidth)
   , ("border-bottom-width",    makeCssPropertyBorderBottomWidth)
   , ("border-left-width",      makeCssPropertyBorderLeftWidth)

   --, ("bottom",                 Nothing)
   --, ("caption-side",           Nothing)
   --, ("clear",                  Nothing)
   --, ("clip",                   Nothing)
   , ("color",                  makeCssPropertyColor)
   , ("content",                makeCssPropertyContent)
   --, ("counter-increment",      Nothing)
   --, ("counter-reset",          Nothing)
   , ("cursor",                 makeCssPropertyCursor)
   --, ("direction",              Nothing)
   , ("display",                makeCssPropertyDisplay)
   --, ("empty-cells",            Nothing)
   --, ("float",                  Nothing)
   , ("font-family",            makeCssPropertyFontFamily)
   , ("font-size",              makeCssPropertyFontSize)
   --, ("font-size-adjust",       Nothing)
   --, ("font-stretch",           Nothing)
   , ("font-style",             makeCssPropertyFontStyle)
   , ("font-variant",           makeCssPropertyFontVariant)
   , ("font-weight",            makeCssPropertyFontWeight)
   , ("height",                 makeCssPropertyHeight)
   --, ("left",                   Nothing)
   , ("letter-spacing",         makeCssPropertyLetterSpacing)
   , ("line-height",            makeCssPropertyLineHeight)

   , ("list-style",             ctorCssPropertyListStyle)
   , ("list-style-image",       ctorCssPropertyListStyleImage)
   , ("list-style-position",    ctorCssPropertyListStylePosition)
   , ("list-style-type",        ctorCssPropertyListStyleType)

   , ("margin",                 makeCssPropertyMargin)
   , ("margin-top",             makeCssPropertyMarginTop)
   , ("margin-right",           makeCssPropertyMarginRight)
   , ("margin-bottom",          makeCssPropertyMarginBottom)
   , ("margin-left",            makeCssPropertyMarginLeft)

   --, ("marker-offset",          Nothing)
   --, ("marks",                  Nothing)
   --, ("max-height",             Nothing)
   --, ("max-width",              Nothing)
   --, ("min-height",             Nothing)
   --, ("min-width",              Nothing)
   --, ("outline-color",          Nothing)
   --, ("outline-style",          Nothing)
   --, ("outline-width",          Nothing)
   --, ("overflow",               Nothing)
   , ("padding",                makeCssPropertyPadding)
   , ("padding-bottom",         makeCssPropertyPaddingBottom)
   , ("padding-left",           makeCssPropertyPaddingLeft)
   , ("padding-right",          makeCssPropertyPaddingRight)
   , ("padding-top",            makeCssPropertyPaddingTop)
   --, ("position",               Nothing)
   --, ("quotes",                 Nothing)
   --, ("right",                  Nothing)
   , ("text-align",             makeCssPropertyTextAlign)
   , ("text-decoration",        makeCssPropertyTextDecoration)
   , ("text-indent",            makeCssPropertyTextIndent)
   --, ("text-shadow",            Nothing)
   , ("text-transform",         makeCssPropertyTextTransform)
   --, ("top",                    Nothing)
   --, ("unicode-bidi",           Nothing)
   , ("vertical-align",         makeCssPropertyVerticalAlign)
   --, ("visibility",             Nothing)
   , ("white-space",            makeCssPropertyWhitespace)
   , ("width",                  makeCssPropertyWidth)
   , ("word-spacing",           makeCssPropertyWordSpacing)
   --, ("z-index",                Nothing)
   ] :: M.Map T.Text PropertyCtor




-- Mapping between name of shorthand property and a constructor of the
-- property.
--
-- Only a subset of CSS2.2 properties is supported by this implementation.
cssShorthandInfo :: M.Map T.Text ShorthandPropertyCtor
cssShorthandInfo = M.fromList [
    ("background",         makeCssPropertyBackground)

    -- Parsing of this property is unit-tested.
  , ("border",             makeCssPropertyBorder)


  , ("font",               makeCssPropertyFont)
  ]




-- Use name of property to look up a constructor used to parse the property
-- and the property's value.
--
-- TODO: case-insensitive search?
getPropertyCtorByName :: T.Text -> Maybe PropertyCtor
getPropertyCtorByName propertyName = M.lookup propertyName cssPropertyCtors




-- Use name of shorthand property to look up a constructor used to parse the
-- shorthand property and the property's value.
--
-- TODO: case-insensitive search?
getShorthandCtorByName :: T.Text -> Maybe ShorthandPropertyCtor
getShorthandCtorByName shorthandName = M.lookup shorthandName cssShorthandInfo




{-
-- TODO: move getting leading minus to takeNumber. Have a clearer distinction
-- between minus being a part of a number and minus being a part of other
-- type of token. Make sure that this call: "nextToken . defaultParser $ "/* hello */ -"}"
-- returns token type == TokenChar.
-- Or should it be treated as invalid?
takeLeadingMinus :: CssParser -> (CssParser, CssToken)
takeLeadingMinus parser = case T.uncons (remainder parser) of
                            Just (c, rem) | c == '-'  -> (parser{remainder = rem}, CssToken (T.singleton c) Nothing)
                                          | otherwise -> (parser, CssTokNone)
                            Nothing -> (parser, CssTokNone)
-}



{-
declValueAsString :: Int -> (CssParser, CssToken) -> ((CssParser, CssToken), Maybe T.Text)
declValueAsString propId (parser, token) = case ((retParser, retToken), value) of
                                             ((p, t), Just (CssValueTypeString s)) -> ((p, t), Just s)
                                             ((p, t), Just (CssValueTypeURI s))    -> ((p, t), Just s)
                                             ((p, t), Nothing)                     -> ((p, t), Nothing)
  where
    ((retParser, retToken), value) | propId == 10  = tokensAsValueString (parser, token) -- TODO: magic value
                                   | propId == 12  = declValueAsURI (parser, token)      -- TODO: magic value
                                   | otherwise     = ((parser, token), Nothing)




-- Interpret current token as "string" value (value of type CssValueTypeString).
--
-- In case of "string" value there is no need to consume more than current
-- token to build the String, but for consistency with other similar
-- functions the function is still called "tokensAs...".
tokensAsValueString :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssValue)
tokensAsValueString (p, CssTokStr s) = (nextToken p, Just (CssValueTypeString s))
tokensAsValueString (p, t)           = ((p, t), Nothing)
-}



declValueAsURI :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssValue)
declValueAsURI (parser, token) = case parseUrl (parser, token) of
                                   ((newParser, newToken), Just url) -> ((newParser, newToken), Just (CssValueTypeURI url))
                                   ((newParser, newToken), Nothing)  -> ((newParser, newToken), Nothing)




{-
tokenMatchesProperty :: CssToken -> CssPropertyInfo -> Maybe CssValueType
tokenMatchesProperty token propInfo = tokenMatchesProperty' token acceptedValueTypes enums
  where
    acceptedValueTypes = tripletSnd propInfo
    enums = tripletThrd propInfo

    tokenMatchesProperty' :: CssToken -> [CssValueType] -> [T.Text] -> Maybe CssValueType
    tokenMatchesProperty' token (t:ts) enums
                                             | t == CssValueTypeBgPosition =
                                                 case token of
                                                   CssTokIdent s -> if s == "center" || s == "left" || s == "right" || s == "top" || s == "bottom"
                                                                  then Just t
                                                                  else tokenMatchesProperty' token ts enums
                                                   CssTokNum n -> Just t   -- TODO: here we should better handle numeric background positions
                                                   _           -> tokenMatchesProperty' token ts enums

                                             | t == CssValueTypeLengthPercent || t == CssValueTypeLength || t == CssValueTypeLengthPercentNumber =
                                                 case token of
                                                   CssTokNum (CssNumF f)   -> if f < 0 then Nothing else Just t
                                                   CssTokNum (CssNumI i)   -> if i < 0 then Nothing else Just t
                                                   CssTokPerc (CssNumF f)  -> if f < 0 then Nothing else Just t
                                                   CssTokPerc (CssNumI i)  -> if i < 0 then Nothing else Just t
                                                   CssTokDim (CssNumF f) _ -> if f < 0 then Nothing else Just t
                                                   CssTokDim (CssNumI i) _ -> if i < 0 then Nothing else Just t
                                                   _              -> Nothing

                                             | t == CssValueTypeSignedLength =
                                                 case token of
                                                   CssTokNum _   -> Just t
                                                   CssTokPerc _  -> Just t
                                                   CssTokDim _ _ -> Just t
                                                   _             -> tokenMatchesProperty' token ts enums

                                             | t == CssValueTypeFontWeight =
                                                 case token of
                                                   CssTokNum (CssNumI i) -> if i >= 100 && i <= 900 -- TODO: this test of range is repeated in this file
                                                                            then Just t
                                                                            else tokenMatchesProperty' token ts enums
                                                   _                     -> tokenMatchesProperty' token ts enums
                                             | t == CssValueTypeURI =
                                                 case token of
                                                   CssTokIdent s -> if s == "url"
                                                                  then Just t
                                                                  else tokenMatchesProperty' token ts enums
                                                   _           -> tokenMatchesProperty' token ts enums
-}




-- TODO: rewrite with consumeBlock?
ignoreBlock :: CssParser -> (CssParser, CssToken)
ignoreBlock parser = ignoreBlock' (parser, CssTokNone) 0
  where
    ignoreBlock' :: (CssParser, CssToken) -> Int -> (CssParser, CssToken)
    ignoreBlock' (par, tok@CssTokEnd) _             = (par, tok)
    ignoreBlock' (par, CssTokBraceCurlyOpen) depth  = ignoreBlock' (nextToken par) (depth + 1)
    ignoreBlock' (par, CssTokBraceCurlyClose) depth = if depth == 1
                                                      then nextToken par
                                                      else ignoreBlock' (nextToken par) (depth - 1)
    ignoreBlock' (par, _tok) depth                  = ignoreBlock' (nextToken par) depth
{-
   while (tokenizer->type != CSS_TOKEN_TYPE_END) {
      if (tokenizer->type == CSS_TOKEN_TYPE_CHAR) {
         if (tokenizer->value[0] == '{') {
            depth++;
         } else if (tokenizer->value[0] == '}') {
            depth--;
            if (depth == 0) {
               nextToken(tokenizer, hll_css_parser);
               return;
            }
         }
      }
      nextToken(tokenizer, hll_css_parser);
   }
-}




-- TODO: this function can recognize only blocks enclosed by curly braces.
-- Make the function recognize all types of Css braces.
consumeBlock :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssToken])
consumeBlock pat = consumeBlock' pat [] []
  where
    -- Last argument (braces) is used to keep track of opened/closed braces
    -- to know what is the current nesting level of blocks.
    consumeBlock' (parser, tok@CssTokEnd) tokens _                                   = ((parser, tok), reverse tokens)
    consumeBlock' (parser, CssTokBraceCurlyOpen) tokens braces                       = consumeBlock' (nextToken parser) (CssTokBraceCurlyOpen : tokens) (CssTokBraceCurlyOpen : braces)
    consumeBlock' (parser, CssTokBraceCurlyClose) tokens (CssTokBraceCurlyOpen : []) = (nextToken parser, reverse tokens)
    consumeBlock' (parser, CssTokBraceCurlyClose) tokens (CssTokBraceCurlyOpen : xs) = consumeBlock' (nextToken parser) (CssTokBraceCurlyClose : tokens) xs
    consumeBlock' (parser, tok) tokens braces                                        = consumeBlock' (nextToken parser) (tok : tokens) braces




ignoreStatement :: CssParser -> (CssParser, CssToken)
ignoreStatement parser = ignoreStatement' (parser, CssTokNone)
  where
    ignoreStatement' (par, tok@CssTokEnd)        = (par, tok)
    ignoreStatement' (par, CssTokSemicolon)      = nextToken par
    ignoreStatement' (par, CssTokBraceCurlyOpen) = ignoreBlock par
    ignoreStatement' (par, _)                    = ignoreStatement' (nextToken par)
{-
   while (tokenizer->type != CSS_TOKEN_TYPE_END) {
      if (tokenizer->type == CSS_TOKEN_TYPE_CHAR) {
         if (tokenizer->value[0] == ';') {
            nextToken(tokenizer, hll_css_parser);
            return;
         } else if (tokenizer->value[0] =='{') {
            ignoreBlock(tokenizer, hll_css_parser);
            return;
         }
      }
      nextToken(tokenizer, hll_css_parser);
   }
-}




-- https://www.w3.org/TR/CSS22/cascade.html#important-rules
-- https://www.w3.org/TR/css-cascade-5/#importance
--
-- https://www.w3.org/TR/css-syntax-3/#consume-declaration: "If the last two
-- non-<whitespace-token>s in the declaration’s value are a <delim-token>
-- with the value "!" followed by an <ident-token> with a value that is an
-- ASCII case-insensitive match for "important", remove them from the
-- declaration’s value and set the declaration’s important flag to true."
cssParseImportance :: (CssParser, CssToken) -> ((CssParser, CssToken), Bool)
cssParseImportance (parser, CssTokDelim '!') = case nextToken parser of
                                                 (newParser, CssTokIdent "important") -> (nextToken newParser, True)
                                                 (newParser, tok)                     -> ((newParser, tok), False)
cssParseImportance (parser, tok)             = ((parser, tok), False)








-- The isSafe flag compilcates this data type. I have to declare a new "Set"
-- type that is a wrapper around list of declarations + that one boolean
-- flag.
data CssDeclarationSet = CssDeclarationSet
  { isSafe :: Bool
  , items  :: S.Seq CssDeclaration
  } deriving (Show, Eq)


defaultCssDeclarationSet = CssDeclarationSet
  { isSafe = True
  , items  = S.fromList []
  }




parseDeclarationNormal :: (CssParser, CssToken) -> PropertyCtor -> ((CssParser, CssToken), [CssDeclaration])
parseDeclarationNormal pat propCtor = case propCtor pat of
                                        Just (pat', prop) -> (pat', [defaultDeclaration{property = prop}])
                                        Nothing           -> (pat, [])




parseDeclarationShorthand :: (CssParser, CssToken) -> ShorthandPropertyCtor -> ((CssParser, CssToken), [CssDeclaration])
parseDeclarationShorthand pat ctor = (pat', decls)
  where
    (pat', properties) = ctor pat
    decls = fmap (\x -> defaultDeclaration { property = x }) properties




-- The input to the function is (parser { rem = ": value" }, TokIdent
-- "name"). The function confirms that current token is an ident, that it is
-- followed by colon name, and returns updated parser + token with CSS
-- property's name. Colon token is discarded.
--
-- :m +Hello.Css.Parser.Rule
-- :m +Hello.Css.Tokenizer
-- takePropertyNameToken (defaultParserInBlock ": value", CssTokIdent "name")
takePropertyNameToken :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssToken)
takePropertyNameToken state = runParser (getIdentToken <* getColonToken) state




getIdentToken :: Parser (CssParser, CssToken) CssToken
getIdentToken = Parser $ \ (parser, token) -> case token of
                                                CssTokIdent _ -> Just ((nextToken parser), token)
                                                _             -> Nothing




getColonToken :: Parser (CssParser, CssToken) CssToken
getColonToken = Parser $ \ (parser, token) -> case token of
                                                CssTokColon -> Just ((nextToken parser), token)
                                                _           -> Nothing




-- The function returns a list of declarations because a line in CSS with a
-- shorthand declaration will be translated to N corresponding "normal"
-- declarations. E.g. "border-color: red" shorthand will be translated into a
-- list of "normal" declarations that will look like this:
-- ["border-top-color: red"; "border-right-color: red"; "border-bottom-color: red"; "border-left-color: red"]
parseSingleDeclarationNormalOrShorthand :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssDeclaration])
parseSingleDeclarationNormalOrShorthand pat = case takePropertyNameToken pat of
                                                -- HASKELL FEATURE: pattern guards
                                                Just (pat', (CssTokIdent sym)) | Just ctor <- getPropertyCtorByName sym  -> parseDeclarationNormal pat' ctor
                                                                               | Just ctor <- getShorthandCtorByName sym -> parseDeclarationShorthand pat' ctor
                                                                               | otherwise                               -> (pat', [])
                                                _ -> (pat, [])




-- For non-shorthand declaration, this function should produce one-element
-- list. But a shorthand declaration translates into two or more regular
-- declarations, hence the return type contains a list of declarations.
parseSingleDeclaration :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssDeclaration])
parseSingleDeclaration (p1, t1) = ((outParser, outToken), declarationsWithImportant)
  where
    ((p2, t2), declarations) = parseSingleDeclarationNormalOrShorthand (p1, t1)
    ((p3, t3), isImportant) = cssParseImportance (p2, t2)
    declarationsWithImportant = if isImportant
                                then markAsImportant <$> declarations
                                else declarations
    (outParser, outToken) = consumeRestOfDeclaration (p3, t3)

    markAsImportant inDecl = inDecl{important = True}




parseDeclarationWrapper :: (CssParser, CssToken) -> CssDeclarationSets -> ((CssParser, CssToken), CssDeclarationSets)
parseDeclarationWrapper pat (inSet, inSetImp) = (pat', (outSet, outSetImp))
  where
    (pat', declarations) = parseSingleDeclaration pat
    (outSet, outSetImp)  = appendDeclarations declarations inSet inSetImp

    appendDeclarations :: [CssDeclaration] -> CssDeclarationSet -> CssDeclarationSet -> CssDeclarationSets
    appendDeclarations [] set setImp     = (set, setImp)
    appendDeclarations (d:ds) set setImp = if important d
                                           then appendDeclarations ds set (declarationsSetUpdateOrAdd setImp d)
                                           else appendDeclarations ds (declarationsSetUpdateOrAdd set d) setImp




-- Find end of current declaration (probably needed only if something goes
-- wrong during parsign of current declaration).
consumeRestOfDeclaration pair@(_, CssTokEnd)             = pair
consumeRestOfDeclaration pair@(_, CssTokBraceCurlyClose) = pair -- '}' is not a part of declaration, so don't go past it. Return '}' as current token.
consumeRestOfDeclaration (parser, CssTokSemicolon)       = nextToken parser
consumeRestOfDeclaration (parser, _)                     = consumeRestOfDeclaration . nextToken $ parser




{-
takeAllTokens :: (CssParser, CssToken) -> IO CssParser
takeAllTokens (parser,token) = do
  T.IO.putStrLn (remainder parser)
  let (p, t) = nextToken parser
  if cssTokenType t == CssTokEnd
    then return p
    else takeAllTokens . nextToken $ p
-}




declarationsSetUpdateOrAdd :: CssDeclarationSet -> CssDeclaration -> CssDeclarationSet
declarationsSetUpdateOrAdd declSet decl =
  case S.findIndexL predicate ix of
    Just idx -> CssDeclarationSet {items = S.update idx decl ix, isSafe = newSafe declSet decl}
    Nothing  -> CssDeclarationSet {items = ix S.|> decl,         isSafe = newSafe declSet decl}
  where
    -- Use 'toConstr' to compare constructors, but values without passed to constructors.
    -- https://stackoverflow.com/questions/47861648/a-general-way-of-comparing-constructors-of-two-terms-in-haskell
    predicate :: CssDeclaration -> Bool
    predicate x = (toConstr . property $ x) == (toConstr . property $ decl)

    ix = items declSet

    newSafe :: CssDeclarationSet -> CssDeclaration -> Bool
    newSafe declSet' decl' = isSafe declSet' && case property decl' of
                                                  CssPropertyDisplay _         -> False
                                                  CssPropertyBackgroundImage _ -> False
                                                  _                            -> True




{-
Merge values from incoming into target, return result of merging

I can't use a concatenation operator because the merging is not that
simple: it has to use declarationsSetUpdateOrAdd function.
-}
declarationsSetAppend :: CssDeclarationSet -> CssDeclarationSet -> CssDeclarationSet
declarationsSetAppend target incoming = if S.null . items $ incoming
                                        then target
                                        else declarationsSetAppend (declarationsSetUpdateOrAdd target iHead) iTail
  where
    iHead = S.index (items incoming) 0
    iTail = incoming {items = S.drop 1 (items incoming)}




{-
Parse CSS style information contained in "cssStyleAttribute". The buffer
contains value of "style" attribute of a html element.

import qualified Data.Sequence as S
let declSet    = CssDeclarationSet {isSafe = True, items = S.fromList []}
let declSetImp = CssDeclarationSet {isSafe = True, items = S.fromList []}
let cssStyleAttribute = "color: red !important; font-weight: bold"
parseElementStyleAttribute "" cssStyleAttribute (declSet, declSetImp)
-}
parseElementStyleAttribute :: T.Text -> T.Text -> CssDeclarationSets -> CssDeclarationSets
parseElementStyleAttribute _baseUrl cssStyleAttribute declSets = declSets'
  where
    ((_p2, _t2), declSets') = parseAllDeclarations ((p1, t1), declSets)
    (p1, t1) = nextToken parser -- Kick-off the parsing

    {-
      TODO: in original C++ code the parser was initialized like this:
      CssParser parser(NULL,              -- c_css_context_t object
                       CSS_ORIGIN_AUTHOR, -- CssOrigin enum
                       baseUrl, cssStyleAttribute, buflen);
      Be sure to recreate this in final Haskell code.
    -}
    parser = CssParser{ remainder      = cssStyleAttribute
                      , spaceSeparated = False
                      , inBlock        = True -- There is no block enclosed in {}. but parser needs to behave as if we were in the block.
                      , bufOffset      = 0
                      , cssOrigin      = CssOriginAuthor
                      }


type CssDeclarationSets = (CssDeclarationSet, CssDeclarationSet)



parseAllDeclarations :: ((CssParser, CssToken), CssDeclarationSets) -> ((CssParser, CssToken), CssDeclarationSets)
parseAllDeclarations ((p1, t1), declSets) | t1 == CssTokEnd             = ((p1, t1), declSets)
                                          | t1 == CssTokBraceCurlyClose = ((p1, t1), declSets)
                                          | otherwise = parseAllDeclarations (parseDeclarationWrapper (p1, t1) declSets)




data CssRule = CssRule {
    complexSelector :: CssCachedComplexSelector
  , declarationSet  :: CssDeclarationSet
  , specificity     :: Int
  , position        :: Int
  } deriving (Eq)


instance Show CssRule where
  show (CssRule cs ds s p) = "Rule {" ++  show cs ++ "\n" ++
                                          show ds ++ "\n" ++
                             "spec = " ++ show s  ++ "\n" ++
                             "pos = "  ++ show p  ++ "}\n"


-- Get top compound selector
getTopCompound :: CssRule -> CssCompoundSelector
getTopCompound rule = chainGetFirstDatum . chain . complexSelector $ rule




-- https://www.w3.org/TR/css-syntax-3/#style-rules
-- https://www.w3.org/TR/css-syntax-3/#qualified-rule
-- https://www.w3.org/TR/CSS22/syndata.html#rule-sets
--
-- Parse a style rule. On success get the ingredients of the rule:
--   a list of complex selectors from prelude of the rule (a <selector-list>)
--   a list of property declarations (actually two lists: normal declarations and important declarations)
--
-- https://www.w3.org/TR/css-syntax-3/#style-rules says that invalid selector
-- list invalidates entire style rule. But a single invalid property doesn't
-- invalidate the entire rule.
--
-- Unit-tested: yes
parseStyleRule :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe ([CssCachedComplexSelector], CssDeclarationSet, CssDeclarationSet))
parseStyleRule pat = case readSelectorList pat of
                       (pat', Nothing)           -> (pat', Nothing)
                       (pat', Just selectorList) -> case readDeclarations pat' of
                                                      (pat'', Just (declSet, declSetImp)) -> (pat'', Just (selectorList, declSet, declSetImp))
                                                      (pat'', Nothing)                    -> (pat'', Nothing)




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


readDeclarations :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclarationSets)
readDeclarations (parser, token) = ((p3, t3), Just declSets)
  where
    ((p2, t2), declSets) = case token of
                             CssTokEnd -> ((parser, token), declSets)
                             _         -> readDeclarations' (nextToken parser{ inBlock = True }, (defaultCssDeclarationSet, defaultCssDeclarationSet))
    (p3, t3) = case t2 of
                 CssTokBraceCurlyClose -> nextToken p2{ inBlock = False }
                 _                     -> (p2{ inBlock = False }, t2)




readDeclarations' ((parser, token), declSets) =
  case token of
    CssTokEnd             -> ((parser, token), declSets)
    CssTokBraceCurlyClose -> ((parser, token), declSets) -- TODO: this should be (nextToken parser)
                             -- instead of (parser, token): ensure that '}' that is part of "declartions" block
                             -- is handled and consumed, so that the next part of code doesn't have to handle it.
    _                     -> readDeclarations' (parseDeclarationWrapper (parser, token) declSets)
