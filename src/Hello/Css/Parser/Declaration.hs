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




module Hello.Css.Parser.Declaration
  (
    ignoreBlock
  , consumeBlock
  , ignoreStatement

  , declValueAsURI

  , CssCombinator (..)

  , parseSingleDeclarationWrapper
  , takePropertyName

  , parseElementStyleAttribute
  , parseAllDeclarations
  , readDeclarations
  , parseSingleDeclaration

  , declarationsSetUpdateOrAdd
  , declarationsSetAppend
  , CssDeclarationSet (..)
  , CssDeclarationSets
  , defaultCssDeclarationSet

  , CssRule (..)
  , CssParsedStyleRule (..)
  , parseStyleRule

  , getTopCompound

  , parseImportantPresent
  , parseImportantNotPresent
  , parseImportant
  , parserImportant

  , readDeclarationsBlock
  , readDeclarationsBlockWithError

  -- Exported only for tests
  , parseProperty
  )
where



import Control.Applicative (Alternative(..))
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




-- Mapping between name of property and a constructor of the property.
--
-- Only a subset of CSS2.2 properties is supported by this implementation.
cssPropertyCtors = M.fromList [
     ("background",             ctorCssPropertyBackground)
   , ("background-attachment",  makeCssPropertyBackgroundAttachment)
   , ("background-color",       makeCssPropertyBackgroundColor)
   , ("background-image",       makeCssPropertyBackgroundImage)
   , ("background-position",    makeCssPropertyBackgroundPosition)
   , ("background-repeat",      makeCssPropertyBackgroundRepeat)

   , ("border",                 ctorCssPropertyBorder)

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

   , ("font",                   ctorCssPropertyFont)
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





-- Use name of property to look up a constructor used to parse the property
-- and the property's value.
--
-- TODO: case-insensitive search?
getPropertyCtorByName :: T.Text -> Maybe PropertyCtor
getPropertyCtorByName propertyName = M.lookup propertyName cssPropertyCtors




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




-- See if last two non-white tokens in value string are "!important". This
-- requires looking at tokens that come after the "!important", and seeing if
-- they are valid post-value tokens. The post-value tokens should form a
-- valid end of declaration.
--
-- This function succeeds if the "!important" was found and value string is
-- terminated properly. Returned parser points to the end of "!important".
--
-- TODO: the backtracking in the function brings inefficiency.
--
-- many: zero or more
-- https://hackage.haskell.org/package/base-4.17.0.0/docs/Control-Applicative.html#v:many
parseImportantPresent :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssToken)
parseImportantPresent pat = case runParser parser pat of
                              -- Backtrack: take just "!important" and leave rest to next parser.
                              Just (_, _) -> runParser parserImportant pat
                              Nothing     -> Nothing
  where
    parser = parserImportant
             *> many parserTokenWhitespace
             *> (parserTokenSemicolon <|> parserTokenBraceCurlyClose <|> parserTokenEnd)




-- TODO: a railroad diagram for 'important' suggests that whitespaces are
-- allowed between these two tokens.
--
-- https://www.w3.org/TR/css-syntax-3/#!important-diagram
parserImportant :: Parser (CssParser, CssToken) CssToken
parserImportant = (parserTokenDelim '!') *> (parserTokenIdent "important")




-- See if last two non-white tokens in value string are "!important". This
-- requires looking at current token and following tokens, and seeing if they
-- are valid post-value tokens. The post-value tokens should form a valid end
-- of declaration. Since this function doesn't expect to find "!important",
-- then it only expects valid post-value tokens.
--
-- This function succeeds if the "!important" was NOT found and value string
-- is terminated properly. Returned parser points to the end of value string.
--
-- TODO: the backtracking in the function brings inefficiency.
parseImportantNotPresent ::  (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssToken)
parseImportantNotPresent pat@(_, t) = case runParser parser pat of
                                        -- Backtrack: take just spaces and leave rest to next parser.
                                        --
                                        -- TODO: returning the same token in
                                        -- 'pat' and in second may cause
                                        -- problems if caller code will
                                        -- attempt to actually use result of
                                        -- the function.
                                        Just (_, _) -> Just (pat, t)
                                        Nothing     -> Nothing
  where parser = (many parserTokenWhitespace
                   *> (parserTokenSemicolon
                        <|> parserTokenBraceCurlyClose
                        <|> parserTokenEnd))





-- Parse "!important" string that is at the end of value. The "!important"
-- string may or may not be present - both cases are valid.
--
-- https://www.w3.org/TR/CSS22/cascade.html#important-rules
-- https://www.w3.org/TR/css-cascade-5/#importance
--
-- https://www.w3.org/TR/css-syntax-3/#consume-declaration: "If the last two
-- non-<whitespace-token>s in the declaration’s value are a <delim-token>
-- with the value "!" followed by an <ident-token> with a value that is an
-- ASCII case-insensitive match for "important", remove them from the
-- declaration’s value and set the declaration’s important flag to true."
--
-- :m +Hello.Css.Parser.Declaration
-- :m +Hello.Css.Tokenizer
-- :m +Hello.Css.Declaration
-- parseImportant ((nextToken . defaultParser $ "!important;"), defaultDeclaration)
-- parseImportant ((nextToken . defaultParser $ "!important }"), defaultDeclaration)
parseImportant :: ((CssParser, CssToken), CssDeclaration) -> Maybe ((CssParser, CssToken), CssDeclaration)
parseImportant (pat, decl) | Just (pat', _) <- parseImportantNotPresent pat = Just (pat', decl)
                           | Just (pat', _) <- parseImportantPresent pat    = Just (pat', decl { important = True })
                           | otherwise                                      = Nothing -- Bad termination of property's value string.





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




-- The input to the function is (parser { rem = ": value" }, TokIdent
-- "name"). The function confirms that current token is an ident, that it is
-- followed by colon name, and returns updated parser + the property's name.
--
-- :m +Hello.Css.Parser.Declaration
-- :m +Hello.Css.Tokenizer
-- takePropertyName (defaultParserInBlock ": value", CssTokIdent "name")
takePropertyName :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), T.Text)
takePropertyName state = case runParser (parserTokenIdentAny <* parserTokenColon) state of
                           Just (state', CssTokIdent name) -> Just (state', name)
                           _                               -> Nothing




-- Parse CSS property into a declaration:
-- "color: rgb(0, 100, 0)" -> CssDeclaration { property = CssPropertyColor <value>, important = False }
--
-- In other words: set 'property' field in returned CssDeclaration.
-- This function doesn't parse "!important".
--
-- The input CssDeclaration is ignored.
--
-- The layout of the code is probably BAD, and probably a 'do' notation is
-- more appropriate here, but I like it this way. And it's way better than
-- the previous version.
parseProperty :: ((CssParser, CssToken), CssDeclaration) -> Maybe ((CssParser, CssToken), CssDeclaration)
parseProperty (pat, _) = takePropertyName pat
                         -- HASKELL FEATURE: BIND
                         >>= (\ (pat', name) -> getPropertyCtorByName name
                               >>= (\ propertyCtor -> propertyCtor pat'
                                     >>= (\ (pat'', prop) -> Just (pat'', defaultDeclaration { property = prop }))))





-- Consume and parse a declaration, from name of property unitl and including
-- white spaces after value and after potential "!important". Per CSS3 syntax
-- the semincolon(s) or '}' brace after that are not touched.
--
-- https://www.w3.org/TR/css-syntax-3/#consume-declaration
-- https://www.w3.org/TR/css-syntax-3/#parse-declaration
parseSingleDeclaration :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclaration)
parseSingleDeclaration pat =
  -- HASKELL FEATURE: BIND
  case parseDeclarationStart (pat, defaultDeclaration) >>= parseProperty >>= parseImportant >>= parseDeclarationEnd of
    Just (pat', declaration) -> (pat', Just declaration)
    Nothing                  -> (seekEndOfDeclaration pat, Nothing)




parseSingleDeclarationWrapper :: ((CssParser, CssToken), CssDeclarationSets) -> ((CssParser, CssToken), CssDeclarationSets)
parseSingleDeclarationWrapper (pat, declSets) = (pat', declSets')
  where
    (pat', declaration) = parseSingleDeclaration pat
    declSets' = appendDeclaration declaration declSets

    appendDeclaration :: Maybe CssDeclaration -> CssDeclarationSets -> CssDeclarationSets
    appendDeclaration Nothing sets = sets
    appendDeclaration (Just d) (set, setImp) = if important d
                                               then (set, (declarationsSetUpdateOrAdd setImp d))
                                               else ((declarationsSetUpdateOrAdd set d), setImp)





-- Consume spaces AND semicolonS from beginning of declaration.
--
-- https://www.w3.org/TR/css-syntax-3/#consume-style-block
-- This place instructs to "do nothing" with initial whitespaces and semicolons. Just ignore them.
--
-- Also https://www.w3.org/TR/css-syntax-3/#consume-list-of-declarations
-- treats initial whitespaces as semicolons as something to be ignored.
--
-- A semicolon isn't treaded by spec as part of a declaration, but as a separator between declarations:
-- https://www.w3.org/TR/css-syntax-3/#syntax-description: "Declarations are separated by semicolons.".
--
-- This function makes sure that the semicolons separating the declarations
-- are handled properly. Unfortunately the final semicolon after the last
-- declaration will be consumed by seekEndOfDeclaration :(
--
-- :m +Hello.Css.Parser.Declaration
-- :m +Hello.Css.Tokenizer
-- :m +Hello.Css.Declaration
-- parseDeclarationStart ((nextToken . defaultParser $ " ; color: red"), defaultDeclaration)
-- parseDeclarationStart ((nextToken . defaultParser $ "  color: red"), defaultDeclaration)
-- parseDeclarationStart ((nextToken . defaultParserInBlock $ "   ; color: red"), defaultDeclaration)
-- parseDeclarationStart ((nextToken . defaultParser $ "    } a.prop >"), defaultDeclaration)
parseDeclarationStart :: ((CssParser, CssToken), CssDeclaration) -> Maybe ((CssParser, CssToken), CssDeclaration)
parseDeclarationStart (pat, decl) = case runParser (many (parserTokenWhitespace <|> parserTokenSemicolon)) pat of
                                      Just (pat', _) -> Just (pat', decl)    -- Some space tokens have been consumed.
                                      Nothing        -> Just (pat, decl)     -- No space tokens were consumed.





-- Consume spaces from end of declaration.
--
-- https://www.w3.org/TR/css-syntax-3/#consume-declaration:
-- "6. While the last token in the declaration’s value is a <whitespace-token>, remove that token."
--
-- :m +Hello.Css.Parser.Declaration
-- :m +Hello.Css.Tokenizer
-- :m +Hello.Css.Declaration
-- parseDeclarationEnd ((nextToken . defaultParser $ " ; color: red"), defaultDeclaration)
-- parseDeclarationEnd ((nextToken . defaultParser $ "  color: red"), defaultDeclaration)
-- parseDeclarationEnd ((nextToken . defaultParserInBlock $ "   ; color: red"), defaultDeclaration)
-- parseDeclarationEnd ((nextToken . defaultParser $ "    } a.prop >"), defaultDeclaration)
parseDeclarationEnd :: ((CssParser, CssToken), CssDeclaration) -> Maybe ((CssParser, CssToken), CssDeclaration)
parseDeclarationEnd (pat, decl) = case runParser (many parserTokenWhitespace) pat of
                                    Just (pat', _) -> Just (pat', decl)    -- Some space tokens have been consumed.
                                    Nothing        -> Just (pat, decl)     -- No space tokens were consumed.




-- Find end of current declaration. Needed only if something goes wrong
-- during parsign of current declaration.
--
-- https://www.w3.org/TR/css-syntax-3/#error-handling: "When interpreting a
-- list of declarations, unknown syntax at any point causes the parser to
-- throw away whatever declaration it’s currently building, and seek forward
-- until it finds a semicolon (or the end of the block). It then starts
-- fresh, trying to parse a declaration again."
seekEndOfDeclaration pair@(_, CssTokEnd)             = pair
seekEndOfDeclaration pair@(_, CssTokBraceCurlyClose) = pair -- '}' is not a
 -- part of declaration, so don't go past it. Return '}' as current token.
 -- strictly speaking ';' is not part of declaration - it separates
 -- declarations, but to properly finalize seek on error let's consume ';'
 -- too. TODO: evaluate if this is a good idea.
seekEndOfDeclaration (parser, CssTokSemicolon)       = nextToken parser
seekEndOfDeclaration (parser, _)                     = seekEndOfDeclaration . nextToken $ parser




-- Consume input until end of {} block is encountered.
-- To be called when handling errors during parsing of {} block.
consumeRestOfCurlyBlock pair@(_, CssTokEnd)             = pair
consumeRestOfCurlyBlock (parser, CssTokBraceCurlyClose) =
  -- Don't forget to consume the spaces after closing } too.
  -- Since we are leaving the block, set inBlock flag accordingly.
  consumeFinalSpaces . nextToken $ parser { inBlock = False }
  where
    consumeFinalSpaces (p, CssTokWS) = consumeFinalSpaces . nextToken $ p
    consumeFinalSpaces pat = pat
consumeRestOfCurlyBlock (parser, _)                     = consumeRestOfCurlyBlock . nextToken $ parser




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

    -- TODO: 'background image' can be also set in value of
    -- CssPropertyBackground property. Expand the function to cover that case
    -- too.
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




-- TODO: this looks like a duplicate of readDeclarations.
--
-- :m +Hello.Css.Parser.Declaration
-- :m +Hello.Css.Tokenizer
-- :m +Hello.Css.Declaration
--
-- parseAllDeclarations  (nextToken . defaultParserInBlock $ "border-top-color: #000001; border-right-color: #000002; border-bottom-color: #000003 !important; border-left-color: #000004;", (defaultCssDeclarationSet, defaultCssDeclarationSet))
parseAllDeclarations :: ((CssParser, CssToken), CssDeclarationSets) -> ((CssParser, CssToken), CssDeclarationSets)
parseAllDeclarations input@((_, CssTokEnd), _)             = input
parseAllDeclarations input@((_, CssTokBraceCurlyClose), _) = input
parseAllDeclarations input                                 = parseAllDeclarations . parseSingleDeclarationWrapper $ input




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





-- A helper data type
--
-- https://www.w3.org/TR/css-syntax-3/#style-rules
--
-- "A style rule is a qualified rule that associates a selector list with a
-- list of property declarations and possibly a list of nested rules."
data CssParsedStyleRule = CssParsedStyleRule
  { -- "The prelude of the qualified rule is parsed as a <selector-list>. If
    -- this returns failure, the entire style rule is invalid."
    prelude :: [CssCachedComplexSelector]

    -- "The content of the qualified rule’s block is parsed as a style
    -- block’s contents."
  , content :: CssDeclarationSets
  } deriving (Show, Eq)




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
-- :m +Hello.Css.Parser.Declaration
-- :m +Hello.Css.Tokenizer
-- :m +Hello.Css.Declaration
--
-- parseStyleRule (nextToken $ defaultParser "body {color:red ; background-color: #ffff00;line-height: normal h1{color:blue} h2{color: #001122} h3 {color : #998877;}")
--
-- Unit-tested: yes
parseStyleRule :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssParsedStyleRule)
parseStyleRule pat = case readSelectorList pat of
                       (pat', Nothing)           -> (pat', Nothing)
                       (pat', Just selectorList) -> case readDeclarationsBlockWithError pat' of
                                                      (pat'', Just declSets) -> (pat'', Just $ CssParsedStyleRule selectorList declSets)
                                                      (pat'', Nothing)       -> (pat'', Nothing)




-- Read declarations that are within a {} block.
--
-- The function expects some empty/initial/default declaration sets as input.
--
-- The function is not dealing with opening or closing brace.
--
-- TODO: this looks like a duplicate of parseAllDeclarations
--
-- :m +Hello.Css.Parser.Declaration
-- :m +Hello.Css.Tokenizer
-- :m +Hello.Css.Declaration
-- readDeclarations (nextToken . defaultParserInBlock $ "border-top-color: #000001; border-right-color: #000002; border-bottom-color: #000003 !important; border-left-color: #000004", (defaultCssDeclarationSet, defaultCssDeclarationSet))
--
-- Unit-tested: yes
readDeclarations :: ((CssParser, CssToken), CssDeclarationSets) -> ((CssParser, CssToken), CssDeclarationSets)
readDeclarations input@((_, token), _) =
  case token of
    CssTokEnd             -> input
    CssTokBraceCurlyClose -> input
    _                     -> readDeclarations . parseSingleDeclarationWrapper $ input




-- Read a {} block with declarations.
--
-- :m +Hello.Css.Parser.Declaration
-- :m +Hello.Css.Tokenizer
-- :m +Hello.Css.Declaration
--
-- readDeclarationsBlock (nextToken $ defaultParser "{} p.v")
-- readDeclarationsBlock (nextToken $ defaultParser "{color: rgb(0, 100, 0)} p.v")
-- readDeclarationsBlock (nextToken $ defaultParser " { color:rgb(0, 100, 0) !important} p.v")
readDeclarationsBlock :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssDeclarationSets)
readDeclarationsBlock = runParser $ parserOpeningBrace *> parserDeclarations <* parserClosingBrace
  where
    parserOpeningBrace :: Parser (CssParser, CssToken) CssToken
    parserOpeningBrace = Parser $ \ pat ->
      case runParser parser pat of
        Just ((p, t), declSet) -> Just ((p { inBlock = True }, t), declSet) -- inBlock = True: we enter {} block.
        Nothing                -> Nothing
      where
        parser = (many parserTokenWhitespace) *> parserTokenBraceCurlyOpen <* (many parserTokenWhitespace)


    parserClosingBrace :: Parser (CssParser, CssToken) CssToken
    parserClosingBrace = Parser $ \ pat ->
      case runParser parser pat of
        Just ((p, t), declSet) -> Just ((p { inBlock = False }, t), declSet) -- inBlock = False: we leave {} block.
        Nothing                -> Nothing
      where
        parser = (many parserTokenWhitespace) *> parserTokenBraceCurlyClose <* (many parserTokenWhitespace)


    -- Parse all declarations located between '{' and '}'.
    -- This function appears to never fail: it always returns some Just. This
    -- is because failure to parse a single declaration in declarations block
    -- result in skipping just the single declaration. Other declarations in
    -- the block may still be parsed correctly and be returned in Just.
    --
    -- TODO: check if a block with empty list of declarations is valid. If
    -- this function fails to correctly parse zero declarations, it will
    -- return empty CssDeclarationSets.
    parserDeclarations :: Parser (CssParser, CssToken) CssDeclarationSets
    parserDeclarations = Parser $ \ pat ->
      case readDeclarations (pat, (defaultCssDeclarationSet, defaultCssDeclarationSet)) of
        (pat', declSets) -> Just (pat', declSets)




-- Read a {} block with declarations. On success return the declaration sets.
-- On parse error do a recovery and move parser to end of invalid block.
--
-- :m +Hello.Css.Parser.Declaration
-- :m +Hello.Css.Tokenizer
-- :m +Hello.Css.Declaration
--
-- readDeclarationsBlockWithError  (nextToken $ defaultParser " { color:rgb(0, 100, 0) !important } p.now ")   -- success
-- readDeclarationsBlockWithError  (nextToken $ defaultParser " { color:rgb(0, 100, 0) !importan } p.now ")    -- failure (invalid input)
readDeclarationsBlockWithError :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclarationSets)
readDeclarationsBlockWithError pat = case readDeclarationsBlock pat of
                                       Just (pat', declSets) -> (pat', Just declSets)
                                       Nothing               -> (consumeRestOfCurlyBlock pat, Nothing)

