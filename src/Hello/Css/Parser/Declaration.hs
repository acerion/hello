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

This file is derived from dillo-3.0.5/src/cssparser.cc.
Copyright assignments from that file:
Copyright 2004 Sebastian Geerken <sgeerken@dillo.org>
Copyright 2008-2009 Johannes Hofmann <Johannes.Hofmann@gmx.de>
Additional note in cssparser.cc:
"This file is heavily based on the CSS parser of dillo-0.8.0-css-3 -
a dillo1 based CSS prototype written by Sebastian Geerken."
-}




{-
Code for parsing of a single CSS declaration.

https://www.w3.org/TR/css-syntax-3/#syntax-description: "Each declaration has
a name, followed by a colon and the declaration value. Declarations are
separated by semicolons."
-}




{-# LANGUAGE OverloadedStrings #-}




module Hello.Css.Parser.Declaration
  (
    parseSingleDeclarationWrapper

  , declarationsSetUpdateOrAdd
  , declarationsSetAppend

  , CssDeclarationSet (..)
  , CssDeclarationSets
  , defaultCssDeclarationSet

  -- Exported only for tests
  , parseProperty
  , parseSingleDeclaration
  )
where




import Control.Applicative (Alternative(..))
import Data.Data (toConstr)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Sequence as S
--import Debug.Trace

import Hello.Css.Declaration
import Hello.Css.Tokenizer
import Hello.Utils.Parser




-- Mapping between name of property and a constructor of the property.
--
-- Only a subset of CSS2.2 properties is supported by this implementation.
cssPropertyCtors :: M.Map T.Text PropertyCtor
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

   , ("font",                   makeCssPropertyFont)
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
parserImportant = parserTokenDelim '!' *> parserTokenIdent "important"




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
  where parser = many parserTokenWhitespace
                 *> (parserTokenSemicolon
                      <|> parserTokenBraceCurlyClose
                      <|> parserTokenEnd)




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
-- parseImportant ((startTokenizer . defaultParser $ "!important;"), defaultDeclaration)
-- parseImportant ((startTokenizer . defaultParser $ "!important }"), defaultDeclaration)
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




defaultCssDeclarationSet :: CssDeclarationSet
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
--
-- Unit-tested: yes
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
--
-- Unit-tested: yes
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
                                               then (set, declarationsSetUpdateOrAdd setImp d)
                                               else (declarationsSetUpdateOrAdd set d, setImp)




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
-- parseDeclarationStart ((startTokenizer . defaultParser $ " ; color: red"), defaultDeclaration)
-- parseDeclarationStart ((startTokenizer . defaultParser $ "  color: red"), defaultDeclaration)
-- parseDeclarationStart ((startTokenizer . defaultParserInBlock $ "   ; color: red"), defaultDeclaration)
-- parseDeclarationStart ((startTokenizer . defaultParser $ "    } a.prop >"), defaultDeclaration)
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
-- parseDeclarationEnd ((startTokenizer . defaultParser $ " ; color: red"), defaultDeclaration)
-- parseDeclarationEnd ((startTokenizer . defaultParser $ "  color: red"), defaultDeclaration)
-- parseDeclarationEnd ((startTokenizer . defaultParserInBlock $ "   ; color: red"), defaultDeclaration)
-- parseDeclarationEnd ((startTokenizer . defaultParser $ "    } a.prop >"), defaultDeclaration)
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
seekEndOfDeclaration :: (CssParser, CssToken) -> (CssParser, CssToken)
seekEndOfDeclaration pair@(_, CssTokEnd)             = pair
seekEndOfDeclaration pair@(_, CssTokBraceCurlyClose) = pair -- '}' is not a
 -- part of declaration, so don't go past it. Return '}' as current token.
 -- strictly speaking ';' is not part of declaration - it separates
 -- declarations, but to properly finalize seek on error let's consume ';'
 -- too. TODO: evaluate if this is a good idea.
seekEndOfDeclaration (parser, CssTokSemicolon)       = nextToken parser
seekEndOfDeclaration (parser, _)                     = seekEndOfDeclaration . nextToken $ parser




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




type CssDeclarationSets = (CssDeclarationSet, CssDeclarationSet)




