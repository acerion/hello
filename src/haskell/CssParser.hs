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

This file is derived from dillo-3.0.5/src/cssparser.cc.
Copyright assignments from that file:
Copyright 2004 Sebastian Geerken <sgeerken@dillo.org>
Copyright 2008-2009 Johannes Hofmann <Johannes.Hofmann@gmx.de>
-}




{-
TODO: be careful with decoding string from external representation into
Data.Text. My original attempt to convert C string into Data.Text in
ffi/CssParser.hsc used T.E.decodeUtf8. I had to change it to T.E.decodeLatin1
because of exceptions from Data.Text module on some characters in some css
files.

Take a look at value of 'content' in this part of css:
a.navmenu::after { content: " â–¶"; }font.logo, font.logobl, img.logo {display: none;}img.sslogo
The line comes from https://lwn.net/CSS/pure-lwn, and the value would lead to
"libEval.so: Cannot decode byte '\xb6': Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream" error.
-}




{-# LANGUAGE OverloadedStrings #-}




module CssParser(nextToken
                , takeSymbol
                , takeInt
                , CssParser (..)
                , CssToken (..)
                , tryTakingRgbFunction
                , parseRgbFunction
                , parseRgbFunctionInt
                , declarationValueAsColor
                , declarationValueAsInt
                , declarationValueAsEnum
                , declarationValueAsEnum'
                , declarationValueAsMultiEnum
                , declarationValueAsWeightInteger
                , tokenMatchesProperty
                , defaultParser) where




import Prelude
import Data.Maybe
import Foreign
import qualified Data.Char as D.C
import qualified Data.Text as T
import qualified Data.Text.Read as T.R
import qualified Data.Text.IO as T.IO
import qualified HelloUtils as HU
import qualified Data.Vector as V
import qualified Data.List as L
import Colors
import HelloUtils




data CssToken =
    CssTokI   Int
  | CssTokF   Float
  | CssTokCol T.Text
  | CssTokSym T.Text
  | CssTokStr T.Text
  | CssTokCh  Char
  | CssTokEnd         -- End of input. No new tokens will appear in input.
  | CssTokNone        -- No token was taken, proceed with parsing input data to try to take some token.
  deriving (Show)




data CssParser = CssParser {
    remainder      :: T.Text
  , spaceSeparated :: Bool
  , withinBlock    :: Bool
  , bufOffset      :: Int
  } deriving (Show)




defaultParser = CssParser {
    remainder = ""
  , withinBlock = False
  , spaceSeparated = False
  , bufOffset = 0
  }




css_background_attachment_enum_vals = ["scroll", "fixed"]
css_background_repeat_enum_vals     = ["repeat", "repeat-x", "repeat-y", "no-repeat"]
css_border_collapse_enum_vals       = ["separate", "collapse"]
css_border_color_enum_vals          = ["transparent"]
css_border_style_enum_vals          = ["none", "hidden", "dotted", "dashed", "solid", "double", "groove", "ridge", "inset", "outset"]
css_border_width_enum_vals          = ["thin", "medium", "thick"]
css_cursor_enum_vals                = ["crosshair", "default", "pointer", "move", "e-resize", "ne-resize", "nw-resize", "n-resize", "se-resize", "sw-resize", "s-resize", "w-resize", "text", "wait", "help"]
css_display_enum_vals               = ["block", "inline", "inline-block", "list-item", "none", "table", "table-row-group", "table-header-group", "table-footer-group", "table-row", "table-cell"]
css_font_size_enum_vals             = ["large", "larger", "medium", "small", "smaller", "xx-large", "xx-small", "x-large", "x-small"]
css_font_style_enum_vals            = ["normal", "italic", "oblique"]
css_font_variant_enum_vals          = ["normal", "small-caps"]
css_font_weight_enum_vals           = ["bold", "bolder", "light", "lighter", "normal"]
css_letter_spacing_enum_vals        = ["normal"]
css_list_style_position_enum_vals   = ["inside", "outside"]
css_line_height_enum_vals           = ["normal"]
css_list_style_type_enum_vals       = ["disc", "circle", "square", "decimal", "decimal-leading-zero", "lower-roman", "upper-roman", "lower-greek", "lower-alpha", "lower-latin", "upper-alpha", "upper-latin", "hebrew", "armenian", "georgian", "cjk-ideographic", "hiragana", "katakana", "hiragana-iroha", "katakana-iroha", "none"]
css_text_align_enum_vals            = ["left", "right", "center", "justify", "string"]
css_text_decoration_enum_vals       = ["underline", "overline", "line-through", "blink"]
css_text_transform_enum_vals        = ["none", "capitalize", "uppercase", "lowercase"]
css_vertical_align_vals             = ["top", "bottom", "middle", "baseline", "sub", "super", "text-top", "text-bottom"]
css_white_space_vals                = ["normal", "pre", "nowrap", "pre-wrap", "pre-line"]
css_word_spacing_enum_vals          = ["normal"]




cssDeclarationValueTypeINTEGER                  =  0 -- This type is only used internally, for x-* properties.
cssDeclarationValueTypeENUM                     =  1 -- Value is i, if represented by enum_symbols[i].
cssDeclarationValueTypeMULTI_ENUM               =  2 -- For all enum_symbols[i], 1 << i are combined.
cssDeclarationValueTypeLENGTH_PERCENTAGE        =  3 -- <length> or <percentage>. Represented by CssLength.
cssDeclarationValueTypeLENGTH                   =  4 -- <length>, represented as CssLength. Note: In some
                                                    -- cases, CSS_TYPE_LENGTH is used instead of
                                                    -- CSS_TYPE_LENGTH_PERCENTAGE, only because Dw cannot
                                                    -- handle percentages in this particular case (e.g.
                                                    -- 'margin-*-width').
cssDeclarationValueTypeSIGNED_LENGTH            =  5 -- As CSS_TYPE_LENGTH but may be negative.
cssDeclarationValueTypeLENGTH_PERCENTAGE_NUMBER =  6 -- <length> or <percentage>, or <number>
cssDeclarationValueTypeAUTO                     =  7 -- Represented as CssLength of type CSS_LENGTH_TYPE_AUTO
cssDeclarationValueTypeCOLOR                    =  8 -- Represented as integer.
cssDeclarationValueTypeFONT_WEIGHT              =  9 -- This very special and only used by 'font-weight'
cssDeclarationValueTypeSTRING                   = 10 -- <string>
cssDeclarationValueTypeSYMBOL                   = 11 -- Symbols, which are directly copied (as opposed to
                                                     -- CSS_PROPERTY_DATA_TYPE_ENUM and
                                                     -- CSS_PROPERTY_DATA_TYPE_MULTI_ENUM). Used for
                                                     -- 'font-family'.
cssDeclarationValueTypeURI                      = 12 -- <uri>
cssDeclarationValueTypeBACKGROUND_POSITION      = 13 --
cssDeclarationValueTypeUNUSED                   = 14 -- Not yet used. Will itself get unused some day.




cssPropertyInfo = V.fromList [
     ("background-attachment",  [ cssDeclarationValueTypeENUM ],                                                    css_background_attachment_enum_vals)
   , ("background-color",       [ cssDeclarationValueTypeCOLOR ],                                                   [])
   , ("background-image",       [ cssDeclarationValueTypeURI ],                                                     [])
   , ("background-position",    [ cssDeclarationValueTypeBACKGROUND_POSITION ],                                     [])
   , ("background-repeat",      [ cssDeclarationValueTypeENUM ],                                                    css_background_repeat_enum_vals)
   , ("border-bottom-color",    [ cssDeclarationValueTypeENUM, cssDeclarationValueTypeCOLOR ],                      css_border_color_enum_vals)
   , ("border-bottom-style",    [ cssDeclarationValueTypeENUM ],                                                    css_border_style_enum_vals)
   , ("border-bottom-width",    [ cssDeclarationValueTypeENUM, cssDeclarationValueTypeLENGTH ],                     css_border_width_enum_vals)
   , ("border-collapse",        [ cssDeclarationValueTypeENUM ],                                                    css_border_collapse_enum_vals)
   , ("border-left-color",      [ cssDeclarationValueTypeENUM, cssDeclarationValueTypeCOLOR ],                      css_border_color_enum_vals)
   , ("border-left-style",      [ cssDeclarationValueTypeENUM ],                                                    css_border_style_enum_vals)
   , ("border-left-width",      [ cssDeclarationValueTypeENUM, cssDeclarationValueTypeLENGTH ],                     css_border_width_enum_vals)
   , ("border-right-color",     [ cssDeclarationValueTypeENUM, cssDeclarationValueTypeCOLOR ],                      css_border_color_enum_vals)
   , ("border-right-style",     [ cssDeclarationValueTypeENUM ],                                                    css_border_style_enum_vals)
   , ("border-rigth-width",     [ cssDeclarationValueTypeENUM, cssDeclarationValueTypeLENGTH ],                     css_border_width_enum_vals)
   , ("border-spacing",         [ cssDeclarationValueTypeLENGTH ],                                                  [])
   , ("border-top-color",       [ cssDeclarationValueTypeENUM, cssDeclarationValueTypeCOLOR ],                      css_border_color_enum_vals)
   , ("border-top-style",       [ cssDeclarationValueTypeENUM ],                                                    css_border_style_enum_vals)
   , ("border-top-width",       [ cssDeclarationValueTypeENUM, cssDeclarationValueTypeLENGTH ],                     css_border_width_enum_vals)
   , ("bottom",                 [ cssDeclarationValueTypeUNUSED ],                                                  [])
   , ("caption-side",           [ cssDeclarationValueTypeUNUSED ],                                                  [])
   , ("clear",                  [ cssDeclarationValueTypeUNUSED ],                                                  [])
   , ("clip",                   [ cssDeclarationValueTypeUNUSED ],                                                  [])
   , ("color",                  [ cssDeclarationValueTypeCOLOR ],                                                   [])
   , ("content",                [ cssDeclarationValueTypeSTRING ],                                                  [])
   , ("counter-increment",      [ cssDeclarationValueTypeUNUSED ],                                                  [])
   , ("counter-reset",          [ cssDeclarationValueTypeUNUSED ],                                                  [])
   , ("cursor",                 [ cssDeclarationValueTypeENUM ],                                                    css_cursor_enum_vals)
   , ("direction",              [ cssDeclarationValueTypeUNUSED ],                                                  [])
   , ("display",                [ cssDeclarationValueTypeENUM ],                                                    css_display_enum_vals)
   , ("empty-cells",            [ cssDeclarationValueTypeUNUSED ],                                                  [])
   , ("float",                  [ cssDeclarationValueTypeUNUSED ],                                                  [])
   , ("font-family",            [ cssDeclarationValueTypeSYMBOL ],                                                  [])
   , ("font-size",              [ cssDeclarationValueTypeENUM, cssDeclarationValueTypeLENGTH_PERCENTAGE ],          css_font_size_enum_vals)
   , ("font-size-adjust",       [ cssDeclarationValueTypeUNUSED ],                                                  [])
   , ("font-stretch",           [ cssDeclarationValueTypeUNUSED ],                                                  [])
   , ("font-style",             [ cssDeclarationValueTypeENUM ],                                                    css_font_style_enum_vals)
   , ("font-variant",           [ cssDeclarationValueTypeENUM ],                                                    css_font_variant_enum_vals)
   , ("font-weight",            [ cssDeclarationValueTypeENUM, cssDeclarationValueTypeFONT_WEIGHT ],                css_font_weight_enum_vals)
   , ("height",                 [ cssDeclarationValueTypeLENGTH_PERCENTAGE, cssDeclarationValueTypeAUTO ],          [])
   , ("left",                   [ cssDeclarationValueTypeUNUSED ],                                                  [])
   , ("letter-spacing",         [ cssDeclarationValueTypeENUM, cssDeclarationValueTypeSIGNED_LENGTH ],              css_letter_spacing_enum_vals)
   , ("line-height",            [ cssDeclarationValueTypeENUM, cssDeclarationValueTypeLENGTH_PERCENTAGE_NUMBER ],   css_line_height_enum_vals)
   , ("list-style-image",       [ cssDeclarationValueTypeUNUSED ],                                                  [])
   , ("list-style-position",    [ cssDeclarationValueTypeENUM ],                                                    css_list_style_position_enum_vals)
   , ("list-style-type",        [ cssDeclarationValueTypeENUM ],                                                    css_list_style_type_enum_vals)
   , ("margin-bottom",          [ cssDeclarationValueTypeSIGNED_LENGTH, cssDeclarationValueTypeAUTO ],              [])
   , ("margin-left",            [ cssDeclarationValueTypeSIGNED_LENGTH, cssDeclarationValueTypeAUTO ],              [])
   , ("margin-right",           [ cssDeclarationValueTypeSIGNED_LENGTH, cssDeclarationValueTypeAUTO ],              [])
   , ("margin-top",             [ cssDeclarationValueTypeSIGNED_LENGTH, cssDeclarationValueTypeAUTO ],              [])
   , ("marker-offset",          [ cssDeclarationValueTypeUNUSED ],                                                  [])
   , ("marks",                  [ cssDeclarationValueTypeUNUSED ],                                                  [])
   , ("max-height",             [ cssDeclarationValueTypeUNUSED ],                                                  [])
   , ("max-width",              [ cssDeclarationValueTypeUNUSED ],                                                  [])
   , ("min-height",             [ cssDeclarationValueTypeUNUSED ],                                                  [])
   , ("min-width",              [ cssDeclarationValueTypeUNUSED ],                                                  [])
   , ("outline-color",          [ cssDeclarationValueTypeUNUSED ],                                                  [])
   , ("outline-style",          [ cssDeclarationValueTypeUNUSED ],                                                  [])
   , ("outline-width",          [ cssDeclarationValueTypeUNUSED ],                                                  [])
   , ("overflow",               [ cssDeclarationValueTypeUNUSED ],                                                  [])
   , ("padding-bottom",         [ cssDeclarationValueTypeLENGTH ],                                                  [])
   , ("padding-left",           [ cssDeclarationValueTypeLENGTH ],                                                  [])
   , ("padding-right",          [ cssDeclarationValueTypeLENGTH ],                                                  [])
   , ("padding-top",            [ cssDeclarationValueTypeLENGTH ],                                                  [])
   , ("position",               [ cssDeclarationValueTypeUNUSED ],                                                  [])
   , ("quotes",                 [ cssDeclarationValueTypeUNUSED ],                                                  [])
   , ("right",                  [ cssDeclarationValueTypeUNUSED ],                                                  [])
   , ("text-align",             [ cssDeclarationValueTypeENUM ],                                                    css_text_align_enum_vals)
   , ("text-decoration",        [ cssDeclarationValueTypeMULTI_ENUM ],                                              css_text_decoration_enum_vals)
   , ("text-indent",            [ cssDeclarationValueTypeLENGTH_PERCENTAGE ],                                       [])
   , ("text-shadow",            [ cssDeclarationValueTypeUNUSED ],                                                  [])
   , ("text-transform",         [ cssDeclarationValueTypeENUM ],                                                    css_text_transform_enum_vals)
   , ("top",                    [ cssDeclarationValueTypeUNUSED ],                                                  [])
   , ("unicode-bidi",           [ cssDeclarationValueTypeUNUSED ],                                                  [])
   , ("vertical-align",         [ cssDeclarationValueTypeENUM ],                                                    css_vertical_align_vals)
   , ("visibility",             [ cssDeclarationValueTypeUNUSED ],                                                  [])
   , ("white-space",            [ cssDeclarationValueTypeENUM ],                                                    css_white_space_vals)
   , ("width",                  [ cssDeclarationValueTypeLENGTH_PERCENTAGE, cssDeclarationValueTypeAUTO ],          [])
   , ("word-spacing",           [ cssDeclarationValueTypeENUM, cssDeclarationValueTypeSIGNED_LENGTH ],              css_word_spacing_enum_vals)
   , ("z-index",                [ cssDeclarationValueTypeUNUSED ],                                                  [])

   -- These are extensions, for internal used, and never parsed.
   , ("x-link",                 [ cssDeclarationValueTypeINTEGER ],                                                 [])
   , ("x-colspan",              [ cssDeclarationValueTypeINTEGER ],                                                 [])
   , ("x-rowspan",              [ cssDeclarationValueTypeINTEGER ],                                                 [])
   , ("last",                   [], [])
   ] :: V.Vector (T.Text, [Int], [T.Text])




nextToken :: CssParser -> (CssParser, CssToken)
nextToken parser = (updatedParser{bufOffset = increasedBufOffset parser}, token)
  where
    (updatedParser, token) = nextToken' parser{spaceSeparated = False}
    increasedBufOffset parser = (bufOffset parser) + (T.length . remainder $ parser) - (T.length . remainder $ updatedParser)




-- This function is based on function with the same name from Real World
-- Haskell, chapter 10.
--
-- These lines are most awesome piece of code that I've written so far, in
-- any project.
(>>?) :: (CssParser, CssToken) -> (CssParser -> (CssParser, CssToken)) -> (CssParser, CssToken)
(parser, CssTokNone) >>? f = f parser
pair@(parser, _) >>? _     = pair





-- Try taking Float before trying to take Int, because otherwise you may take
-- only an initial (integral) part of Float as an Int, and leave fractional
-- part in remainder.
nextToken' :: CssParser -> (CssParser, CssToken)
nextToken' parser = takeLeadingWhite parser >>?
                    takeFloat               >>?
                    takeInt                 >>?
                    takeSymbol              >>?
                    takeString              >>?
                    takeColor               >>?
                    takeCharacter




-- Symbol must not start with a digit, therefore we have to have some kind of
-- test at the beginning. TODO: can we do the test in a better way?
--
-- TODO: Original C code parsed symbols starting with '-' (such as
-- "-webkit-user-select") in a way that resulted in token without the leading
-- '-' (so the resulting token was "webkit-user-select"). Haskell code keeps
-- the leading '-' character.
takeSymbol :: CssParser -> (CssParser, CssToken)
takeSymbol parser = if predNonNumeric . T.head . remainder $ parser
                    then (parserAppend parser tok, CssTokSym tok)
                    else (parser, CssTokNone)
  where tok = T.takeWhile pred (remainder parser)
        predNonNumeric = (\c -> D.C.isAlpha c || c == '_' || c == '-')
        pred = (\c -> D.C.isAlphaNum c || c == '_' || c == '-')




{-
TODO: handle escaped sequences in string.
Examples of escaped sequences:

before{content:"Poka\00017C  wi\000119cej";position:relative;top:-5px}
token: "Poka7C  wi19cej", [50 6f 6b 61 01 37 43 20 20 77 69 01 31 39 63 65 6a ]

before{content:"Na \00017Bywo"}
token: "Na 7Bywo", [4e 61 20 01 37 42 79 77 6f ]

.icon--arrow-right:before{content:"\f107"}
token  = "", [07 ]
-}
takeString :: CssParser -> (CssParser, CssToken)
takeString parser = case HU.takeEnclosed (remainder parser) "\"" "\"" True of
                      (Just string, rem) -> parseString parser string rem
                      (Nothing, _) -> case HU.takeEnclosed (remainder parser) "'" "'" True of
                                       (Just string, rem) -> parseString parser string rem
                                       (Nothing, _) -> (parser, CssTokNone)
  where
    parseString :: CssParser -> T.Text -> T.Text -> (CssParser, CssToken)
    parseString parser string rem = (parser{remainder = rem}, CssTokStr (escapedString string))
    escapedString str = case T.findIndex (== '\\') str of
                          Just i -> ""
                          Nothing -> str




-- TODO: think about performance of using isPrefixOf to get just one
-- character, here and elsewhere.
takeColor :: CssParser -> (CssParser, CssToken)
takeColor parser = if T.isPrefixOf "#" (remainder parser) && (withinBlock parser)
                   then takeColor' parser
                   else (parser, CssTokNone) -- Don't take the leading '#' if we are not in a block

  where
    takeColor' :: CssParser -> (CssParser, CssToken)
    takeColor' parser = (parser{ remainder = newRem }
                        , CssTokCol (T.concat ["#", newValue ]))
                        -- TODO: verify if we really need the leading '#' in token.
                        -- TODO: what if there are no digits after '#'?
                        -- TODO: add better handling of '#' followed by non-hex string.

      where
        newValue = T.takeWhile D.C.isHexDigit digitsString
        newRem = T.dropWhile D.C.isHexDigit digitsString
        digitsString = T.drop 1 (remainder parser)




takeCharacter :: CssParser -> (CssParser, CssToken)
takeCharacter parser = if T.null . remainder $ parser
                       then (parser, CssTokNone)
                       else (parserAppend parser (T.singleton . T.head . remainder $ parser),
                             CssTokCh (T.head . remainder $ parser))




-- This function does not return a token. Discarding meaningless data from
-- beginning of text would not create a valid token.
takeLeadingWhite :: CssParser -> (CssParser, CssToken)
takeLeadingWhite parser
  | T.null rem                 = (parser, CssTokEnd)
  | D.C.isSpace . T.head $ rem = takeLeadingWhite parser { remainder = T.tail rem, spaceSeparated = True }
  | T.isPrefixOf "/*" rem      = takeLeadingWhite parser { remainder = HU.skipEnclosed rem "/*" "*/" }
  | T.isPrefixOf "<!--" rem    = takeLeadingWhite parser { remainder = HU.skipEnclosed rem "<!--" "-->" }
  | otherwise                  = (parser, CssTokNone)
  where rem = remainder parser




parserAppend :: CssParser -> T.Text -> CssParser
parserAppend parser tok = parser { remainder = T.drop (T.length tok) (remainder parser) }




-- TODO: this function doesn't recognize some float formats that are valid in
-- CSS, e.g. ".5".
--
-- T.R.rational is happy to interpret "100" as float, but we want to treat is
-- as int. Therefore we have to search for '.' in taken sub-string :(
takeFloat :: CssParser -> (CssParser, CssToken)
takeFloat parser = case T.R.signed T.R.rational (remainder parser) of
                     Right pair -> case T.find (== '.') val of
                                     Just c    -> (parserAppend parser val, CssTokF (fst pair))
                                     otherwise -> (parser, CssTokNone)
                       where
                         val = T.take diff (remainder parser)
                         newRem = snd pair
                         diff = (T.length . remainder $ parser) - (T.length newRem)
                     Left pair -> (parser, CssTokNone)




-- This function is very similar to takeFloat, but I don't want to write a
-- common function just yet. takeFloat will have to be updated to read all
-- formats of float value, and that change may make it more complicated and
-- less similar to takeInt.
takeInt :: CssParser -> (CssParser, CssToken)
takeInt parser = case T.R.signed T.R.decimal (remainder parser) of
                   Right pair -> (parserAppend parser val, CssTokI (fst pair))
                     where
                       val = T.take diff (remainder parser)
                       newRem = snd pair
                       diff = (T.length . remainder $ parser) - (T.length newRem)
                   Left pair -> (parser, CssTokNone)




{-
takeAllTokens :: (CssParser, CssToken) -> IO CssParser
takeAllTokens (parser,token) = do
  T.IO.putStrLn (remainder parser)
  let (p, t) = nextToken parser
  if cssTokenType t == CssTokEnd
    then return p
    else takeAllTokens . nextToken $ p
-}




parseRgbFunctionInt :: CssParser -> (CssParser, Maybe Int)
parseRgbFunctionInt parser =
  case parseRgbFunction parser of
    (parser', Nothing) -> (parser', Nothing)
    (parser', Just (tr, tg, tb, ta, isPercent)) -> (parser', Just color)
      where
        color = (r `shiftL` 16) .|. (g `shiftL` 8) .|. b
        r = getInt tr isPercent
        g = getInt tg isPercent
        b = getInt tb isPercent

        -- TODO: make sure that r/g/b values are in range 0-255.
        getInt :: Int -> Bool -> Int
        getInt i isPercent = if isPercent then (i * 255) `div` 100 else i




parseRgbFunction :: CssParser -> (CssParser, Maybe (Int, Int, Int, T.Text, Bool))
parseRgbFunction parser =
  let fun = tryTakingRgbFunction $ parser
      parser' = fst fun
      tokens = snd fun :: [CssToken]
  in
    case tokens of
      (CssTokCh par2:CssTokCh perc3:CssTokI b:CssTokCh comma2:CssTokCh perc2:CssTokI g:CssTokCh comma1:CssTokCh perc1:CssTokI r:CssTokCh par1:[]) ->
        if par1 == '(' && par2 == ')' && perc3 == '%' && perc2 == '%' && perc1 == '%' && comma2 == ',' && comma1 == ','
        then (parser', Just (r, g, b, "%", True))
        else (parser', Nothing)
      (CssTokCh par2:CssTokI b:CssTokCh comma2:CssTokI g:CssTokCh comma1:CssTokI r:CssTokCh par1:[]) ->
        if par1 == '(' && par2 == ')' && comma2 == ',' && comma1 == ','
        then (parser', Just (r, g, b, "/100", False))
        else (parser', Nothing)
      otherwise -> (parser', Nothing)




tryTakingRgbFunction :: CssParser -> (CssParser, [CssToken])
tryTakingRgbFunction parser = takeNext parser []
  where
    takeNext :: CssParser -> [CssToken] -> (CssParser, [CssToken])
    takeNext parser list@(CssTokCh c:xs) = if length list == 10 || (c == ')' && length list == 7)
                                           then (nextParser, list)
                                           else takeNext nextParser (tok:list)
      where (nextParser, tok) = nextToken parser

    takeNext parser []        = takeNext nextParser [tok]
      where (nextParser, tok) = nextToken parser

    takeNext parser list                 = if length list == 10
                                           then (nextParser, list)
                                           else takeNext nextParser (tok:list)
      where (nextParser, tok) = nextToken parser
-- TODO: handle invalid token type here




{-
-- TODO: move getting leading minus to takeNumber. Have a clearer distinction
-- between minus being a part of a number and minus being a part of other
-- type of token. Make sure that this call: "nextToken
-- defaultParser{remainder="/* hello */ -"}" returns token type == TokenChar.
-- Or should it be treated as invalid?
takeLeadingMinus :: CssParser -> (CssParser, CssToken)
takeLeadingMinus parser = case T.uncons (remainder parser) of
                            Just (c, rem) | c == '-'  -> (parser{remainder = rem}, CssToken (T.singleton c) Nothing)
                                          | otherwise -> (parser, CssTokNone)
                            Nothing -> (parser, CssTokNone)
-}





declarationValueAsColor :: CssParser -> CssToken -> (CssParser, Maybe Int)
declarationValueAsColor parser (CssTokCol c) = case colorsStringToColor c of -- TODO: we know here that color should have form #RRGGBB. Call function that accepts only this format.
                                                 Just i  -> (parser, Just i)
                                                 Nothing -> (parser, Nothing)
declarationValueAsColor parser (CssTokSym s) | s == "rgb" = parseRgbFunctionInt parser
                                             | otherwise = case colorsStringToColor s of
                                                             Just i  -> (parser, Just i)
                                                             Nothing -> (parser, Nothing)
declarationValueAsColor parser _             = (parser, Nothing)




declarationValueAsInt :: CssParser -> CssToken -> Int -> Int -> (CssParser, Maybe Int)
declarationValueAsInt parser token valueType property = (retParser, retInt)
  where
    (newParser, retInt) | valueType == cssDeclarationValueTypeENUM                     = declarationValueAsEnum parser token property
                        | valueType == cssDeclarationValueTypeCOLOR                    = declarationValueAsColor parser token
                        | valueType == cssDeclarationValueTypeFONT_WEIGHT              = declarationValueAsWeightInteger parser token property
                        | otherwise                                                    = (parser, Nothing)
    --(retParser, _) = nextToken newParser
    retParser = newParser

{-
  | valueType == cssDeclarationValueTypeINTEGER                  = (parser, Nothing)
  | valueType == cssDeclarationValueTypeMULTI_ENUM               = (parser, Nothing)
  | valueType == cssDeclarationValueTypeLENGTH_PERCENTAGE        = (parser, Nothing)
  | valueType == cssDeclarationValueTypeLENGTH                   = (parser, Nothing)
  | valueType == cssDeclarationValueTypeSIGNED_LENGTH            = (parser, Nothing)
  | valueType == cssDeclarationValueTypeLENGTH_PERCENTAGE_NUMBER = (parser, Nothing)
  | valueType == cssDeclarationValueTypeAUTO                     = (parser, Nothing)
  | valueType == cssDeclarationValueTypeSTRING                   = (parser, Nothing)
  | valueType == cssDeclarationValueTypeSYMBOL                   = (parser, Nothing)
  | valueType == cssDeclarationValueTypeURI                      = (parser, Nothing)
  | valueType == cssDeclarationValueTypeBACKGROUND_POSITION      = (parser, Nothing)
  | valueType == cssDeclarationValueTypeUNUSED                   = (parser, Nothing)
-}





declarationValueAsEnum :: CssParser -> CssToken -> Int -> (CssParser, Maybe Int)
declarationValueAsEnum parser (CssTokSym symbol) property =
  case declarationValueAsEnum' symbol enums 0 of
    -1  -> (parser, Nothing)
    idx -> (parser, Just idx)
  where
    propInfo = cssPropertyInfo V.! property
    enums = tripletThrd propInfo
declarationValueAsEnum parser _ property                  = (parser, Nothing)
                                                            -- TODO: is this the right place to reject everything else other than symbol?
                                                            -- Shouldn't we do it somewhere else?




-- TODO: can't we use Data.List.elemIndex here?
declarationValueAsEnum' :: T.Text -> [T.Text] -> Int -> Int
declarationValueAsEnum' symbol []     idx = -1
declarationValueAsEnum' symbol (x:xs) idx = if x == symbol
                                            then idx
                                            else declarationValueAsEnum' symbol xs (idx + 1)




declarationValueAsMultiEnum :: CssParser -> CssToken -> Int -> (CssParser, Maybe Int)
declarationValueAsMultiEnum parser token@(CssTokSym symbol) property = declarationValueAsMultiEnum' parser token enums 0
  where
    propInfo = cssPropertyInfo V.! property
    enums = tripletThrd propInfo
declarationValueAsMultiEnum parser _ property               = (parser, Nothing)
                                                            -- TODO: is this the right place to reject everything else other than symbol?
                                                            -- Shouldn't we do it somewhere else?




declarationValueAsMultiEnum' :: CssParser -> CssToken -> [T.Text] -> Int -> (CssParser, Maybe Int)
declarationValueAsMultiEnum' parser (CssTokSym symbol) (enums) bits =
  case L.elemIndex symbol enums of -- TODO: this search should be case-insensitive
    Just pos -> declarationValueAsMultiEnum' newParser newToken enums (bits .|. (1  `shiftL` pos))
    Nothing  -> declarationValueAsMultiEnum' newParser newToken enums bits
  where
    (newParser, newToken) = nextToken parser
declarationValueAsMultiEnum' parser _ _ bits                        = (parser, Just bits)
-- TODO: we should probably handle in a different way a situation where one
-- of tokens is not a symbol.
--
-- TOOO: symbol "none" should be handled in special way (probably).




declarationValueAsWeightInteger :: CssParser -> CssToken -> Int -> (CssParser, Maybe Int)
declarationValueAsWeightInteger parser (CssTokI i) property = if i >= 100 && i <= 900
                                                              then (parser, Just i)
                                                              else (parser, Nothing)




tokenMatchesProperty :: CssToken -> Int -> Maybe Int
tokenMatchesProperty token property = tokenMatchesProperty' token acceptedValueTypes enums
  where
    propInfo = cssPropertyInfo V.! property
    acceptedValueTypes = tripletSnd propInfo
    enums = tripletThrd propInfo

    tokenMatchesProperty' :: CssToken -> [Int] -> [T.Text] -> Maybe Int
    tokenMatchesProperty' token (t:ts) enums        | t == cssDeclarationValueTypeENUM =
                                                        case token of -- TODO: compare with similar code in in declarationValueAsEnum
                                                          CssTokSym symbol -> case L.elemIndex symbol enums of -- TODO: this search should be case-insensitive
                                                                                Just pos -> Just t
                                                                                Nothing  -> tokenMatchesProperty' token ts enums
                                                          _                -> tokenMatchesProperty' token ts enums

                                                    | t == cssDeclarationValueTypeMULTI_ENUM =
                                                        case token of
                                                          CssTokSym symbol -> if symbol == "none"
                                                                              then Just t
                                                                              else case L.elemIndex symbol enums of -- TODO: this search should be case-insensitive
                                                                                     Just pos -> Just t
                                                                                     Nothing  -> tokenMatchesProperty' token ts enums
                                                          _                -> tokenMatchesProperty' token ts enums
                                                    | t == cssDeclarationValueTypeBACKGROUND_POSITION =
                                                        case token of
                                                          CssTokSym s -> if s == "center" || s == "left" || s == "right" || s == "top" || s == "bottom"
                                                                         then Just t
                                                                         else tokenMatchesProperty' token ts enums
                                                          _           -> tokenMatchesProperty' token ts enums
                                                          -- TODO: actually here we should also somehow handle numeric background positions


                                                    | t == cssDeclarationValueTypeLENGTH_PERCENTAGE || t == cssDeclarationValueTypeLENGTH || t == cssDeclarationValueTypeLENGTH_PERCENTAGE_NUMBER =
                                                        case token of
                                                          CssTokF f -> if f < 0
                                                                       then Nothing
                                                                       else Just t
                                                          CssTokI i -> if i < 0
                                                                       then Nothing
                                                                       else Just t
                                                          _         -> Nothing

                                                    | t == cssDeclarationValueTypeSIGNED_LENGTH =
                                                        case token of
                                                          CssTokF _ -> Just t
                                                          CssTokI _ -> Just t
                                                          _         -> tokenMatchesProperty' token ts enums

                                                    | t == cssDeclarationValueTypeAUTO =
                                                        case token of
                                                          CssTokSym symbol -> if symbol == "auto"
                                                                              then Just t
                                                                              else tokenMatchesProperty' token ts enums
                                                          _                 -> tokenMatchesProperty' token ts enums
                                                    | t == cssDeclarationValueTypeCOLOR =
                                                        case token of
                                                          CssTokCol c -> Just t -- We already know that the token is a valid color token
                                                          CssTokSym s -> case colorsStringToColor s of
                                                                          Just i -> Just t
                                                                          _      -> if s == "rgb"
                                                                                    then Just t
                                                                                    else tokenMatchesProperty' token ts enums
                                                          _           -> tokenMatchesProperty' token ts enums
                                                    | t == cssDeclarationValueTypeSTRING =
                                                        case token of
                                                          CssTokStr s -> Just t
                                                          _           -> tokenMatchesProperty' token ts enums
                                                    | t == cssDeclarationValueTypeSYMBOL =
                                                      case token of
                                                        CssTokSym sym -> Just t
                                                        CssTokStr str -> Just t
                                                        _             -> tokenMatchesProperty' token ts enums
                                                    | t == cssDeclarationValueTypeFONT_WEIGHT =
                                                        case token of
                                                          CssTokI i -> if i >= 100 && i <= 900 -- TODO: this test of range is repeated in this file
                                                                       then Just t
                                                                       else  tokenMatchesProperty' token ts enums
                                                          _         -> tokenMatchesProperty' token ts enums
                                                    | t == cssDeclarationValueTypeURI =
                                                        case token of
                                                          CssTokSym s -> if s == "url"
                                                                         then Just t
                                                                         else tokenMatchesProperty' token ts enums
                                                          _           -> tokenMatchesProperty' token ts enums
                                                    | t == cssDeclarationValueTypeINTEGER = tokenMatchesProperty' token ts enums
                                                    | t == cssDeclarationValueTypeUNUSED = tokenMatchesProperty' token ts enums
                                                    | otherwise = Nothing

    tokenMatchesProperty' token [] _ = Nothing
