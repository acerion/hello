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
Additional note in cssparser.cc:
"This file is heavily based on the CSS parser of dillo-0.8.0-css-3 -
a dillo1 based CSS prototype written by Sebastian Geerken."
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

TODO: think about performance of using isPrefixOf to get just one character.
-}




{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}



module Hello.Css.Tokenizer( CssParser (..)
                          , defaultParser
                          , nextToken1
                          , nextToken2

                          , CssToken (..)

                          , CssNum (..)
                          , cssNumToFloat

                          , CssOrigin (..)
                          )
  where




import Data.Maybe
import qualified Data.Char as D.C
import qualified Data.Text as T
import qualified Data.Text.Read as T.R
import Data.Bits
import Debug.Trace

import qualified Hello.Utils as HU
import Hello.Utils




data CssNum
    = CssNumI Int
    | CssNumF Float
    deriving (Show, Eq)

cssNumToFloat (CssNumF f) = f
cssNumToFloat (CssNumI i) = fromIntegral i




-- Tokens listed in https://www.w3.org/TR/css-syntax-3/#tokenization, but not
-- included in CssToken type (or not commented yet):
--
-- <at-keyword-token>, <string-token>, <bad-string-token>,
-- <delim-token>, <whitespace-token>, <CDO-token>, <CDC-token>,
-- <colon-token>, <semicolon-token>, <comma-token>, <[-token>, <]-token>,
-- <(-token>, <)-token>, <{-token>, and <}-token>.

data CssToken =
    CssTokNum CssNum            -- <number-token>
  | CssTokPerc CssNum           -- <percentage-token>
  | CssTokDim CssNum T.Text     -- <dimension-token>

  -- Ident-like-tokens (https://www.w3.org/TR/css-syntax-3/#consume-ident-like-token):
  | CssTokIdent T.Text          -- <ident-token>; CSS3 spec says that text can be empty: "have a value composed of zero or more code points".
  | CssTokFunc T.Text           -- <function-token>
  | CssTokUrl T.Text            -- <url-token>
  | CssTokBadUrl                -- <bad-url-token>

  | CssTokHash T.Text           -- <hash-token>; T.Text value is not prefixed by '#'.
  | CssTokStr T.Text
  | CssTokCh Char
  | CssTokWS          -- Whitespace
  | CssTokEnd         -- End of input. No new tokens will appear in input.
  | CssTokNone        -- No token was taken, proceed with parsing input data to try to take some token.
  deriving (Show, Eq)




-- TODO: add baseUrl field.
data CssParser = CssParser {
    remainder      :: T.Text
  , spaceSeparated :: Bool
  , inBlock        :: Bool
  , bufOffset      :: Int
  , cssOrigin      :: CssOrigin -- TODO: rethink wheter origin should be a member of parser or not.
  } deriving (Show)




defaultParser = CssParser {
    remainder = ""
  , inBlock   = False
  , spaceSeparated = False
  , bufOffset = 0
  , cssOrigin = CssOriginUserAgent
  }




-- Where does a rule come from?
data CssOrigin =
    CssOriginUserAgent -- = 0  -- Rule comes from User Agent. It is defined in program's source code.
  | CssOriginUser      -- = 1
  | CssOriginAuthor    -- = 2
  deriving (Show)




nextToken1 :: CssParser -> (CssParser, CssToken)
nextToken1 parser = (updatedParser{bufOffset = increasedBufOffset parser}, token)
  where
    (updatedParser, token) = case nextToken1' parser{spaceSeparated = False} of
                               (p, Just t)  -> (p, t)
                               (p, Nothing) -> (p, CssTokNone)
    increasedBufOffset parser = (bufOffset parser) + (T.length . remainder $ parser) - (T.length . remainder $ updatedParser)



nextToken2 :: CssParser -> (CssParser, CssToken)
nextToken2 parser = (updatedParser{bufOffset = increasedBufOffset parser}, token)
  where
    (updatedParser, token) = case nextToken2' parser{spaceSeparated = False} of
                               (p, Just t)  -> (p, t)
                               (p, Nothing) -> (p, CssTokNone)
    increasedBufOffset parser = (bufOffset parser) + (T.length . remainder $ parser) - (T.length . remainder $ updatedParser)




-- This function is based on function with the same name from Real World
-- Haskell, chapter 10.
--
-- These lines are most awesome piece of code that I've written so far, in
-- any project.
(>>?) :: (CssParser, Maybe a) -> (CssParser -> (CssParser, Maybe a)) -> (CssParser, Maybe a)
(parser, Nothing) >>? f = f parser
pair@(parser, _)  >>? _ = pair




nextToken1' :: CssParser -> (CssParser, Maybe CssToken)
nextToken1' parser = takeLeadingWhite parser >>?
                     takeNumericToken        >>?
                     takeIdentLikeToken      >>?
                     takeString              >>?
                     takeHashToken           >>?
                     takeCharToken




nextToken2' :: CssParser -> (CssParser, Maybe CssToken)
nextToken2' parser = takeLeadingWhite2 parser >>?
                     takeNumericToken         >>?
                     takeIdentLikeToken       >>?
                     takeString               >>?
                     takeHashToken            >>?
                     takeCharToken



{-
-- Symbol must not start with a digit, therefore we have to have some kind of
-- test at the beginning. TODO: can we do the test in a better way?
--
-- TODO: Original C code parsed symbols starting with '-' (such as
-- "-webkit-user-select") in a way that resulted in token without the leading
-- '-' (so the resulting token was "webkit-user-select"). Haskell code keeps
-- the leading '-' character.
--
-- TODO: the function uses T.head on a string that can be empty.
takeSymbol :: CssParser -> (CssParser, Maybe CssToken)
takeSymbol parser = if predNonNumeric . T.head . remainder $ parser
                    then (parserMoveBy parser tok, Just $ CssTokSym tok)
                    else (parser, Nothing)
  where tok = T.takeWhile pred (remainder parser)
        predNonNumeric = (\c -> D.C.isAlpha c || c == '_' || c == '-')
        pred = (\c -> D.C.isAlphaNum c || c == '_' || c == '-')
-}



-- Take <ident-token> from a string.
--
-- This implementation is not very pretty. It closely resembles algorithm
-- rescribed in CSS3 spec.
takeIdentToken :: CssParser -> (CssParser, Maybe CssToken)
takeIdentToken parser = if isValidStartOfIdentifier . remainder $ parser
                        then (parserMoveBy parser ident, Just $ CssTokIdent ident)
                        else (parser, Nothing)
  where
    (ident, n) = consumeName (remainder parser) "" 0




-- https://www.w3.org/TR/css-syntax-3/#check-if-three-code-points-would-start-an-identifier
isValidStartOfIdentifier buffer = case T.uncons buffer of
                                    Just (c, rem) | c == '-'               -> False -- TODO: properly handle identifier starting with '-'
                                                  | isNameStartCodePoint c -> True
                                                  | c == '\\'              -> False -- TODO: properly handle an escape
                                                  | otherwise              -> False
                                    Nothing -> False




isNameStartCodePoint :: Char -> Bool
isNameStartCodePoint c = (D.C.isAlpha c && D.C.isAscii c) || isNonAscii c || c == '_'


-- https://www.w3.org/TR/css-syntax-3/#non-ascii-code-point
isNonAscii :: Char -> Bool
isNonAscii c = D.C.ord c >= 0x80


isNameCodePoint :: Char -> Bool
isNameCodePoint c = isNameStartCodePoint c || D.C.isDigit c || c == '-'




takeIdentLikeToken :: CssParser -> (CssParser, Maybe CssToken)
takeIdentLikeToken p1 = case takeIdentToken p1 of
                          (_, Nothing)                      -> (p1, Nothing)
                          (p2, Just t2@(CssTokIdent ident)) -> case takeCharToken p2 of
                                                                 (_, Nothing)              -> (p2, Just t2)
                                                                 (p3, Just (CssTokCh '(')) -> if ident == "url"
                                                                                              then consumeUrlToken p3
                                                                                              else (p3, Just $ CssTokFunc ident)
                                                                 (p3, _)                   -> (p2, Just t2)



consumeUrlToken p1 = if T.length text > 0 && T.last text == ')'
                     then (p2, Just $ CssTokUrl $ T.take (n - 1) text) -- Don't include closing paren.
                     else (p2, Just $ CssTokBadUrl)
  where
    p2 = p1 {remainder = T.drop n $ remainder p1}
    text = T.pack . reverse $ f (remainder p1) []
    n = T.length text

    f :: T.Text -> [Char] -> String
    f buffer acc = case T.uncons buffer of
                 Just (c, rem) | c == ')'  -> (c:acc) -- Include the paren here to recognize valid URL.
                               | otherwise -> f rem (c:acc)
                               -- TODO: these conditions for taking chars should be improved.
                 Nothing -> acc



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
takeString :: CssParser -> (CssParser, Maybe CssToken)
takeString parser = case HU.takeEnclosed (remainder parser) "\"" "\"" True of
                      (Just string, rem) -> parseString parser string rem
                      (Nothing, _) -> case HU.takeEnclosed (remainder parser) "'" "'" True of
                                       (Just string, rem) -> parseString parser string rem
                                       (Nothing, _) -> (parser, Nothing)
  where
    parseString :: CssParser -> T.Text -> T.Text -> (CssParser, Maybe CssToken)
    parseString parser string rem = (parser{remainder = rem}, Just $ CssTokStr (escapedString string))
    escapedString str = case T.findIndex (== '\\') str of
                          Just i -> ""
                          Nothing -> str




-- TODO: the function probably should return <delim-token> in some situations.
-- TODO: what if there are no characters after '#'?
--
-- TODO: do we still need to use inBlock here? After the function has been
-- changed from takeColor to takeHashToken, it could be used to take ID
-- selector tokens. The function can be now very well used outside of block.
takeHashToken :: CssParser -> (CssParser, Maybe CssToken)
takeHashToken parser = if not $ inBlock parser
                       then (parser, Nothing) -- Don't take the leading '#' if we are not in a block;
                       else
                         case T.uncons $ remainder parser of
                           Just ('#', rem) -> (parser { remainder = T.drop (n + 1) $ remainder parser}, Just $ CssTokHash value)
                             -- TODO: That +1 for '#' above doesn't seem too clean. What if there are no valid characters after '#'?
                             where
                               (value, n) = consumeName rem "" 0
                           Just (c, rem)   -> (parser, Nothing)
                           Nothing         -> (parser, Nothing)




-- https://www.w3.org/TR/css-syntax-3/#consume-a-name
--
-- Returns pair (name, count), where count is a number of consumed code
-- points.
consumeName :: T.Text -> T.Text -> Int -> (T.Text, Int)
consumeName buffer acc n = case T.uncons buffer of
                             Just (c, rem) | isNameCodePoint c -> consumeName rem (T.snoc acc c) (n + 1)
                                           | c == '\\'         -> (acc, n) -- TODO: properly handle an escape
                                           | otherwise         -> (acc, n)
                             Nothing -> (acc, n)




takeCharToken :: CssParser -> (CssParser, Maybe CssToken)
takeCharToken parser = if T.null . remainder $ parser
                       then (parser, Nothing)
                       else (parserMoveBy parser (T.singleton . T.head . remainder $ parser),
                             Just $ CssTokCh (T.head . remainder $ parser))




-- This function does not return a token. Discarding meaningless data from
-- beginning of text would not create a valid token.
takeLeadingWhite :: CssParser -> (CssParser, Maybe CssToken)
takeLeadingWhite parser
  | T.null rem                 = (parser, Just CssTokEnd)
  | D.C.isSpace . T.head $ rem = takeLeadingWhite parser { remainder = T.tail rem, spaceSeparated = True }
  | T.isPrefixOf "/*" rem      = takeLeadingWhite parser { remainder = HU.skipEnclosed rem "/*" "*/" }
  | T.isPrefixOf "<!--" rem    = takeLeadingWhite parser { remainder = HU.skipEnclosed rem "<!--" "-->" }
  | otherwise                  = (parser, Nothing)
  where rem = remainder parser




-- This function may complete withouth returning a valid token. Discarding
-- meaningless data from beginning of text would not create a valid token.
takeLeadingWhite2 :: CssParser -> (CssParser, Maybe CssToken)
takeLeadingWhite2 parser
  | T.null rem                 = (parser, Just CssTokEnd)
  | D.C.isSpace . T.head $ rem = takeLeadingWhite2 parser { remainder = T.tail rem, spaceSeparated = True }
  | T.isPrefixOf "/*" rem      = takeLeadingWhite2 parser { remainder = HU.skipEnclosed rem "/*" "*/" }
  | T.isPrefixOf "<!--" rem    = takeLeadingWhite2 parser { remainder = HU.skipEnclosed rem "<!--" "-->" }
  | otherwise                  = if (not . inBlock $ parser) && spaceSeparated parser
                                 then (parser, Just CssTokWS)
                                 else (parser, Nothing)
  where rem = remainder parser




-- Move parser's remainder by length of given string. Call this function when
-- givne string has been consumed to token and now you want to remove it from
-- front of parser's remainder.
parserMoveBy :: CssParser -> T.Text -> CssParser
parserMoveBy parser tok = parser { remainder = T.drop (T.length tok) (remainder parser) }




-- Try to interpret what comes after a <number-token> as <percentage-token>
-- or <dimension-token>.
tryTakingPercOrDim :: CssParser -> CssNum -> (CssParser, Maybe CssToken)
tryTakingPercOrDim numParser cssNum | (parser, Just (CssTokCh '%'))      <- takeCharToken numParser  = (parser, Just $ CssTokPerc cssNum)
                                    | (parser, Just (CssTokIdent ident)) <- takeIdentToken numParser = (parser, Just $ CssTokDim cssNum ident)
                                    | otherwise                                                      = (numParser, Nothing)




-- Take <number-token>, then try and see if what comes next in input string
-- allows to convert the <number-token> into <percentage-token> or
-- <dimension-token>. Return one of the three token types.
--
takeNumericToken :: CssParser -> (CssParser, Maybe CssToken)
takeNumericToken parser = case takeNumber parser of
                            (numParser, Just cssNum) -> (numTokenOrMore numParser cssNum)
                            otherwise                ->(parser, Nothing)

  where
    -- Use given CssNum to either create <number-token>, or (if data in
    -- parser allows it) to create <percentage-token> or <dimension-token>.
    numTokenOrMore :: CssParser -> CssNum -> (CssParser, Maybe CssToken)
    numTokenOrMore numParser cssNum = case tryTakingPercOrDim numParser cssNum of
                                        pair@(parser, Just token) -> pair
                                        -- Data in parser didn't allow creating other token, so just return <number-token>.
                                        (_, Nothing)              -> (numParser, Just $ CssTokNum cssNum)




-- Take a number: either float or integer. Don't take a unit that may or may
-- not follow the number - leave it to next function.
-- Try taking Float before trying to take Int, because otherwise you may take
-- only an initial (integral) part of Float as an Int, and leave fractional
-- part in remainder.
takeNumber :: CssParser -> (CssParser, Maybe CssNum)
takeNumber parser = wellFormedFloat parser >>? noStartingDigitFloat >>? takeInt




-- Parse a string of characters that represents a well formed float. "Well
-- formed" means with integral digit(s), e.g. "1.4". A "malformed" float (e.g.
-- ".5") is a special case that should be handled by noStartingDigitFloat.
wellFormedFloat :: CssParser -> (CssParser, Maybe CssNum)
wellFormedFloat parser = case T.R.signed T.R.rational (remainder parser) of
                           -- T.R.rational is happy to interpret "100" as float,
                           -- but we want to treat is as int and reject it.
                           -- Therefore we have to search for '.' in taken
                           -- sub-string :( Similarly we search for 'e' to
                           -- recognize a string that represents a float in
                           -- exponential notation.
                           Right (f, rem) -> case T.find (\c -> elem c ['.', 'e']) valString of
                                               Just c    -> (parser{remainder = rem}, Just $ CssNumF f)
                                               otherwise -> (parser, Nothing)
                             where
                               valString = T.take valLen $ remainder parser
                               valLen = (T.length . remainder $ parser) - (T.length rem)
                           Left _         -> (parser, Nothing)




-- Fix a string that is a malformed float (i.e. starting with a dot: ".7")
-- and try parsing it again. TODO: this function can't handle a case of sign
-- followed by dot: "-.4".
noStartingDigitFloat :: CssParser -> (CssParser, Maybe CssNum)
noStartingDigitFloat parser = if isFloatWithoutLeadingDot parser
                                 -- TODO: this adding character to remainder may break calculation of offset in input buffer.
                              then wellFormedFloat parser{remainder=(T.cons '0' (remainder parser))}
                              else (parser, Nothing)
  where
    -- TODO: this is one ugly function. This necessity to do two T.uncons
    -- calls is UGLY.
    isFloatWithoutLeadingDot parser =
      case T.uncons (remainder parser) of
        Just ('.', rem) -> case T.uncons (rem) of
                             -- Don't just rely on a dot, check if the dot
                             -- stands before a fractional part (good), or
                             -- before CSS selector (bad).
                             Just (d, rem2) -> d >= '0' && d <= '9'
                             otherwise      -> False
        otherwise       -> False




-- This function is very similar to takeFloat, but I don't want to write
-- a common function just yet. takeFloat will have to be updated to read
-- all formats of float value, and that change may make it more
-- complicated and less similar to takeInt.
takeInt :: CssParser -> (CssParser, Maybe CssNum)
takeInt parser = case T.R.signed T.R.decimal (remainder parser) of
                   Right (i, rem) -> (parser{remainder = rem}, Just $ CssNumI i)
                   Left _         -> (parser, Nothing)




