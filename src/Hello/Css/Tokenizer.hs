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
TODO: be careful with decoding string from external representation into
Data.Text. My original attempt to convert C string into Data.Text in
ffi/Parser.hsc used T.E.decodeUtf8. I had to change it to T.E.decodeLatin1
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



module Hello.Css.Tokenizer
  (
    CssParser (..)
  , defaultParser
  , defaultParserInBlock
  , defaultParserEmpty
  , nextToken
  , startTokenizer
  , takeIdentToken

  , CssToken (..)
  , CssHashType (..)

  , CssNum (..)
  , cssNumToFloat

  , CssOrigin (..)

  , takeFloatString
  , expectSign
  , expectLeadingDigits
  , expectDot
  , expectFollowingDigits
  , expectExponent
  , peekUpToNCodePoints
  , isValidStartOfIdentifier
  , consumeEscapedCodePoint
  , consumeEscapedCodePointString
  , consumeName
  , removeDoubleWhitespaces
  , isWhitespace

  , splitAtCommaToken

  , parserTokenNum
  , parserTokenPerc
  , parserTokenIdentAny
  , parserTokenIdent
  , parserTokenIdentValue
  , parserTokenColon
  , parserTokenComma
  , parserTokenSemicolon
  , parserTokenParenOpen
  , parserTokenParenClose
  , parserTokenBraceCurlyOpen
  , parserTokenBraceCurlyClose
  , parserTokenDelim
  , parserTokenString
  , parserTokenStringValue
  , parserTokenWhitespace
  , parserTokenAtKeyword
  , parserTokenEnd

  , unsatisfy
  )
where




import qualified Data.Char as D.C
import qualified Data.Text as T
import qualified Data.Text.Read as T.R
--import Debug.Trace

import qualified Hello.Utils as HU
import Hello.Utils
import Hello.Utils.Parser
import qualified Hello.Unicode as H.U




data CssNum
  = CssNumI Int
  | CssNumF Float
  deriving (Show, Eq)




cssNumToFloat :: CssNum -> Float
cssNumToFloat (CssNumF f) = f
cssNumToFloat (CssNumI i) = fromIntegral i




-- Allowed values of type flag in <hash-token>
data CssHashType
  = CssHashUn    -- "unrestricted" (the default one)
  | CssHashId    -- "id"
  deriving (Show, Eq)




-- Tokens listed in https://www.w3.org/TR/css-syntax-3/#tokenization, but not
-- included in CssToken type (or not moved to a comment next to specific
-- value constructor below yet):
--
-- <string-token>, <bad-string-token>,
-- <whitespace-token>, <CDO-token>, <CDC-token>,
data CssToken =
    CssTokNum CssNum            -- <number-token>
  | CssTokPerc CssNum           -- <percentage-token>
  | CssTokDim CssNum T.Text     -- <dimension-token>

  -- Ident-like-tokens (https://www.w3.org/TR/css-syntax-3/#consume-ident-like-token):
  | CssTokIdent T.Text          -- <ident-token>; CSS3 spec says that text can be empty: "have a value composed of zero or more code points".
  | CssTokFunc T.Text           -- <function-token>
  | CssTokUrl T.Text            -- <url-token>
  | CssTokBadUrl                -- <bad-url-token>

  | CssTokColon                 -- <colon-token>
  | CssTokSemicolon             -- <semicolon-token>
  | CssTokComma                 -- <comma-token>

  | CssTokBraceSquareOpen       -- <[-token>
  | CssTokBraceSquareClose      -- <]-token>
  | CssTokParenOpen             -- <(-token>
  | CssTokParenClose            -- <)-token>
  | CssTokBraceCurlyOpen        -- <{-token>
  | CssTokBraceCurlyClose       -- <}-token>

  | CssTokHash CssHashType T.Text   -- <hash-token>; T.Text value is not prefixed by '#'.
  | CssTokStr T.Text
  | CssTokDelim Char            -- <delim-token>
  | CssTokWS                    -- Whitespace

  | CssTokAtKeyword T.Text      -- <at-keyword-token>

  | CssTokEnd                   -- End of input. No new tokens will appear in input.
  | CssTokNone                  -- No token was taken, proceed with parsing input data to try to take some token.
  deriving (Show, Eq)




-- TODO: add baseUrl field.
data CssParser = CssParser {
    remainder      :: T.Text
  , spaceSeparated :: Bool
  , inBlock        :: Bool
  , bufOffset      :: Int
  } deriving (Show, Eq)




-- Create a CSS parser with all fields set to empty values.
defaultParserEmpty :: CssParser
defaultParserEmpty = CssParser {
    remainder      = ""
  , inBlock        = False
  , spaceSeparated = False
  , bufOffset      = 0
  }




-- A basic onstructor for a CSS parser. It creates an almost-empty (initial)
-- parser and sets its remainder with value passed to the constructor.
defaultParser :: T.Text -> CssParser
defaultParser remd = defaultParserEmpty { remainder = remd }




-- Constructor for a CSS parser that knows that it is inside of {} block.
--
-- The constructor is useful for tests that test parsing e.g. CSS properties
-- strings, which are found inside of {} block.
--
-- Can be also used outside fo tests, where we need to have a new parser, and
-- the parser must know that we are now inside of {} block.
defaultParserInBlock :: T.Text -> CssParser
defaultParserInBlock remd = defaultParserEmpty { remainder = remd, inBlock = True }




-- Where does a rule come from?
data CssOrigin =
    CssOriginUserAgent -- = 0  -- Rule comes from User Agent. It is defined in program's source code.
  | CssOriginUser      -- = 1
  | CssOriginAuthor    -- = 2
  deriving (Show, Eq)




nextToken :: CssParser -> (CssParser, CssToken)
nextToken parserArg = (updatedParser{bufOffset = increasedBufOffset parserArg}, token)
  where
    (updatedParser, token) = case tokenAlternative parserArg{ spaceSeparated = False } of
                               (p, Just t)  -> (p, t)
                               (p, Nothing) -> (p, CssTokNone)
    increasedBufOffset parser = bufOffset parser + (T.length . remainder $ parser) - (T.length . remainder $ updatedParser)




-- Function used to kick-off a tokenization process, turning an initial tokenizer into (tokenizer, token) pair.
startTokenizer :: CssParser -> (CssParser, CssToken)
startTokenizer = nextToken




tokenAlternative :: CssParser -> (CssParser, Maybe CssToken)
tokenAlternative parser = takeLeadingWhite parser >>?
                          takeNumericToken        >>?
                          takeSingleCharToken     >>?
                          takeIdentLikeToken      >>?
                          takeString              >>?
                          takeHashToken           >>?
                          takeAtToken             >>?
                          takeDelimToken




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
                        then (parserMoveByLen parser len, Just $ CssTokIdent ident)
                        else (parser, Nothing)
  where
    (ident, len) = consumeName (remainder parser) "" 0




-- https://www.w3.org/TR/css-syntax-3/#check-if-three-code-points-would-start-an-identifier
-- TODO: write test for this function.
isValidStartOfIdentifier :: T.Text -> Bool
isValidStartOfIdentifier buffer | null points             = False
                                | c1 == '-'               = tryStartingWithHyphen points
                                | isNameStartCodePoint c1 = True
                                | c1 == '\\'              = length points >= 2 && isValidEscape c1 c2
                                | otherwise               = False
  where
    points = peekUpToNCodePoints buffer 3 (const True)

    tryStartingWithHyphen pts | length pts >= 2 && (isNameStartCodePoint c2 || c2 == '-') = True
                              | length pts >= 3 && isValidEscape c2 c3                    = True
                              | otherwise                                                 = False

    c1 = points !! 0
    c2 = points !! 1
    c3 = points !! 2




peekUpToNCodePoints :: T.Text -> Int -> (Char -> Bool) -> [Char]
peekUpToNCodePoints buffer nPoints predicate = peekUpToNCodePoints' buffer [] nPoints
  where
    peekUpToNCodePoints' :: T.Text -> [Char] -> Int -> [Char]
    peekUpToNCodePoints' _   acc 0 = acc
    peekUpToNCodePoints' buf acc n = case T.uncons buf of
                                       Just (c, remd) | predicate c -> peekUpToNCodePoints' remd (acc ++ [c]) (n - 1)
                                                      | otherwise   -> acc
                                       Nothing        -> acc




-- https://www.w3.org/TR/css-syntax-3/#starts-with-a-valid-escape
isValidEscape :: Char -> Char -> Bool
isValidEscape c1 c2 = c1 == '\\' && c2 /= '\n'




isNameStartCodePoint :: Char -> Bool
isNameStartCodePoint c = (D.C.isAlpha c && D.C.isAscii c) || isNonAscii c || c == '_'


-- https://www.w3.org/TR/css-syntax-3/#non-ascii-code-point
isNonAscii :: Char -> Bool
isNonAscii c = D.C.ord c >= 0x80


isNameCodePoint :: Char -> Bool
isNameCodePoint c = isNameStartCodePoint c || D.C.isDigit c || c == '-'



-- https://www.w3.org/TR/css-syntax-3/#consume-an-ident-like-token
-- TODO: make the algo more adhering to the spec.
takeIdentLikeToken :: CssParser -> (CssParser, Maybe CssToken)
takeIdentLikeToken p1 = if len == 0
                        then (p1, Nothing)
                        else takeIdentLikeToken' p2 name
  where
    (name, len) = consumeName (remainder p1) "" 0
    p2 = parserMoveByLen p1 len


takeIdentLikeToken' :: CssParser -> T.Text -> (CssParser, Maybe CssToken)
takeIdentLikeToken' p1 name =
  case T.uncons . remainder $ p1 of
    -- Opening paren disappears, it is not represented in list of output
    -- tokens.
    Just ('(', remd) -> if T.toLower name == "url"
                        then tryConsumingUrlToken p2 name -- Just try. It may succeed, or it may result in <function-token>.
                        else (p2, Just $ CssTokFunc name)
      where
        p2 = p1{ remainder = remd}
    _               -> (p1, Just $ CssTokIdent name)




-- Remove a leading whitespace from parser's remainder as long as the
-- remainder starts with two whitespaces.
--
-- TODO: this is not very effective. Just peek all whitespaces and them move parser by all-1.
removeDoubleWhitespaces :: CssParser -> CssParser
removeDoubleWhitespaces p1 = if length points == 2
                             then removeDoubleWhitespaces $ parserMoveByLen p1 1
                             else p1
  where
    points = peekUpToNCodePoints (remainder p1) 2 isWhitespace




tryConsumingUrlToken :: CssParser -> T.Text -> (CssParser, Maybe CssToken)
tryConsumingUrlToken p1 name | length points >= 1 && (c0 == '\'' || c0 == '\"')                    = (p2, Just $ CssTokFunc name)
                             | length points == 2 && isWhitespace c0 && (c1 == '\'' || c1 == '\"') = (p2, Just $ CssTokFunc name)
                             | otherwise = consumeUrlToken p2
  where
    p2 = removeDoubleWhitespaces p1
    points = peekUpToNCodePoints (remainder p2) 2 (const True)
    c0 = points !! 0
    c1 = points !! 1




consumeUrlToken :: CssParser -> (CssParser, Maybe CssToken)
consumeUrlToken p1 = if T.length text > 0 && T.last text == ')' -- TODO: shouldn't the ')' char be CssTokParenClose?
                     then (p2, Just $ CssTokUrl $ T.take (n - 1) text) -- Don't include closing paren.
                     else (p2, Just CssTokBadUrl)
  where
    p2 = p1 {remainder = T.drop n $ remainder p1}
    text = T.pack . reverse $ f (remainder p1) []
    n = T.length text

    f :: T.Text -> [Char] -> String
    f buffer acc = case T.uncons buffer of
                 Just (c, remd) | c == ')'  -> c:acc -- Include the paren here to recognize valid URL. TODO: Shouldn't the ')' char be CssParenClose?
                                | otherwise -> f remd (c:acc)
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
                      (Just string, remd) -> parseString parser string remd
                      (Nothing, _) -> case HU.takeEnclosed (remainder parser) "'" "'" True of
                                       (Just string, remd) -> parseString parser string remd
                                       (Nothing, _) -> (parser, Nothing)
  where
    parseString :: CssParser -> T.Text -> T.Text -> (CssParser, Maybe CssToken)
    parseString par string remd = (par{ remainder = remd }, Just $ CssTokStr (escapedString string))
    escapedString str = case T.findIndex (== '\\') str of
                          Just _  -> ""
                          Nothing -> str




-- Implementation of algorithm for hash token described in
-- https://www.w3.org/TR/css-syntax-3/#consume-token
takeHashToken :: CssParser -> (CssParser, Maybe CssToken)
takeHashToken p1 =
  case T.uncons $ remainder p1 of
    Just ('#', remd) | length points >= 1 && isNameCodePoint c0  -> createHashToken p1{ remainder = remd }
                     | length points >= 2 && isValidEscape c0 c1 -> createHashToken p1{ remainder = remd }
                     | otherwise                                 -> (p1{ remainder = remd }, Just $ CssTokDelim '#')
      where
        points = peekUpToNCodePoints remd 2 (const True)
        c0 = points !! 0
        c1 = points !! 1

        createHashToken p2 = if isValidStartOfIdentifier $ remainder p2
                             then (parserMoveByLen p2 len, Just $ CssTokHash CssHashId name)
                             else (parserMoveByLen p2 len, Just $ CssTokHash CssHashUn name)
          where
            (name, len) = consumeName (remainder p2) "" 0
    _ -> (p1, Nothing)




takeAtToken :: CssParser -> (CssParser, Maybe CssToken)
takeAtToken p1 =
  case T.uncons $ remainder p1 of
    Just ('@', remd) | length points == 3 && isValidStartOfIdentifier (T.pack points) -> createAtToken p1{remainder = remd }
                     | otherwise                                                      -> (p1{ remainder = remd }, Just $ CssTokDelim '@')
      where
        points           = peekUpToNCodePoints remd 3 (const True)
        createAtToken p2 = (parserMoveByLen p2 len, Just $ CssTokAtKeyword name)
          where
            (name, len) = consumeName (remainder p2) "" 0

    _ -> (p1, Nothing)




-- https://www.w3.org/TR/css-syntax-3/#consume-a-name
--
-- Returns pair (name, count), where count is a number of consumed code
-- points.
consumeName :: T.Text -> T.Text -> Int -> (T.Text, Int)
consumeName buffer acc n = case T.uncons buffer of
                             Just (c, remr) | isNameCodePoint c -> consumeName remr              (T.snoc acc c)  (n + 1)
                                            | c == '\\'         -> consumeName (T.drop len remr) (T.snoc acc ec) (n + len + 1)
                                            | otherwise         -> (acc, n)
                               where
                                 (ec, len) = consumeEscapedCodePoint remr
                             Nothing -> (acc, n)





-- https://www.w3.org/TR/css-syntax-3/#consume-an-escaped-code-point
consumeEscapedCodePoint :: T.Text -> (Char, Int)
consumeEscapedCodePoint buf = case T.uncons buf of
                                Just (c, _) | D.C.isHexDigit c -> consumeEscapedCodePointString buf
                                            -- EOF case from CSS spec is handled by "Nothing" below.
                                            | otherwise        -> (c, 1)
                                Nothing -> (D.C.chr H.U.replacementCharacter, 0)  -- "This is a parse error. Return U+FFFD REPLACEMENT CHARACTER (�)."




-- TODO: this function should also consume a whitespace if it exists after
-- the hex digits: "If the next input code point is whitespace, consume it as
-- well.".
consumeEscapedCodePointString :: T.Text -> (Char, Int)
consumeEscapedCodePointString buf = (char, len)
  where
    len = length digits
    digits = peekUpToNCodePoints buf 6 D.C.isHexDigit
    char = D.C.chr $ case T.R.hexadecimal . T.pack $ digits of
                       Right (d, _) | d == 0                           -> H.U.replacementCharacter
                                    | d >= H.U.maximumAllowedCodePoint -> H.U.replacementCharacter
                                    | H.U.isSurrogate d                -> H.U.replacementCharacter
                                    | otherwise                        -> d
                       Left _ -> H.U.replacementCharacter -- TODO: is it the best choice to use Replacement Character here?




takeDelimToken :: CssParser -> (CssParser, Maybe CssToken)
takeDelimToken parser = case T.uncons . remainder $ parser of
                          Just (c, remd) -> (parser{ remainder = remd }, Just $ CssTokDelim c)
                          Nothing        -> (parser, Nothing)




-- This function may complete without returning a valid token. Discarding
-- meaningless data from beginning of text would not create a valid token.
takeLeadingWhite :: CssParser -> (CssParser, Maybe CssToken)
takeLeadingWhite parser
  | T.null remd                 = (parser, Just CssTokEnd)
  | D.C.isSpace . T.head $ remd = takeLeadingWhite parser { remainder = T.tail remd, spaceSeparated = True }
  | T.isPrefixOf "/*" remd      = takeLeadingWhite parser { remainder = HU.skipEnclosed remd "/*" "*/" }
  | T.isPrefixOf "<!--" remd    = takeLeadingWhite parser { remainder = HU.skipEnclosed remd "<!--" "-->" }
  | otherwise                   = if (not . inBlock $ parser) && spaceSeparated parser
                                  then (parser, Just CssTokWS)
                                  else (parser, Nothing)
  where remd = remainder parser




-- https://www.w3.org/TR/css-syntax-3/#whitespace
--
-- TODO: CARRIAGE RETURN and FORM FEED should be converted to LINE FEED
-- during preprocessing of input stream.
isWhitespace :: Char -> Bool
isWhitespace c = c `elem` ['\n', '\r', '\f', '\t', ' ']



{-
-- Move parser's remainder by length of given string. Call this function when
-- givne string has been consumed to token and now you want to remove it from
-- front of parser's remainder.
parserMoveByString :: CssParser -> T.Text -> CssParser
parserMoveByString parser tok = parser { remainder = T.drop (T.length tok) (remainder parser) }
-}



-- Move parser's remainder by given explicit length.
parserMoveByLen :: CssParser -> Int -> CssParser
parserMoveByLen parser len = parser { remainder = T.drop len (remainder parser) }




-- Try to interpret what comes after a <number-token> as <percentage-token>
-- or <dimension-token>.
tryTakingPercOrDim :: CssParser -> CssNum -> (CssParser, Maybe CssToken)
tryTakingPercOrDim numParser cssNum | (parser, Just (CssTokDelim '%'))   <- takeDelimToken numParser  = (parser, Just $ CssTokPerc cssNum)
                                    | (parser, Just (CssTokIdent ident)) <- takeIdentToken numParser  = (parser, Just $ CssTokDim cssNum ident)
                                    | otherwise                                                       = (numParser, Nothing)




-- Take <number-token>, then try and see if what comes next in input string
-- allows to convert the <number-token> into <percentage-token> or
-- <dimension-token>. Return one of the three token types.
--
takeNumericToken :: CssParser -> (CssParser, Maybe CssToken)
takeNumericToken parser = case takeNumber parser of
                            (numParser, Just cssNum) -> numTokenOrMore numParser cssNum
                            _                        -> (parser, Nothing)

  where
    -- Use given CssNum to either create <number-token>, or (if data in
    -- parser allows it) to create <percentage-token> or <dimension-token>.
    numTokenOrMore :: CssParser -> CssNum -> (CssParser, Maybe CssToken)
    numTokenOrMore numParser cssNum = case tryTakingPercOrDim numParser cssNum of
                                        pair@(_, Just _) -> pair
                                        -- Data in parser didn't allow creating other token, so just return <number-token>.
                                        (_, Nothing)     -> (numParser, Just $ CssTokNum cssNum)




-- Take a number: either float or integer. Don't take a unit that may or may
-- not follow the number - leave it to next function.
-- Try taking Float before trying to take Int, because otherwise you may take
-- only an initial (integral) part of Float as an Int, and leave fractional
-- part in remainder.
takeNumber :: CssParser -> (CssParser, Maybe CssNum)
takeNumber parser = takeFloat parser >>? takeInt




-- CSS defines number-token (non-normative illustration) like this:
-- https://www.w3.org/TR/css-syntax-3/#number-token-diagram
--
-- Haskell reader/readers can deal with only a subset of possible CSS float
-- numbers. It can't parse correctly tokens such as ".03" (no leading digit)
-- or "+.4" (no leading digit, and a sign directly before dot).
--
-- Therefore I have implemented a function that converts all valid CSS number
-- strings into something parsable by Haskell reader functions.
--
-- Sure, there is probably some parser there that can deal with this problem
-- in 10 lines of code, but I'm at the stage of a project where I don't use
-- external libs too much (yet). Hence I'm using this "manual" parsing for
-- now.
takeFloatString :: (T.Text, T.Text) -> Maybe (T.Text, T.Text)
takeFloatString (buf, acc) = expectSign (buf, acc) >>!
                             expectLeadingDigits   >>!
                             expectDot             >>!
                             expectFollowingDigits >>!
                             expectExponent




expectSign :: (T.Text, T.Text) -> Maybe (T.Text, T.Text)
expectSign (buf, acc) = case T.uncons buf of
                          Just ('+', remd) -> Just (remd, T.concat [acc, "+"])
                          Just ('-', remd) -> Just (remd, T.concat [acc, "-"])

                          -- Assume positive on other chars. TODO: we expect
                          -- sign here, but we are ok with non-sign, is this
                          -- ok. Also do we need to append the explicit '+'
                          -- in this case?
                          Just (_,   _)    -> Just (buf, T.concat [acc, "+"])
                          _                -> Nothing




expectLeadingDigits :: (T.Text, T.Text) -> Maybe (T.Text, T.Text)
expectLeadingDigits (buf, acc) = case T.uncons buf of
                                   Just ('.', _) -> Just (buf, T.concat [acc, "0"])
                                   Just (d,   _) -> tryTakingDigits (buf, acc) d
                                   _             -> Nothing




expectDot :: (T.Text, T.Text) -> Maybe (T.Text, T.Text)
expectDot (buf, acc) = case T.uncons buf of
                         Just ('.', remd) -> Just (remd, T.concat [acc, "."])
                         _                -> Just (buf, acc) -- No dot, but maybe it's an exponential notation.




expectFollowingDigits :: (T.Text, T.Text) -> Maybe (T.Text, T.Text)
expectFollowingDigits (buf, acc) = case T.uncons buf of
                                     Just (d, _) -> tryTakingDigits (buf, acc) d
                                     _           -> Just (buf, acc)




tryTakingDigits :: (T.Text, T.Text) -> Char -> Maybe (T.Text, T.Text)
tryTakingDigits (buf, acc) d = if D.C.isDigit d
                               then Just (T.drop len buf, T.concat [acc, digits])
                               else Just (buf, acc)
  where
    digits = T.takeWhile D.C.isDigit buf
    len    = T.length digits




requestFollowingDigits :: (T.Text, T.Text) -> Maybe (T.Text, T.Text)
requestFollowingDigits (buf, acc) = case T.uncons buf of
                                      Just (d, _) -> requestDigits (buf, acc) d
                                      Nothing     -> Nothing



requestDigits :: (T.Text, T.Text) -> Char -> Maybe (T.Text, T.Text)
requestDigits (buf, acc) d = if D.C.isDigit d
                             then Just (T.drop len buf, T.concat [acc, digits])
                             else Nothing
  where
    digits = T.takeWhile D.C.isDigit buf
    len    = T.length digits




expectExponent :: (T.Text, T.Text) -> Maybe (T.Text, T.Text)
expectExponent (buf1, acc1) =
  case T.uncons buf1 of
    Nothing       -> Just (buf1, acc1) -- Probably string ending at fractional part of float (e.g. "1.21<NUL>").
    Just (c, remd) -> if c == 'e' || c == 'E'
                      then case expectSign (remd, T.concat [acc1, "e"]) of
                             Nothing           -> Just (buf1, acc1) -- No character after 'e', so ignore the 'e' ('e' is part of next token).
                             Just (buf2, acc2) -> case requestFollowingDigits (buf2, acc2) of
                                                    Nothing           -> Just (buf1, acc1) -- Roll back to buf/acc that existed before hypothetical exponent.
                                                    Just (buf3, acc3) -> Just (buf3, acc3)
                      else Just (buf1, acc1) -- No exponent part, but there is another token after float.




takeFloat :: CssParser -> (CssParser, Maybe CssNum)
takeFloat parser = case takeFloatString (remainder parser, "") of
                     Nothing                  -> (parser, Nothing)
                     Just (remd, floatString) -> case interpretFloatString floatString of
                                                   Nothing  -> (parser, Nothing)
                                                   Just num -> (parser{ remainder = remd }, Just num)




interpretFloatString :: T.Text -> Maybe CssNum
interpretFloatString buf = case T.R.signed T.R.rational buf of
                             -- T.R.rational is happy to interpret "100" as float,
                             -- but we want to treat is as int and reject it.
                             -- Therefore we have to search for '.' in taken
                             -- sub-string :( Similarly we search for 'e' to
                             -- recognize a string that represents a float in
                             -- exponential notation.
                             Right (f, remd) -> case T.find (\c -> c `elem` ['.', 'e']) valString of
                                                  Just _  -> Just $ CssNumF f
                                                  Nothing -> Nothing
                               where
                                 valString = T.take valLen buf
                                 valLen = T.length buf - T.length remd
                             Left _         -> Nothing




takeInt :: CssParser -> (CssParser, Maybe CssNum)
takeInt parser = case T.R.signed T.R.decimal (remainder parser) of
                   Right (i, remd) -> (parser{remainder = remd}, Just $ CssNumI i)
                   Left _          -> (parser, Nothing)




-- Take a token that consists of a single char. Simple cases of tokenizations
-- from https://www.w3.org/TR/css-syntax-3/#consume-token that consist only
-- of unconditional "return a X token".
--
-- Not all such tokens may be returned by this function yet.
--
-- This function differs from takeDelimToken in the fact that takeDelimToken
-- returns a "char token with some character in it". The function below
-- returns a distinct token for each successfully consumed character.
takeSingleCharToken :: CssParser -> (CssParser, Maybe CssToken)
takeSingleCharToken parser = case T.uncons $ remainder parser of
                               Just (':', remd) -> (parser{ remainder = remd }, Just CssTokColon)
                               Just (';', remd) -> (parser{ remainder = remd }, Just CssTokSemicolon)
                               Just (',', remd) -> (parser{ remainder = remd }, Just CssTokComma)
                               Just ('[', remd) -> (parser{ remainder = remd }, Just CssTokBraceSquareOpen)
                               Just (']', remd) -> (parser{ remainder = remd }, Just CssTokBraceSquareClose)
                               Just ('(', remd) -> (parser{ remainder = remd }, Just CssTokParenOpen)
                               Just (')', remd) -> (parser{ remainder = remd }, Just CssTokParenClose)
                               Just ('{', remd) -> (parser{ remainder = remd }, Just CssTokBraceCurlyOpen)
                               Just ('}', remd) -> (parser{ remainder = remd }, Just CssTokBraceCurlyClose)
                               _                -> (parser, Nothing)




splitAtCommaToken :: [CssToken] -> [[CssToken]] -> [[CssToken]]
splitAtCommaToken [] acc               = reverse acc
splitAtCommaToken (CssTokComma:xs) acc = splitAtCommaToken xs ([]:acc)
splitAtCommaToken (x:xs)       (a:acc) = splitAtCommaToken xs ((a ++ [x]):acc)
splitAtCommaToken (x:xs)       []      = splitAtCommaToken xs [[x]]




satisfy :: (CssParser, CssToken) -> CssToken -> Maybe ((CssParser, CssToken), CssToken)
satisfy (parser, token) needed = if token == needed
                                 then Just (nextToken parser, token)
                                 else Nothing




unsatisfy :: (CssParser, CssToken) -> CssToken -> Maybe ((CssParser, CssToken), CssToken)
unsatisfy (parser, token) needed = if token /= needed
                                   then Just (nextToken parser, token)
                                   else Nothing




parserTokenNum :: Parser (CssParser, CssToken) CssToken
parserTokenNum = Parser $ \ (parser, token) -> case token of
                                                 CssTokNum _ -> Just (nextToken parser, token)
                                                 _           -> Nothing




parserTokenPerc :: Parser (CssParser, CssToken) CssToken
parserTokenPerc = Parser $ \ (parser, token) -> case token of
                                                  CssTokPerc _ -> Just (nextToken parser, token)
                                                  _            -> Nothing




parserTokenIdentAny :: Parser (CssParser, CssToken) CssToken
parserTokenIdentAny = Parser $ \ (parser, token) -> case token of
                                                      CssTokIdent _ -> Just (nextToken parser, token)
                                                      _             -> Nothing




parserTokenIdent :: T.Text -> Parser (CssParser, CssToken) CssToken
parserTokenIdent ident = Parser $ \ (parser, token) -> case token of
                                                         CssTokIdent i | i == ident -> Just (nextToken parser, token)
                                                                       | otherwise  -> Nothing
                                                         _             -> Nothing




-- Parse an Ident token. If success, return string stored in the token.
parserTokenIdentValue :: Parser (CssParser, CssToken) T.Text
parserTokenIdentValue = Parser $ \ (parser, token) -> case token of
                                                        CssTokIdent str -> Just (nextToken parser, str)
                                                        _               -> Nothing




parserTokenColon :: Parser (CssParser, CssToken) CssToken
parserTokenColon = Parser $ \ (parser, token) -> case token of
                                                   CssTokColon -> Just (nextToken parser, token)
                                                   _           -> Nothing



parserTokenComma :: Parser (CssParser, CssToken) CssToken
parserTokenComma = Parser $ \ (parser, token) -> case token of
                                                   CssTokComma -> Just (nextToken parser, token)
                                                   _           -> Nothing




parserTokenSemicolon :: Parser (CssParser, CssToken) CssToken
parserTokenSemicolon = Parser $ \ (parser, token) -> case token of
                                                       CssTokSemicolon -> Just (nextToken parser, token)
                                                       _               -> Nothing



parserTokenParenOpen :: Parser (CssParser, CssToken) CssToken
parserTokenParenOpen = Parser $ \ pat -> satisfy pat CssTokParenOpen




parserTokenParenClose :: Parser (CssParser, CssToken) CssToken
parserTokenParenClose = Parser $ \ pat -> satisfy pat CssTokParenClose




parserTokenBraceCurlyOpen :: Parser (CssParser, CssToken) CssToken
parserTokenBraceCurlyOpen = Parser $ \ (parser, token) -> case token of
                                                            CssTokBraceCurlyOpen -> Just (nextToken parser, token)
                                                            _                    -> Nothing




parserTokenBraceCurlyClose :: Parser (CssParser, CssToken) CssToken
parserTokenBraceCurlyClose = Parser $ \ (parser, token) -> case token of
                                                             CssTokBraceCurlyClose -> Just (nextToken parser, token)
                                                             _                     -> Nothing




parserTokenDelim :: Char -> Parser (CssParser, CssToken) CssToken
parserTokenDelim delim = Parser $ \ (parser, token) -> case token of
                                                         CssTokDelim d | d == delim -> Just (nextToken parser, token)
                                                                       | otherwise  -> Nothing
                                                         _             -> Nothing




parserTokenString :: Parser (CssParser, CssToken) CssToken
parserTokenString = Parser $ \ (parser, token) -> case token of
                                                    CssTokStr _ -> Just (nextToken parser, token)
                                                    _           -> Nothing




-- Parse a String token. If success, return string stored in the token.
parserTokenStringValue :: Parser (CssParser, CssToken) T.Text
parserTokenStringValue = Parser $ \ (parser, token) -> case token of
                                                         CssTokStr str -> Just (nextToken parser, str)
                                                         _             -> Nothing




parserTokenWhitespace :: Parser (CssParser, CssToken) CssToken
parserTokenWhitespace = Parser $ \ (parser, token) -> case token of
                                                        CssTokWS -> Just (nextToken parser, token)
                                                        _        -> Nothing




parserTokenAtKeyword :: T.Text -> Parser (CssParser, CssToken) CssToken
parserTokenAtKeyword keyword = Parser $ \ (parser, token) -> case token of
                                                               CssTokAtKeyword x | x == keyword -> Just (nextToken parser, token)
                                                                                 | otherwise    -> Nothing
                                                               _ -> Nothing




parserTokenEnd :: Parser (CssParser, CssToken) CssToken
parserTokenEnd = Parser $ \ (parser, token) -> case token of
                                                 CssTokEnd -> Just (nextToken parser, token)
                                                 _         -> Nothing



