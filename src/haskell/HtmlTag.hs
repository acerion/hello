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

This file is derived from dillo-3.0.5/src/html.cc.
Copyright assignments from that file:
Copyright (C) 2005-2007 Jorge Arellano Cid <jcid@dillo.org>
-}




{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module HtmlTag(
  -- Only for tests
    htmlTagGetAttributeValue
  , htmlTagParseWholeTag
  , takeAttrNameAndValue
  , takeAttrName
  , takeTagName
  , takeAttrValue
  , parserDefault
  , TagParser(..)
  ) where




import Prelude
import Data.Char
import qualified Data.Text as T
import qualified Data.Map as M




data TagParser = TagParser {
    tagName   :: Maybe T.Text
  , currentAttrName  :: T.Text
  , currentAttrValue :: T.Text
  , attributes :: M.Map T.Text T.Text
  , remainder :: T.Text
  } deriving (Show)




parserDefault = TagParser {
    tagName   = Just ""
  , currentAttrName  = ""
  , currentAttrValue = ""
  , attributes = M.empty
  , remainder = ""
  }




{-
TODO: the following features of C implementation are missing in Haskell
implementation:

1. HTML entities in attribute values were evaluated and replaced with Unicode
codepoint. It seems reasonable to have it back, but not for all attributes.
The original implementation also evaluated strings in URLs that contained
'&something=val' string - it looks like HTML entity, but it's not. So
additional care must be taken when deciding which attribute values can
contain HTML entities.

2. Function getting value of attribute received additional argument that
decided if whitespaces were stripped from beginning and/or end of the value
string. Current Haskell implementation does not have this argument. Attribute
values are always stripped.

TODO: handling of escaped delimiter inside of attribute value
(e.g. alt='this won\'t be handled well') to be investigated.
-}




htmlTagGetAttributeValue :: T.Text -> T.Text -> Maybe T.Text
htmlTagGetAttributeValue text needle = case htmlTagParseWholeTag text of
                                         Nothing -> Nothing
                                         Just parser -> case M.lookup needle (attributes parser) of
                                           Nothing -> Nothing
                                           Just text -> Just text




-- Parse whole tag. Save tag's name and all of tag's attribute-name-value
-- pairs into parser.
--
-- Return Nothing if tag name can't be parsed.
htmlTagParseWholeTag :: T.Text -> Maybe TagParser
htmlTagParseWholeTag text =
  case tagName parser of
    Nothing -> Nothing
    otherwise -> Just (takeAllAttrNamesAndValues parser)
  where parser = takeTagName parserDefault text




takeAllAttrNamesAndValues :: TagParser -> TagParser
takeAllAttrNamesAndValues parser = case takeAttrNameAndValue parser of
                                     Just parser -> takeAllAttrNamesAndValues parser
                                     Nothing -> parser




takeTagName :: TagParser -> T.Text -> TagParser
takeTagName parser text =
  if T.isPrefixOf "<" text
  then parser { tagName = Just (fst pair), remainder = snd pair }
  else parser { tagName = Nothing, remainder = text }
  where
    pair = T.span pred (T.stripStart . T.tail $ text)
    -- TODO: improve the predicate. We should handle different cases like "< an invalid tag =".
    pred :: Char -> Bool
    pred c = Data.Char.isDigit c || Data.Char.isAsciiUpper c || Data.Char.isAsciiLower c




takeAttrNameAndValue :: TagParser -> Maybe TagParser
takeAttrNameAndValue parser =
  if currentAttrName parserWithName == "" || remainder parserWithName == "" -- Empty attribute name indicates end of parsing of tag. No more names (and their values) are available.
  then Nothing
  else Just parserWithValue { attributes = M.insert (currentAttrName parserWithValue) (currentAttrValue parserWithValue) (attributes parser) }
  where
    parserWithName = takeAttrName parser
    parserWithValue = takeAttrValue parserWithName




takeAttrName :: TagParser -> TagParser
takeAttrName parser = parser { currentAttrName = fixWhiteSpaces name,
                               remainder = if T.null restOfText -- There was no "=" sign in input text. (snd nameAndRemainder) may be empty text, so don't use T.tail on it.
                                           then restOfText
                                           else T.tail restOfText }
  where
    name = T.strip . fst $ pair
    restOfText = snd pair
    pair = T.span (\c -> not (elem c ['=', '/', '>'])) (remainder parser)




takeAttrValue :: TagParser -> TagParser
takeAttrValue parser = parser { currentAttrValue = T.strip . fixWhiteSpaces $ value
                               -- dropWhile is used to remove value delimiter
                               -- from what comes after the value (from the
                               -- remainer).
                              , remainder = T.strip (T.dropWhile (== delimiter) restOfText)
                              }
  where
    valueBegin = T.stripStart . remainder $ parser
    delimiter = case T.head valueBegin of
      '\'' -> '\''
      '"'  -> '"'
      otherwise -> ' '

    value = if delimiter == ' '
            then T.takeWhile pred valueBegin -- valueBegin is already stripped at the front, so we will drop until ending space is found
            else T.takeWhile pred (T.drop 1 valueBegin)

    restOfText = if delimiter == ' '
                 then T.drop (T.length value) valueBegin
                 else T.drop ((T.length value) + 1) valueBegin

    pred = (\c -> c /= delimiter && c /= '>') -- '>' indicates end of tag. TODO: Notice that "/>" is not recognized correctly here as end of tag.



fixWhiteSpaces :: T.Text -> T.Text
fixWhiteSpaces text = T.map (\c -> if c == '\t' then ' ' else if c == '\n' then ' ' else c) text
