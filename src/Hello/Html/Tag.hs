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

This file is derived from dillo-3.0.5/src/html.cc.
Copyright assignments from that file:
Copyright (C) 2005-2007 Jorge Arellano Cid <jcid@dillo.org>
-}




{-# LANGUAGE OverloadedStrings #-}




module Hello.Html.Tag
  (
    -- Only for tests
    htmlAttributeGetValue
  , htmlTagParseWholeTag
  , takeAttrNameAndValue
  , takeAttrName
  , takeTagName
  , takeAttrValue
  , parserDefault
  , htmlTagIndex
  , htmlTagIndex2
  , TagParser(..)
  )
where




import Prelude
import Data.Char
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Vector as V
--import Debug.Trace




htmlTagInfo = V.fromList [
    ("a") --,                 B8(011101), 'R', 2, Html_tag_open_a,           NULL,                        Html_tag_close_a},
  , ("abbr") --,              B8(010101), 'R', 2, Html_tag_open_abbr,        NULL,                        NULL)
  -- acronym 010101 -- obsolete in HTML5
  , ("address") --,           B8(010110), 'R', 2, Html_tag_open_default,     NULL,                        Html_tag_close_par)
  , ("area") --,              B8(010001), 'F', 0, Html_tag_open_default,     Html_tag_content_area,       NULL)
  , ("article") --,           B8(011110), 'R', 2, Html_tag_open_sectioning,  NULL,                        NULL)
  , ("aside") --,             B8(011110), 'R', 2, Html_tag_open_sectioning,  NULL,                        NULL)
  , ("audio") --,             B8(011101), 'R', 2, Html_tag_open_audio,       NULL,                        Html_tag_close_media)
  , ("b") --,                 B8(010101), 'R', 2, Html_tag_open_default,     NULL,                        NULL)
  , ("base") --,              B8(100001), 'F', 0, Html_tag_open_base,        NULL,                        NULL)
  -- basefont 010001 -- obsolete in HTML5
  -- bdo 010101
  , ("big") --,               B8(010101), 'R', 2, Html_tag_open_default,     NULL,                        NULL)
  , ("blockquote") --,        B8(011110), 'R', 2, Html_tag_open_blockquote,  NULL,                        NULL)
  , ("body") --,              B8(011110), 'O', 1, Html_tag_open_body,        NULL,                        Html_tag_close_body)
  , ("br") --,                B8(010001), 'F', 0, Html_tag_open_default,     Html_tag_content_br,         NULL)
  , ("button") --,            B8(011101), 'R', 2, Html_tag_open_button,      NULL,                        Html_tag_close_button)
  -- caption
  , ("center") --,            B8(011110), 'R', 2, Html_tag_open_default,     NULL,                        NULL)
  , ("cite") --,              B8(010101), 'R', 2, Html_tag_open_default,     NULL,                        NULL)
  , ("code") --,              B8(010101), 'R', 2, Html_tag_open_default,     NULL,                        NULL)
  -- col 010010 'F'
  -- colgroup
  , ("dd") --,                B8(011110), 'O', 1, Html_tag_open_dd,          NULL,                        NULL)
  , ("del") --,               B8(011101), 'R', 2, Html_tag_open_default,     NULL,                        NULL)
  , ("dfn") --,               B8(010101), 'R', 2, Html_tag_open_default,     NULL,                        NULL)
  , ("dir") --,               B8(011010), 'R', 2, Html_tag_open_dir,         NULL,                        Html_tag_close_par)
  -- TODO: complete <div> support!
  , ("div") --,               B8(011110), 'R', 2, Html_tag_open_div,         NULL,                        NULL)
  , ("dl") --,                B8(011010), 'R', 2, Html_tag_open_dl,          NULL,                        Html_tag_close_par)
  , ("dt") --,                B8(010110), 'O', 1, Html_tag_open_dt,          NULL,                        Html_tag_close_par)
  , ("em") --,                B8(010101), 'R', 2, Html_tag_open_default,     NULL,                        NULL)
  , ("embed") --,             B8(010001), 'F', 0, Html_tag_open_embed,       Html_tag_content_embed,      NULL)
  -- fieldset
  , ("figcaption") --,        B8(011110), 'R', 2, Html_tag_open_default,     NULL,                        NULL)
  , ("figure") --,            B8(011110), 'R', 2, Html_tag_open_default,     NULL,                        NULL)
  , ("font") --,              B8(010101), 'R', 2, Html_tag_open_font,        NULL,                        NULL)
  , ("footer") --,            B8(011110), 'R', 2, Html_tag_open_sectioning,  NULL,                        NULL)
  , ("form") --,              B8(011110), 'R', 2, Html_tag_open_form,        NULL,                        Html_tag_close_form)
  , ("frame") --,             B8(010010), 'F', 0, Html_tag_open_frame,       Html_tag_content_frame,      NULL)
  , ("frameset") --,          B8(011110), 'R', 2, Html_tag_open_default,     Html_tag_content_frameset,   NULL)
  , ("h1") --,                B8(010110), 'R', 2, Html_tag_open_h,           NULL,                        NULL)
  , ("h2") --,                B8(010110), 'R', 2, Html_tag_open_h,           NULL,                        NULL)
  , ("h3") --,                B8(010110), 'R', 2, Html_tag_open_h,           NULL,                        NULL)
  , ("h4") --,                B8(010110), 'R', 2, Html_tag_open_h,           NULL,                        NULL)
  , ("h5") --,                B8(010110), 'R', 2, Html_tag_open_h,           NULL,                        NULL)
  , ("h6") --,                B8(010110), 'R', 2, Html_tag_open_h,           NULL,                        NULL)
  , ("head") --,              B8(101101), 'O', 1, Html_tag_open_head,        NULL,                        Html_tag_close_head)
  , ("header") --,            B8(011110), 'R', 2, Html_tag_open_sectioning,  NULL,                        NULL)
  , ("hr") --,                B8(010010), 'F', 0, Html_tag_open_hr,          Html_tag_content_hr,         NULL)
  , ("html") --,              B8(001110), 'O', 1, Html_tag_open_html,        NULL,                        Html_tag_close_html)
  , ("i") --,                 B8(010101), 'R', 2, Html_tag_open_default,     NULL,                        NULL)
  , ("iframe") --,            B8(011110), 'R', 2, Html_tag_open_frame,       Html_tag_content_frame,      NULL)
  , ("img") --,               B8(010001), 'F', 0, Html_tag_open_img,         Html_tag_content_img,        NULL)
  , ("input") --,             B8(010001), 'F', 0, Html_tag_open_input,       NULL,                        NULL)
  , ("ins") --,               B8(011101), 'R', 2, Html_tag_open_default,     NULL,                        NULL)
  , ("isindex") --,           B8(110001), 'F', 0, Html_tag_open_isindex,     NULL,                        NULL)
  , ("kbd") --,               B8(010101), 'R', 2, Html_tag_open_default,     NULL,                        NULL)
  -- label 010101
  -- legend 01??
  , ("li") --,                B8(011110), 'O', 1, Html_tag_open_li,          NULL,                        Html_tag_close_li)
  , ("link") --,              B8(100001), 'F', 0, Html_tag_open_link,        NULL,                        NULL)
  , ("map") --,               B8(011001), 'R', 2, Html_tag_open_default,     Html_tag_content_map,        Html_tag_close_map)
  , ("mark") --,              B8(010101), 'R', 2, Html_tag_open_default,     NULL,                        NULL)
  -- menu 1010 -- TODO: not exactly 1010, it can contain LI and inline
  , ("menu") --,              B8(011010), 'R', 2, Html_tag_open_menu,        NULL,                        Html_tag_close_par)
  , ("meta") --,              B8(110001), 'F', 0, Html_tag_open_meta,        NULL,                        NULL)
  , ("nav") --,               B8(011110), 'R', 2, Html_tag_open_sectioning,  NULL,                        NULL)
  -- noframes 1011 -- obsolete in HTML5
  -- noscript 1011
  , ("object") --,            B8(111101), 'R', 2, Html_tag_open_object,      Html_tag_content_object,     NULL)
  , ("ol") --,                B8(011010), 'R', 2, Html_tag_open_ol,          NULL,                        NULL)
  , ("optgroup") --,          B8(010101), 'O', 1, Html_tag_open_optgroup,    NULL,                        Html_tag_close_optgroup)
  , ("option") --,            B8(010001), 'O', 0, Html_tag_open_option,      NULL,                        Html_tag_close_option)
  , ("p") --,                 B8(010110), 'O', 1, Html_tag_open_p,           NULL,                        NULL)
  -- param 010001 'F'
  , ("pre") --,               B8(010110), 'R', 2, Html_tag_open_pre,         NULL,                        Html_tag_close_pre)
  , ("q") --,                 B8(010101), 'R', 2, Html_tag_open_q,           NULL,                        Html_tag_close_q)
  , ("s") --,                 B8(010101), 'R', 2, Html_tag_open_default,     NULL,                        NULL)
  , ("samp") --,              B8(010101), 'R', 2, Html_tag_open_default,     NULL,                        NULL)
  , ("script") --,            B8(111001), 'R', 2, Html_tag_open_script,      NULL,                        Html_tag_close_script)
  , ("section") --,           B8(011110), 'R', 2, Html_tag_open_sectioning,  NULL,                        NULL)
  , ("select") --,            B8(010101), 'R', 2, Html_tag_open_select,      NULL,                        Html_tag_close_select)
  , ("small") --,             B8(010101), 'R', 2, Html_tag_open_default,     NULL,                        NULL)
  , ("source") --,            B8(010001), 'F', 0, Html_tag_open_source,      Html_tag_content_source,     NULL)
  , ("span") --,              B8(010101), 'R', 2, Html_tag_open_span,        NULL,                        NULL)
  , ("strike") --,            B8(010101), 'R', 2, Html_tag_open_default,     NULL,                        NULL)
  , ("strong") --,            B8(010101), 'R', 2, Html_tag_open_default,     NULL,                        NULL)
  , ("style") --,             B8(100101), 'R', 2, Html_tag_open_style,       NULL,                        Html_tag_close_style)
  , ("sub") --,               B8(010101), 'R', 2, Html_tag_open_default,     NULL,                        NULL)
  , ("sup") --,               B8(010101), 'R', 2, Html_tag_open_default,     NULL,                        NULL)
  , ("table") --,             B8(011010), 'R', 5, Html_tag_open_table,       Html_tag_content_table,      NULL)
  -- tbody
  , ("td") --,                B8(011110), 'O', 3, Html_tag_open_td,          Html_tag_content_td,         NULL)
  , ("textarea") --,          B8(010101), 'R', 2, Html_tag_open_textarea,    Html_tag_content_textarea,   Html_tag_close_textarea)
  -- tfoot
  , ("th") --,                B8(011110), 'O', 1, Html_tag_open_th,          Html_tag_content_th,         NULL)
  -- thead
  , ("title") --,             B8(100101), 'R', 2, Html_tag_open_title,       NULL,                        Html_tag_close_title)
  , ("tr") --,                B8(011010), 'O', 4, Html_tag_open_tr,          Html_tag_content_tr,         NULL)
  , ("tt") --,                B8(010101), 'R', 2, Html_tag_open_default,     NULL,                        NULL)
  , ("u") --,                 B8(010101), 'R', 2, Html_tag_open_default,     NULL,                        NULL)
  , ("ul") --,                B8(011010), 'R', 2, Html_tag_open_ul,          NULL,                        NULL)
  , ("var") --,               B8(010101), 'R', 2, Html_tag_open_default,     NULL,                        NULL)
  , ("video") --,             B8(011101), 'R', 2, Html_tag_open_video,       NULL,                        Html_tag_close_media)
  , ("wbr") --,               B8(010101), 'F', 0, Html_tag_open_default,     Html_tag_content_wbr,        NULL}
  ] :: V.Vector (T.Text)


data TagParser = TagParser {
    tagName   :: Maybe T.Text
  , currentAttrName  :: T.Text
  , currentAttrValue :: T.Text
  , attributes :: M.Map T.Text T.Text
  , htmlRemainder :: T.Text
  } deriving (Show)




parserDefault = TagParser {
    tagName   = Just ""
  , currentAttrName  = ""
  , currentAttrValue = ""
  , attributes = M.empty
  , htmlRemainder = ""
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




htmlAttributeGetValue :: T.Text -> T.Text -> Maybe T.Text
htmlAttributeGetValue text needle = case htmlTagParseWholeTag text of
                                      Nothing     -> Nothing
                                      Just parser -> case M.lookup needle (attributes parser) of
                                                       Nothing  -> Nothing
                                                       Just txt -> Just txt




-- Parse whole tag. Save tag's name and all of tag's attribute-name-value
-- pairs into parser.
--
-- Return Nothing if tag name can't be parsed.
htmlTagParseWholeTag :: T.Text -> Maybe TagParser
htmlTagParseWholeTag text =
  case tagName parser of
    Nothing -> Nothing
    _       -> Just (takeAllAttrNamesAndValues parser)
  where parser = takeTagName parserDefault text




takeAllAttrNamesAndValues :: TagParser -> TagParser
takeAllAttrNamesAndValues parser = case takeAttrNameAndValue parser of
                                     Just parser' -> takeAllAttrNamesAndValues parser'
                                     Nothing      -> parser




takeTagName :: TagParser -> T.Text -> TagParser
takeTagName parser text =
  if T.isPrefixOf "<" text
  then parser { tagName = Just (fst pair), htmlRemainder = snd pair }
  else parser { tagName = Nothing, htmlRemainder = text }
  where
    pair = T.span predicate (T.stripStart . T.tail $ text)
    -- TODO: improve the predicate. We should handle different cases like "< an invalid tag =".
    predicate :: Char -> Bool
    predicate c = Data.Char.isDigit c || Data.Char.isAsciiUpper c || Data.Char.isAsciiLower c




-- TODO: this function and the handling of attr name/value needs to be
-- re-written from scratch taking into account the possibility that an
-- attribute with empty value and without '=' is valid (see "<td nowrap>"
-- case).
takeAttrNameAndValue :: TagParser -> Maybe TagParser
takeAttrNameAndValue parser | currentAttrName parserWithName == "" = Nothing  -- Empty attribute name indicates end of parsing of tag. No more names (and their values) are available.
                            | htmlRemainder parserWithName   == "" = Just parserWithName  { attributes = M.insert (currentAttrName parserWithValue) "" (attributes parser) }
                            | otherwise                            = Just parserWithValue { attributes = M.insert (currentAttrName parserWithValue) (currentAttrValue parserWithValue) (attributes parser) }
  where
    parserWithName  = takeAttrName parser
    parserWithValue = takeAttrValue parserWithName




takeAttrName :: TagParser -> TagParser
takeAttrName parser = parser { currentAttrName = fixWhiteSpaces name,
                               htmlRemainder = if T.null restOfText -- There was no "=" sign in input text. (snd nameAndRemainder) may be empty text, so don't use T.tail on it.
                                               then restOfText
                                               else T.tail restOfText }
  where
    name = T.strip . fst $ pair
    restOfText = snd pair
    pair = T.span (\c -> not (elem c ['=', '/', '>'])) (htmlRemainder parser)




takeAttrValue :: TagParser -> TagParser
takeAttrValue parser = parser { currentAttrValue = T.strip . fixWhiteSpaces $ value
                               -- dropWhile is used to remove value delimiter
                               -- from what comes after the value (from the
                               -- remainer).
                              , htmlRemainder = T.strip (T.dropWhile (== delimiter) restOfText)
                              }
  where
    valueBegin = T.stripStart . htmlRemainder $ parser
    delimiter = case T.head valueBegin of
                  '\'' -> '\''
                  '"'  -> '"'
                  _    -> ' '

    value = if delimiter == ' '
            then T.takeWhile predicate valueBegin -- valueBegin is already stripped at the front, so we will drop until ending space is found
            else T.takeWhile predicate (T.drop 1 valueBegin)

    restOfText = if delimiter == ' '
                 then T.drop (T.length value) valueBegin
                 else T.drop ((T.length value) + 1) valueBegin

    predicate = (\c -> c /= delimiter && c /= '>') -- '>' indicates end of tag. TODO: Notice that "/>" is not recognized correctly here as end of tag.



fixWhiteSpaces :: T.Text -> T.Text
fixWhiteSpaces text = T.map (\c -> if c == '\t' then ' ' else if c == '\n' then ' ' else c) text




-- Notice that tag name in elementName may be followed by characters that can
-- appear in a HTML: '/', '>', ' ', e.g. "head>", "a ", "br/".
-- The other chars that can appear in the elementName are '\n', '\r', '\t'.
--
-- TODO: make sure before calling the function that the elementName contains
-- only an element name, without any following characters.
htmlTagIndex :: T.Text -> Int -- TODO: replace all calls of this function with htmlTagIndex2
htmlTagIndex elementName =
  case V.findIndex findP htmlTagInfo of
    Just idx -> idx
    Nothing  -> -1
  where
    findP :: T.Text -> Bool
    findP = (\t -> t == elementName')

    nameP :: Char -> Bool
    nameP = (\c -> isAlphaNum c && isAscii c)

    elementName' = T.toLower (T.takeWhile nameP elementName)




htmlTagIndex2 :: T.Text -> Maybe Int -- TODO: the function should return Maybe Int
htmlTagIndex2 elementName = V.findIndex findP htmlTagInfo
  where
    findP :: T.Text -> Bool
    findP = (\t -> t == elementName')

    nameP :: Char -> Bool
    nameP = (\c -> isAlphaNum c && isAscii c)

    elementName' = T.toLower (T.takeWhile nameP elementName)
