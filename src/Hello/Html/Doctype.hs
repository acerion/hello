{-
Copyright (C) 2022-2023 Kamil Ignacak acerion@wp.pl

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

module Hello.Html.Doctype
  (
    HtmlDoctype (..)
  , getDoctypeFromBuffer

  -- Exported only for tests
  , getDoctype4
  , sanitizeDoctypeString
  )
where




import Prelude
import qualified Data.Char as D.C
import qualified Data.Text as T
--import Debug.Trace

-- import Hello.Css.Parser




data HtmlDoctype =
    HtmlDoctypeNone
  | HtmlDoctypeUnrecognized
  | HtmlDoctypeHtml Float
  | HtmlDoctypeXhtml Float
  deriving (Show, Eq)




-- TODO: check how much we can really coerce to lower cases
html20       = T.toLower "-//IETF//DTD HTML"
html32       = T.toLower "-//W3C//DTD HTML 3.2"
html40       = T.toLower "-//W3C//DTD HTML 4.0"
html401      = T.toLower "-//W3C//DTD HTML 4.01"
html401Url   = T.toLower "http://www.w3.org/TR/html4/"
xhtml1       = T.toLower "-//W3C//DTD XHTML 1.0"
xhtml1Url    = T.toLower "http://www.w3.org/TR/xhtml1/DTD/"
xhtml11      = T.toLower "-//W3C//DTD XHTML 1.1"
xhtml11Url   = T.toLower "http://www.w3.org/TR/xhtml11/DTD/"




-- Get doctype of document up to and including 4.x
getDoctype4 :: T.Text -> HtmlDoctype -> HtmlDoctype
getDoctype4 bufL doctype | T.isPrefixOf  html401 bufL && urlMatches bufL  html401  html401Url = HtmlDoctypeHtml  4.01
                         | T.isPrefixOf xhtml1   bufL && urlMatches bufL xhtml1   xhtml1Url   = HtmlDoctypeXhtml 1.0
                         | T.isPrefixOf xhtml11  bufL && urlMatches bufL xhtml11  xhtml11Url  = HtmlDoctypeXhtml 1.1
                         | T.isPrefixOf  html40  bufL = HtmlDoctypeHtml 4.0
                         | T.isPrefixOf  html32  bufL = HtmlDoctypeHtml 3.2
                         | T.isPrefixOf  html20  bufL = HtmlDoctypeHtml 2.0
                         | otherwise                  = doctype
  where
    urlMatches buf tag url = T.isInfixOf urlL remL
      where
        urlL = T.toLower url
        remL = T.toLower $ T.drop (T.length tag) buf



getDoctype5 buffer doctype | any (\x -> x == buffer) html5Doctypes = HtmlDoctypeHtml 5.0
                           | otherwise                             = doctype
  where
    -- TODO: check how much we can really coerce to lower cases
    html5Doctypes = [ T.toLower "<!DOCTYPE html>"
                    , T.toLower "<!DOCTYPE html >"
                    , T.toLower "<!DOCTYPE html SYSTEM \"about:legacy-compat\">"
                    , T.toLower "<!DOCTYPE html SYSTEM 'about:legacy-compat'>"
                    ]





data SanState = SanState
  {
     -- Char that opens quoted string. Space is an initial value indicating
     -- that sanitizer is not inside of quoted string
    quote     :: Char
  , accText   :: T.Text -- Accumulator of result text
  } deriving (Show)




sanitizeDoctypeString :: T.Text -> T.Text
sanitizeDoctypeString string = T.reverse $ accText state
  where
    (state, _) = sanitizeDoctypeString' string




{-
Tag sanitization: Collapse whitespace between tokens
and replace '\n' and '\r' with ' ' inside quoted strings.

TODO: I'm not 100% sure how the different types of spaces should be
collapsed. Check it one day.
-}
sanitizeDoctypeString' = T.mapAccumL f SanState { quote = ' ', accText = "" }
  where
    -- Notice that we first handle special spaces \r and \n, and only then
    -- general space D.isSpace.
    f state c | c == '\n' || c == '\r' = if insideQuote state
                                         then (appendChar state ' ', c)
                                         else (collapseSpaces state  ' ', c)
              | D.C.isSpace c          = if insideQuote state
                                         then (appendChar state c, c)
                                         else (collapseSpaces state c, c)
              | c == '"' || c == '\''  = if c == quote state
                                         then (state { quote = ' ', accText = T.cons c (accText state) }, c)
                                         else (state { quote = c,   accText = T.cons c (accText state) }, c)
              | otherwise              = (state { accText = T.cons c (accText state)}, c)

      where
        insideQuote sanState = ' ' /= quote sanState
        collapseSpaces sanState char = case T.uncons $ accText sanState of
                                         Just (' ', _) -> sanState
                                         _             -> appendChar sanState char
        appendChar sanState char = sanState {accText = T.cons char (accText sanState)}




{-
From Dillo code:

"
Handle DOCTYPE declaration

Follows the convention that HTML 4.01 doctypes which include a full w3c DTD
url are treated as standards-compliant, but 4.01 without the url and HTML 4.0
and earlier are not. XHTML doctypes are always standards-compliant whether or
not an url is present.

Note: I'm not sure about this convention. The W3C validator recognizes the
"HTML Level" with or without the URL. The convention comes from mozilla (see
URLs below), but Dillo doesn't have the same rendering modes, so it may be
better to chose another behaviour. --Jcid

http://www.mozilla.org/docs/web-developer/quirks/doctypes.html
http://lists.auriga.wearlab.de/pipermail/dillo-dev/2004-October/002300.html
http://lists.dillo.org/pipermail/dillo-dev/2004-October/002300.html

This is not a full DOCTYPE parser, just enough for what Dillo uses.
"
-}
getDoctypeFromBuffer :: T.Text -> HtmlDoctype -> HtmlDoctype
getDoctypeFromBuffer buffer htmlDoctype = setFallbackType . get . warnOnMultiple $ (buffer, htmlDoctype)
  where
    warnOnMultiple (buf, doctype) = if doctype /= HtmlDoctypeNone
                                    then (buf, doctype) -- TODO: print warning about the fact that the function was probably called for a second time with this arg ("Multiple DOCTYPE declarations.")
                                    else (buf, doctype)

    get (buf, doc) = getDoctypeFromSanitizedBuffer (sanitizeDoctypeString buf) doc

    setFallbackType doctype = if doctype == HtmlDoctypeNone
                              then HtmlDoctypeUnrecognized -- TODO: print warning about unrecognized document type
                              else doctype




getDoctypeFromSanitizedBuffer :: T.Text -> HtmlDoctype -> HtmlDoctype
getDoctypeFromSanitizedBuffer buffer doctype = if T.isPrefixOf (T.toLower htmlPublicSig) (T.toLower buffer)
                                               then getDoctype4 (T.drop (T.length htmlPublicSig) (T.toLower buffer)) doctype
                                               else getDoctype5 (T.toLower buffer) doctype
  where
    htmlPublicSig = "<!DOCTYPE HTML PUBLIC "




