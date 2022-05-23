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

This file is derived from dillo-3.0.5/src/styleengine.cc.
Copyright assignments from that file:
Copyright 2008-2009 Johannes Hofmann <Johannes.Hofmann@gmx.de>
-}




{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}



module Hello.Css.UserAgentStyle
  (
    styleEngineBuildUserAgentStyle
  )
  where



import Data.Text as T

import Hello.Css.StyleSheet
import Hello.Css.Tokenizer




userAgentStyleString = T.unlines
  [
    "body  {margin: 5px}"
  , "big {font-size: 1.17em}"
  , "blockquote, dd {margin-left: 40px; margin-right: 40px}"
  , "center {text-align: center}"
  , "dt {font-weight: bolder}"
  , ":link {color: blue; text-decoration: underline; cursor: crosshair}"
  , ":visited {color: orange; text-decoration: underline; cursor: pointer}"
  --,  ":visited {color: #800080; text-decoration: underline; cursor: pointer}"
  , "h1, h2, h3, h4, h5, h6, b, strong {font-weight: bolder}"
  , "address, article, aside, center, div, figure, figcaption, footer,"
  , " h1, h2, h3, h4, h5, h6, header, nav, ol, p, pre, section, ul"
  , " {display: block}"
  , "i, em, cite, address, var {font-style: italic}"
  , ":link img, :visited img {border: 1px solid}"
  , "frameset, ul, ol, dir {margin-left: 40px}"

  -- WORKAROUND: It should be margin: 1em 0 but as we don't collapse these
  -- margins yet, it look better like this.
  , "p {margin: 0.5em 0}"

  , "figure {margin: 1em 40px}"
  , "h1 {font-size: 2em; margin-top: .67em; margin-bottom: 0}"
  , "h2 {font-size: 1.5em; margin-top: .75em; margin-bottom: 0}"
  , "h3 {font-size: 1.17em; margin-top: .83em; margin-bottom: 0}"
  , "h4 {margin-top: 1.12em; margin-bottom: 0}"
  , "h5 {font-size: 0.83em; margin-top: 1.5em; margin-bottom: 0}"
  , "h6 {font-size: 0.75em; margin-top: 1.67em; margin-bottom: 0}"
  , "hr {width: 100%; border: 1px inset}"
  , "li {margin-top: 0.1em; display: list-item}"
  , "pre {white-space: pre}"
  , "ol {list-style-type: decimal}"
  , "ul {list-style-type: disc}"
  , "ul ul {list-style-type: circle}"
  , "ul ul ul {list-style-type: square}"
  , "ul ul ul ul {list-style-type: disc}"
  , "ins, u {text-decoration: underline}"
  , "small, sub, sup {font-size: 0.83em}"
  , "sub {vertical-align: sub}"
  , "sup {vertical-align: super}"
  , "s, strike, del {text-decoration: line-through}"

  -- HTML5 spec notes that mark styling "is just a suggestion and can be
  -- changed based on implementation feedback"
  , "mark {background: yellow; color: black;}"

  , "table {border-spacing: 2px}"
  , "td, th {padding: 2px}"
  , "thead, tbody, tfoot {vertical-align: middle}"
  , "th {font-weight: bolder; text-align: center}"
  , "code, tt, pre, samp, kbd {font-family: monospace}"

  -- WORKAROUND: Reset font properties in tables as some pages rely on it
  -- (e.g. gmail).
  -- http://developer.mozilla.org/En/Fixing_Table_Inheritance_in_Quirks_Mode
  -- has a detailed description of the issue.
  , "table, caption {font-size: medium; font-weight: normal}"
  ]




-- Create the user agent style in given css context
--
-- The user agent style defines how a browser renders HTML in the absence of
-- author or user styles.
--
-- TODO: in C++ this function was a class static method that was called only
-- once in program's life time, at the beginning of program's run. An user
-- agent style is common for all style sheets, so there is no point in
-- parsing it from scratch for all visited pages. As an optimisation this
-- style was parsed once (in the static method) and inserted into each page's
-- style sheet. TO DO: recreate this in Haskell.
styleEngineBuildUserAgentStyle :: CssContext -> CssContext
styleEngineBuildUserAgentStyle context = context'
  where
    ((_, _), context') = parseCss ((parser, CssTokNone), context)
    parser = defaultParser { remainder = userAgentStyleString
                           , cssOrigin = CssOriginUserAgent
                           }


