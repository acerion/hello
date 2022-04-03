{-
Copyright (C) 2022 Kamil Ignacak acerion@wp.pl

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

module Hello.Html.Document
  (
    HtmlDocument (..)
  , HtmlDocumentType (..)
  )
where




import Prelude
import qualified Data.Char as D.C
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.E
import qualified Data.Text.Read as T.R
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Map as M

import Hello.Css.Parser




data HtmlDocumentType =
    HtmlDocumentTypeNone
  | HtmlDocumentTypeUnrecognized
  | HtmlDocumentTypeHtml
  | HtmlDocumentTypeXhtml
  deriving (Show, Eq)




data HtmlDocument = HtmlDocument
  { docType        :: HtmlDocumentType
  , docTypeVersion :: Float
  }

