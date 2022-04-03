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
  , getDoctypePublic
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
  deriving (Show, Eq)




html20       = "-//IETF//DTD HTML"
html32       = "-//W3C//DTD HTML 3.2"
html40       = "-//W3C//DTD HTML 4.0"
html401      = "-//W3C//DTD HTML 4.01"
html401Url   = "http://www.w3.org/TR/html4/"
xhtml1       = "-//W3C//DTD XHTML 1.0"
xhtml1Url    = "http://www.w3.org/TR/xhtml1/DTD/"
xhtml11      = "-//W3C//DTD XHTML 1.1"
xhtml11Url   = "http://www.w3.org/TR/xhtml11/DTD/"




getDoctypePublic :: T.Text -> HtmlDocument -> HtmlDocument
getDoctypePublic buf doc | T.isPrefixOf  html401 buf && urlMatches buf  html401  html401Url = doc { docType = HtmlDocumentTypeHtml,  docTypeVersion = 4.01 }
                         | T.isPrefixOf xhtml1   buf && urlMatches buf xhtml1   xhtml1Url   = doc { docType = HtmlDocumentTypeXhtml, docTypeVersion = 1.0  }
                         | T.isPrefixOf xhtml11  buf && urlMatches buf xhtml11  xhtml11Url  = doc { docType = HtmlDocumentTypeXhtml, docTypeVersion = 1.1  }
                         | T.isPrefixOf  html40  buf = doc { docType = HtmlDocumentTypeHtml,  docTypeVersion = 4.0  }
                         | T.isPrefixOf  html32  buf = doc { docType = HtmlDocumentTypeHtml,  docTypeVersion = 3.2  }
                         | T.isPrefixOf  html20  buf = doc { docType = HtmlDocumentTypeHtml,  docTypeVersion = 2.0  }
                         | otherwise                 = doc
  where
    urlMatches buf tag url = T.isInfixOf urlL remL
      where
        urlL = T.toLower url
        remL = T.toLower $ T.drop (T.length tag) buf


