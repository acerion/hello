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

This file is derived from dillo-3.0.5/src/html.cc.
Copyright assignments from that file:
Copyright (C) 2005-2007 Jorge Arellano Cid <jcid@dillo.org>
-}




{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}




module Hello.Ffi.Html.Tag
  (
    ffiHtmlAttributeGetValue
  )
where




import Prelude
import Foreign
import Foreign.C
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.E
import qualified Data.ByteString.Unsafe as BSU

import Hello.Html.Tag




foreign export ccall "ffiHtmlAttributeGetValue" ffiHtmlAttributeGetValue :: CString -> Int -> CString -> IO CString
foreign export ccall "ffiHtmlTagIndex" ffiHtmlTagIndex :: CString -> IO Int




-- Return NULL if cAttrName was not found in cDocumentRem string.
--
-- Otherwise return C string with value of the attribute. The returned string
-- may be empty.
--
-- TODO: This function is probably a good example of how not to return a C
-- string from a function (because I don't free a buffer returned by
-- newCString), but I don't care that much because sooner or later most of
-- HTML code (if not all) will be rewritten in Haskell, and this particular
-- FFI function will be gone.
ffiHtmlAttributeGetValue :: CString -> Int -> CString -> IO CString
ffiHtmlAttributeGetValue cDocumentRem ctagSize cAttrName = do
  documentRem <- BSU.unsafePackCStringLen (cDocumentRem, ctagSize)
  attrName <- BSU.unsafePackCString cAttrName
  case htmlAttributeGetValue (T.E.decodeUtf8 documentRem) (T.E.decodeUtf8 attrName) of
    Just attrValue -> newCString (T.unpack attrValue) -- This string may be empty if attr value is empty.
    Nothing        -> return nullPtr                  -- Null pointer indicates "attribute name not found".




ffiHtmlTagIndex :: CString -> IO Int
ffiHtmlTagIndex cName = do
  name <- BSU.unsafePackCString cName
  return (htmlTagIndex . T.E.decodeUtf8 $ name)
