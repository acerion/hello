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




module HtmlTagFFI(hll_getAttrValue
                 ) where




import Prelude
import Foreign
import Foreign.C
import Foreign.C.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.E
import qualified Data.ByteString.Unsafe as BSU
import HtmlTag




foreign export ccall "hll_getAttrValue" hll_getAttrValue :: CString -> Int -> CString -> IO CString



-- Return NULL if cAttrName was not found in cTag string.
--
-- Otherwise return C string with value of the attribute. The returned string
-- may be empty.
--
-- TODO: This function is probably a good example of how not to return a C
-- string from a function (because I don't free a buffer returned by
-- newCString), but I don't care that much because sooner or later most of
-- HTML code (if not all) will be rewritten in Haskell, and this particular
-- FFI function will be gone.
hll_getAttrValue :: CString -> Int -> CString -> IO CString
hll_getAttrValue cTag ctagSize cAttrName = do
  tag <- BSU.unsafePackCStringLen (cTag, ctagSize)
  attrName <- BSU.unsafePackCString (cAttrName)
  case htmlTagGetAttributeValue (T.E.decodeUtf8 tag) (T.E.decodeUtf8 attrName) of
    Just attrValue -> newCString (T.unpack attrValue) -- This string may be empty if attr value is empty.
    Nothing        -> return nullPtr                  -- Null pointer indicates "attribute name not found".
