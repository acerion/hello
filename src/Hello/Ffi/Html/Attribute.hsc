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
{-# LANGUAGE ForeignFunctionInterface #-}




module Hello.Ffi.Html.Attribute
  (
    hll_htmlParseAttributeWidthOrHeight
  )
where




import Prelude
import Foreign
import Foreign.C
import Foreign.C.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.E
import qualified Data.ByteString.Unsafe as BSU
import Hello.Html.Attribute
import Hello.Ffi.Css.Parser
import Hello.Css.Parser




foreign export ccall "hll_htmlParseAttributeWidthOrHeight" hll_htmlParseAttributeWidthOrHeight :: CString -> IO Word32




hll_htmlParseAttributeWidthOrHeight :: CString -> IO Word32
hll_htmlParseAttributeWidthOrHeight cAttrValue = do
  attrValue <- BSU.unsafePackCString cAttrValue
  case parseLengthOrMultiLength (T.E.decodeUtf8 attrValue) of
    Just (l, t) -> hll_cssCreateLength l t
    Nothing     -> hll_cssCreateLength 0.0 cssLengthTypeAuto



