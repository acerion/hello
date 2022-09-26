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
{-# LANGUAGE ScopedTypeVariables #-}




module Hello.Ffi.Html.Attribute
  (
    hll_htmlParseAttributeWidthOrHeight
  )
where




import Prelude
import Foreign
import Foreign.C
import qualified Data.Text.Encoding as T.E
import qualified Data.ByteString.Unsafe as BSU

import Hello.Css.Distance

import Hello.Html.Attribute

import Hello.Ffi.Css.Distance
import Hello.Ffi.Html.Document




foreign export ccall "hll_htmlParseAttributeWidthOrHeight" hll_htmlParseAttributeWidthOrHeight :: CString -> Ptr FfiCssLength -> IO ()
foreign export ccall "hll_htmlValidateNameOrIdValue" hll_htmlValidateNameOrIdValue :: Ptr FfiHtmlDoctype -> CString -> CString -> IO Bool




hll_htmlParseAttributeWidthOrHeight :: CString -> Ptr FfiCssLength -> IO ()
hll_htmlParseAttributeWidthOrHeight cAttrValue ptrStructCssLength = do
  attrValue <- BSU.unsafePackCString cAttrValue

  let (v', t') = case parseLengthOrMultiLength (T.E.decodeUtf8 attrValue) of
                   Just (l, t) -> (l, t)
                   Nothing     -> (0.0, cssLengthTypeAuto)

  poke ptrStructCssLength $ FfiCssLength v' (fromIntegral t')

  return ()





hll_htmlValidateNameOrIdValue :: Ptr FfiHtmlDoctype -> CString -> CString -> IO Bool
hll_htmlValidateNameOrIdValue ptrHtmlDoctype ptrAttrName ptrAttrValue = do

  htmlDocument <- peekHtmlDoctype ptrHtmlDoctype

  attrName <- BSU.unsafePackCString ptrAttrName
  attrValue <- BSU.unsafePackCString ptrAttrValue

  let attrNameT = T.E.decodeUtf8 attrName
  let attrValueT = T.E.decodeUtf8 attrValue

  return $ validateNameOrIdValue htmlDocument attrNameT attrValueT
