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




module Hello.Ffi.Html.Document
  (
    FfiHtmlDoctype (..)
  , peekHtmlDoctype
  , pokeHtmlDoctype
  )
where




#include "../../hello.h"




import Prelude
import Foreign
import Foreign.C
import Foreign.C.String

import qualified Data.Text as T
import qualified Data.Text.Encoding as T.E
import qualified Data.ByteString.Unsafe as BSU

import Hello.Html.Doctype




foreign export ccall "hll_getDoctype4" hll_getDoctype4 :: Ptr FfiHtmlDoctype -> CString -> IO ()
foreign export ccall "hll_getDoctypeFromBuffer" hll_getDoctypeFromBuffer :: Ptr FfiHtmlDoctype -> CString -> CInt -> IO ()




data FfiHtmlDoctype = FfiHtmlDoctype {
    docTypeC        :: CInt
  , docTypeVersionC :: Float
  } deriving (Show)




instance Storable FfiHtmlDoctype where
  sizeOf    _ = #{size c_html_doctype_t}
  alignment _ = #{alignment c_html_doctype_t}

  poke ptr (FfiHtmlDoctype argType argTypeVersion) = do
    #{poke c_html_doctype_t, c_doc_type}         ptr argType
    #{poke c_html_doctype_t, c_doc_type_version} ptr argTypeVersion

  peek ptr = do
    a <- #{peek c_html_doctype_t, c_doc_type}         ptr
    b <- #{peek c_html_doctype_t, c_doc_type_version} ptr
    return (FfiHtmlDoctype a b)




peekHtmlDoctype :: Ptr FfiHtmlDoctype -> IO HtmlDoctype
peekHtmlDoctype ptrStructHtmlDoctype = do

  ffiDoctype <- peek ptrStructHtmlDoctype

  let v :: Float = docTypeVersionC ffiDoctype

  return $ intToDoctype (fromIntegral $ docTypeC ffiDoctype) v




-- Set fields in pointer to struct passed from C code.
pokeHtmlDoctype :: Ptr FfiHtmlDoctype-> HtmlDoctype -> IO ()
pokeHtmlDoctype ptrStructHtmlDoctype doctype = do
  let (t, v) = doctypeToPair $ doctype
  poke ptrStructHtmlDoctype $ FfiHtmlDoctype t v




doctypeToPair doctype = case doctype of
                          HtmlDoctypeNone         -> (0, 0)
                          HtmlDoctypeUnrecognized -> (1, 0)
                          HtmlDoctypeHtml v       -> (2, v)
                          HtmlDoctypeXhtml v      -> (3, v)



intToDoctype :: Int -> Float -> HtmlDoctype
intToDoctype i v = case i of
                     0 -> HtmlDoctypeNone
                     1 -> HtmlDoctypeUnrecognized
                     2 -> HtmlDoctypeHtml v
                     3 -> HtmlDoctypeXhtml v





hll_getDoctype4 :: Ptr FfiHtmlDoctype -> CString -> IO ()
hll_getDoctype4 ptrHtmlDoctype cBuf = do
  buf <- BSU.unsafePackCString cBuf
  let bufT = T.E.decodeUtf8 buf

  htmlDocument <- peekHtmlDoctype ptrHtmlDoctype

  let newHtmlDoctype = getDoctype4 bufT htmlDocument

  pokeHtmlDoctype ptrHtmlDoctype newHtmlDoctype

  -- putStrLn $ show newHtmlDoctype

  return ()




hll_getDoctypeFromBuffer :: Ptr FfiHtmlDoctype -> CString -> CInt -> IO ()
hll_getDoctypeFromBuffer ptrHtmlDoctype cBuf cBufLen = do
  buf <- BSU.unsafePackCStringLen (cBuf, fromIntegral cBufLen)
  let bufT = T.E.decodeUtf8 buf

  htmlDocument <- peekHtmlDoctype ptrHtmlDoctype

  let newHtmlDoctype = getDoctypeFromBuffer bufT htmlDocument

  pokeHtmlDoctype ptrHtmlDoctype newHtmlDoctype

  -- putStrLn $ show newHtmlDoctype

  return ()

