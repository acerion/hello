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
    FfiHtmlDocument (..)
  , peekHtmlDocument
  , pokeHtmlDocument
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

import Hello.Html.Document




foreign export ccall "hll_getDoctype4" hll_getDoctype4 :: Ptr FfiHtmlDocument -> CString -> IO ()
foreign export ccall "hll_getDoctypeFromBuffer" hll_getDoctypeFromBuffer :: Ptr FfiHtmlDocument -> CString -> CInt -> IO ()




data FfiHtmlDocument = FfiHtmlDocument {
    docTypeC        :: CInt
  , docTypeVersionC :: Float
  } deriving (Show)




instance Storable FfiHtmlDocument where
  sizeOf    _ = #{size c_html_document_t}
  alignment _ = #{alignment c_html_document_t}

  poke ptr (FfiHtmlDocument argType argTypeVersion) = do
    #{poke c_html_document_t, c_doc_type}         ptr argType
    #{poke c_html_document_t, c_doc_type_version} ptr argTypeVersion

  peek ptr = do
    a <- #{peek c_html_document_t, c_doc_type}         ptr
    b <- #{peek c_html_document_t, c_doc_type_version} ptr
    return (FfiHtmlDocument a b)




peekHtmlDocument :: Ptr FfiHtmlDocument -> IO HtmlDocument
peekHtmlDocument ptrStructHtmlDocument = do

  ffiDoc <- peek ptrStructHtmlDocument

  let t :: HtmlDocumentType = intToDocumentType $ fromIntegral $ docTypeC ffiDoc
  let v = docTypeVersionC ffiDoc

  let doc = HtmlDocument { docType = t, docTypeVersion = v }
  return doc




-- Set fields in pointer to struct passed from C code.
pokeHtmlDocument :: Ptr FfiHtmlDocument-> HtmlDocument -> IO ()
pokeHtmlDocument ptrStructHtmlDocument doc = do
  let t = documentTypeToInt $ docType doc
  let v :: Float = docTypeVersion doc
  poke ptrStructHtmlDocument $ FfiHtmlDocument t v




documentTypeToInt HtmlDocumentTypeNone         = 0
documentTypeToInt HtmlDocumentTypeUnrecognized = 1
documentTypeToInt HtmlDocumentTypeHtml         = 2
documentTypeToInt HtmlDocumentTypeXhtml        = 3



intToDocumentType :: Int -> HtmlDocumentType
intToDocumentType i = case i of
                        0 -> HtmlDocumentTypeNone
                        1 -> HtmlDocumentTypeUnrecognized
                        2 -> HtmlDocumentTypeHtml
                        3 -> HtmlDocumentTypeXhtml





hll_getDoctype4 :: Ptr FfiHtmlDocument -> CString -> IO ()
hll_getDoctype4 ptrHtmlDocument cBuf = do
  buf <- BSU.unsafePackCString cBuf
  let bufT = T.E.decodeUtf8 buf

  htmlDocument <- peekHtmlDocument ptrHtmlDocument

  let newHtmlDocument = getDoctype4 bufT htmlDocument

  pokeHtmlDocument ptrHtmlDocument newHtmlDocument

  putStrLn $ show newHtmlDocument

  return ()




hll_getDoctypeFromBuffer :: Ptr FfiHtmlDocument -> CString -> CInt -> IO ()
hll_getDoctypeFromBuffer ptrHtmlDocument cBuf cBufLen = do
  buf <- BSU.unsafePackCStringLen (cBuf, fromIntegral cBufLen)
  let bufT = T.E.decodeUtf8 buf

  htmlDocument <- peekHtmlDocument ptrHtmlDocument

  let newHtmlDocument = getDoctypeFromBuffer bufT htmlDocument

  pokeHtmlDocument ptrHtmlDocument newHtmlDocument

  putStrLn $ show newHtmlDocument

  return ()

