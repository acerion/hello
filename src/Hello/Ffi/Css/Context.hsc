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
-}




{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}




module Hello.Ffi.Css.Context
  (
  )
where




import Prelude
import Foreign.C.String
import Foreign.C.Types
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.E

import System.IO

--import Debug.Trace

import Hello.Css.ContextGlobal
import Hello.Css.StyleSheet
import Hello.Css.Tokenizer
import Hello.Css.UserAgentStyle



#include "../../hello.h"




foreign export ccall "ffiCssContextCtor" ffiCssContextCtor :: IO CInt
--foreign export ccall "ffiCssCascadeApplyCssContext" ffiCssCascadeApplyCssContext :: CInt -> CInt -> CInt -> IO CInt
foreign export ccall "ffiParseCss" ffiParseCss :: CInt -> CInt -> CString -> CInt -> CString -> IO ()

foreign export ccall "ffiCssContextPrint" ffiCssContextPrint :: CString -> CInt -> IO ()



{-
data FfiCssContext = FfiCssContext {
    cSheets              :: Ptr (Ptr FfiCssStyleSheet)
  , cStructPtrMatchCache :: Ptr FfiCssMatchCache
  , cRulePosition        :: CInt
  } deriving (Show)




/**
 * \brief A set of c_css_style_sheet_t sheets
 */
typedef struct c_css_context_t {
   c_css_style_sheet_t * c_sheets[CSS_PRIMARY_ORDER_SIZE];
   c_css_match_cache_t * c_match_cache;
   int c_rule_position;
} c_css_context_t;





instance Storable FfiCssContext where
  sizeOf    _ = #{size c_css_context_t}
  alignment _ = #{alignment c_css_context_t}

  peek ptr = do
    let a = (\hsc_ptr -> plusPtr hsc_ptr #{offset c_css_context_t, c_sheets}) ptr
    b <- #{peek c_css_context_t, c_match_cache}   ptr
    c <- #{peek c_css_context_t, c_rule_position} ptr
    return (FfiCssContext a b c)

  poke ptr (FfiCssContext a b c) = do
    #{poke c_css_context_t, c_sheets}        ptr a
    #{poke c_css_context_t, c_match_cache}   ptr b
    #{poke c_css_context_t, c_rule_position} ptr c




peekCssContext :: Ptr FfiCssContext -> IO CssContext
peekCssContext ptrStructContext = do
  ffiContext <- peek ptrStructContext

  s :: [CssStyleSheet] <- peekArrayOfPointers (cSheets ffiContext) 5 peekCssStyleSheet
  cache <- peekPtrCssMatchCache . cStructPtrMatchCache $ ffiContext

  return CssContext{ sheets       = s
                   , matchCache   = cache
                   , rulePosition = fromIntegral . cRulePosition $ ffiContext
                   }




pokeCssContext :: Ptr FfiCssContext -> CssContext -> IO ()
pokeCssContext ptrStructContext context = do
  ffiContext <- peek ptrStructContext

  let array :: Ptr (Ptr FfiCssStyleSheet) = cSheets ffiContext
  pokeArrayOfPreallocedPointers (sheets context) pokeStyleSheet array

  pokeCssMatchCache (cStructPtrMatchCache ffiContext) (matchCache context)

  let pos :: CInt = fromIntegral . rulePosition $ context
  pokeByteOff ptrStructContext #{offset c_css_context_t, c_rule_position} pos
-}



-- Constructor of new context.
ffiCssContextCtor :: IO CInt
ffiCssContextCtor = do
  ref     <- fmap fromIntegral globalContextCtor
  context <- globalContextGet ref

  -- Put the (most probably non-empty) User Agent Sheet in the newly created
  -- context (it has been created with globalContextCtor on top of this
  -- function).
  let context' = (setSheet CssPrimaryUserAgent userAgentStyleSheet) $ context
  globalContextUpdate ref context'

  -- This is a constructor, so return a reference to newly created context.
  return . fromIntegral $ ref




{-
ffiCssContextUpdate :: CInt -> Ptr FfiCssContext -> IO ()
ffiCssContextUpdate cRef ptrStructCssContext = do
  let ref  = fromIntegral cRef
  context <- peekCssContext ptrStructCssContext

  globalContextUpdate ref context




ffiCssContextPut :: Ptr FfiCssContext -> IO CInt
ffiCssContextPut ptrStructCssContext = do
  context <- peekCssContext ptrStructCssContext
  ref <- globalContextPut context

  return . fromIntegral $ ref
-}



{-
getSomeDeclSet2 :: Int -> IO CssDeclarationSet
getSomeDeclSet2 ref = if (-1) == ref
                      then return defaultCssDeclarationSet
                      else globalDeclarationSetGet ref
-}




{-
ffiCssCascadeApplyCssContext :: CInt -> CInt -> CInt -> IO CInt
ffiCssCascadeApplyCssContext cStyleEngineRef cRef cStyleNodeIndex = do

  -- FFI and debugging.
  fHandle <- openFile "/tmp/hello_browser_matching_rules_debug.txt" AppendMode
  context <- globalContextGet . fromIntegral $ cRef
  engine  <- globalStyleEngineGet . fromIntegral $ cStyleEngineRef
  let styleNodeIndex = fromIntegral cStyleNodeIndex

  -- The main part.
  --
  -- Remember that styleNode and dtn aren't necessarily the top/current
  -- elements of style node stack or doctree. This function may be called for
  -- any element of style node stack and doctree during restyling of entire
  -- tree. The restyling is done by C++ code when <body> is opened, see
  -- "html->styleEngine->restyle (html->bw);" in Html_tag_open_body(). Always
  -- use styleNodeIndex as a starting point to get a proper styleNode and
  -- dtn.
  let doctree   = SE.doctree engine
      styleNode = SE.styleNodesStackGet engine styleNodeIndex
      dtn       = getDtnUnsafe doctree (doctreeNodeIdx styleNode)
  mergedDeclSet <- cssCascadeApplyCssContext fHandle context doctree dtn styleNode

  -- FFI and debugging.
  mergedDeclSetRef <- globalDeclarationSetPut mergedDeclSet
  hClose fHandle
  return . fromIntegral $ mergedDeclSetRef
-}




-- Parse contents of given buffer as set of CSS rules. Add result of parsing
-- to given CSS context.
--
-- TODO: make use of cBaseUrl argument. Notice that this argument is NULL
-- when function is called to parse User's stylesheet.
--
-- TODO: synchronize this function with ffiCssParseElementStyleAttribute
ffiParseCss :: CInt -> CInt -> CString -> CInt -> CString -> IO ()
ffiParseCss cContextRef cOrigin ptrStringBuf cBufLen _cBaseUrl = do

  let contextRef = fromIntegral cContextRef
  context <- globalContextGet contextRef

  let origin = getCssOrigin . fromIntegral $ cOrigin

  buf <- BSU.unsafePackCStringLen (ptrStringBuf, fromIntegral cBufLen)

  let parser = defaultParser . T.E.decodeLatin1 $ (seq buf buf) -- TODO: do we decode the string correctly?

  let context' = parseCss (parser, context { cssOrigin = origin })

  globalContextUpdate contextRef context'

  return ()



ffiCssContextPrint :: CString -> CInt -> IO ()
ffiCssContextPrint cPath cRef = do
  bufPathstringVal <- BSU.unsafePackCString cPath
  let path :: String  = T.unpack . T.E.decodeLatin1 $ bufPathstringVal

  let ref  = fromIntegral cRef
  context <- globalContextGet ref

  h <- openFile path WriteMode

  hPrint h context
  hClose h

  return ()




getCssOrigin :: Int -> CssOrigin
getCssOrigin o = case o of
                   0 -> CssOriginUserAgent
                   1 -> CssOriginUser
                   2 -> CssOriginAuthor
                   _ -> CssOriginAuthor



{-
getIntOrigin :: CssOrigin -> Int
getIntOrigin origin = case origin of
                        CssOriginUserAgent -> 0
                        CssOriginUser      -> 1
                        CssOriginAuthor    -> 2
-}

