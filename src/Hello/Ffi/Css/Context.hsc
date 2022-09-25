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
-}




{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}




module Hello.Ffi.Css.Context
  (
  )
where




import Prelude
import Foreign
import Foreign.C.String
import Foreign.C.Types
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.E

import System.IO


--import Debug.Trace

import Hello.Html.Doctree
--import Hello.Html.DoctreeNode
--import Hello.Utils

import Hello.Css.Cascade
import Hello.Css.ContextGlobal
import Hello.Css.DeclarationSetsGlobal
import Hello.Css.MatchCache
import Hello.Css.Parser
import Hello.Css.StyleNode
import Hello.Css.StyleSheet
import Hello.Css.UserAgentStyle

import Hello.Ffi.Css.Doctree
--import Hello.Ffi.Css.MatchCache
import Hello.Ffi.Css.Parser
--import Hello.Ffi.Css.StyleSheet
--import Hello.Ffi.Utils



#include "../../hello.h"




foreign export ccall "hll_cssContextCtor" hll_cssContextCtor :: IO CInt
foreign export ccall "hll_cssContextApplyCssContext" hll_cssContextApplyCssContext :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO CInt
foreign export ccall "hll_parseCss" hll_parseCss :: Ptr FfiCssParser -> Ptr FfiCssToken -> CInt -> IO ()

foreign export ccall "hll_cssContextPrint" hll_cssContextPrint :: CString -> CInt -> IO ()



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
hll_cssContextCtor :: IO CInt
hll_cssContextCtor = do
  ref     <- fmap fromIntegral globalContextCtor
  context <- globalContextGet ref

  -- Since this new context will contain a sheet with 'primary user agent'
  -- style sheet, the context must have its match cache increased to be large
  -- enough for matching rules form the 'primary user agent' style sheet.
  --
  -- TODO: why do we need to put -1?
  let requiredMatchCacheSize = (matchCacheSize . matchCache $ userAgentContext) - 1

  -- Put the (most probably non-empty) User Agent Sheet in the newly created
  -- context (it has been created with globalContextCtor on top of this
  -- function).
  let context' = (setSheet CssPrimaryUserAgent userAgentStyleSheet) . (increaseMatchCacheSize requiredMatchCacheSize) $ context
  globalContextUpdate ref context'

  -- This is a constructor, so return a reference to newly created context.
  return . fromIntegral $ ref




{-
hll_cssContextUpdate :: CInt -> Ptr FfiCssContext -> IO ()
hll_cssContextUpdate cRef ptrStructCssContext = do
  let ref  = fromIntegral cRef
  context <- peekCssContext ptrStructCssContext

  globalContextUpdate ref context




hll_cssContextPut :: Ptr FfiCssContext -> IO CInt
hll_cssContextPut ptrStructCssContext = do
  context <- peekCssContext ptrStructCssContext
  ref <- globalContextPut context

  return . fromIntegral $ ref
-}




getSomeDeclSet2 ref = if (-1) == ref
                      then return defaultCssDeclarationSet
                      else globalDeclarationSetGet ref



hll_cssContextApplyCssContext :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO CInt
hll_cssContextApplyCssContext cRef cDoctreeRef cDtnNum cMainDeclSetRef cImportantDeclSetRef cNonCssDeclSetRef = do

  let ref  = fromIntegral cRef
  context <- globalContextGet ref

  doctree <- globalDoctreeGet . fromIntegral $ cDoctreeRef
  let dtn  = getDtnUnsafe doctree (fromIntegral cDtnNum)

  let mainDeclSetRef = fromIntegral cMainDeclSetRef
  mainDeclSetIn :: CssDeclarationSet <- getSomeDeclSet2 mainDeclSetRef -- TODO: we should not be using getSomeDeclSet2 - it may return a new decl set for uninitialzed decl set reference

  let importantDeclSetRef = fromIntegral cImportantDeclSetRef
  importantDeclSetIn :: CssDeclarationSet <- getSomeDeclSet2 importantDeclSetRef -- TODO: we should not be using getSomeDeclSet2 - it may return a new decl set for uninitialzed decl set reference

  let nonCssDeclSetRef = fromIntegral cNonCssDeclSetRef
  nonCssDeclSetIn :: CssDeclarationSet <- getSomeDeclSet2 nonCssDeclSetRef -- TODO: we should not be using getSomeDeclSet2 - it may return a new decl set for uninitialzed decl set reference

  let styleNode :: StyleNode = StyleNode
                               { mainDeclSet      = mainDeclSetIn
                               , importantDeclSet = importantDeclSetIn
                               , nonCssDeclSet    = nonCssDeclSetIn
                               }

  (mergedDeclSet, matchCache') <- cssContextApplyCssContext context doctree dtn styleNode

  declSetRef <- globalDeclarationSetPut mergedDeclSet

  let context2 = context { matchCache = matchCache' }
  globalContextUpdate ref context2 -- { matchCache = matchCache' }

  return . fromIntegral $ declSetRef




hll_parseCss :: Ptr FfiCssParser -> Ptr FfiCssToken -> CInt -> IO ()
hll_parseCss ptrStructCssParser ptrStructCssToken cRef = do
  parser  <- peekCssParser ptrStructCssParser
  token   <- peekCssToken ptrStructCssToken

  let ref  = fromIntegral cRef
  context <- globalContextGet ref

  let ((p2, t2), c2) = parseCss ((parser, token), context)

  pokeCssParser ptrStructCssParser p2
  pokeCssToken ptrStructCssToken t2

  globalContextUpdate ref c2

  return ()



hll_cssContextPrint :: CString -> CInt -> IO ()
hll_cssContextPrint cPath cRef = do
  bufPathstringVal <- BSU.unsafePackCString $ cPath
  let path :: String  = T.unpack . T.E.decodeLatin1 $ bufPathstringVal

  let ref  = fromIntegral cRef
  context <- globalContextGet ref

  h <- openFile path WriteMode

  hPrint h context
  hClose h

  return ()

