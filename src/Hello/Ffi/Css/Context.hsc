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
    FfiCssContext (..)
  , peekCssContext
  , pokeCssContext
  )
where




import Prelude
import Foreign
import Foreign.C.String
import Foreign.C.Types
import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.E


import Debug.Trace

import Data.IORef
import System.IO.Unsafe

import Hello.Html.Doctree
import Hello.Html.DoctreeNode
import Hello.Utils

import Hello.Css.ContextGlobal
import Hello.Css.Match
import Hello.Css.Parser
import Hello.Css.StyleSheet

import Hello.Ffi.Css.Doctree
import Hello.Ffi.Css.MatchCache
import Hello.Ffi.Css.Parser
import Hello.Ffi.Css.StyleSheet
import Hello.Ffi.Utils



#include "../../hello.h"




foreign export ccall "hll_cssContextCtor" hll_cssContextCtor :: IO CInt
foreign export ccall "hll_cssContextUpdate" hll_cssContextUpdate :: CInt -> Ptr FfiCssContext -> IO ()
foreign export ccall "hll_cssContextPut" hll_cssContextPut :: Ptr FfiCssContext -> IO CInt
foreign export ccall "hll_cssContextApplyCssContext" hll_cssContextApplyCssContext :: Ptr FfiCssContext -> Ptr FfiCssDeclarationSet -> CInt -> CInt -> Ptr FfiCssDeclarationSet -> Ptr FfiCssDeclarationSet -> Ptr FfiCssDeclarationSet -> IO ()
foreign export ccall "hll_parseCss" hll_parseCss :: Ptr FfiCssParser -> Ptr FfiCssToken -> Ptr FfiCssContext -> IO ()




data FfiCssContext = FfiCssContext {
    cSheets              :: Ptr (Ptr FfiCssStyleSheet)
  , cStructPtrMatchCache :: Ptr FfiCssMatchCache
  , cRulePosition        :: CInt
  } deriving (Show)




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




hll_cssContextCtor :: IO CInt
hll_cssContextCtor = fmap fromIntegral globalContextCtor




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




getSomeDeclSet ptr = if nullPtr == ptr
                     then return defaultCssDeclarationSet
                     else peekCssDeclarationSet ptr




hll_cssContextApplyCssContext :: Ptr FfiCssContext -> Ptr FfiCssDeclarationSet -> CInt -> CInt -> Ptr FfiCssDeclarationSet -> Ptr FfiCssDeclarationSet -> Ptr FfiCssDeclarationSet -> IO ()
hll_cssContextApplyCssContext ptrStructCssContext ptrStructTargetDeclSet cDoctreeRef cDtnNum ptrStructMainDeclSet ptrStructImportantDeclSet ptrStructNonCssDeclSet = do

  context <- peekCssContext ptrStructCssContext
  doctree <- getDoctreeFromRef . fromIntegral $ cDoctreeRef
  let dtn  = getDtnUnsafe doctree (fromIntegral cDtnNum)

  mainDeclSet      <- getSomeDeclSet ptrStructMainDeclSet
  importantDeclSet <- getSomeDeclSet ptrStructImportantDeclSet
  nonCssDeclSet    <- getSomeDeclSet ptrStructNonCssDeclSet

  targetDeclSet <- peekCssDeclarationSet ptrStructTargetDeclSet

  (targetDeclSet', matchCache') <- cssContextApplyCssContext context doctree dtn mainDeclSet importantDeclSet nonCssDeclSet

  pokeCssDeclarationSet ptrStructTargetDeclSet targetDeclSet'

  let context2 = context { matchCache = matchCache' }
  pokeCssContext ptrStructCssContext context2

  return ()




hll_parseCss :: Ptr FfiCssParser -> Ptr FfiCssToken -> Ptr FfiCssContext -> IO ()
hll_parseCss ptrStructCssParser ptrStructCssToken ptrStructCssContext = do
  parser  <- peekCssParser ptrStructCssParser
  token   <- peekCssToken ptrStructCssToken
  context <- peekCssContext ptrStructCssContext

  let ((p2, t2), c2) = parseCss ((parser, token), context)

  pokeCssParser ptrStructCssParser p2
  pokeCssToken ptrStructCssToken t2
  pokeCssContext ptrStructCssContext c2

  return ()

