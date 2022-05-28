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


module Hello.Ffi.Css.MatchCache
  (
    FfiCssMatchCache (..)
  , peekPtrCssMatchCache
  , pokeCssMatchCache
  )
where




import Prelude
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Data.List
import Data.Bits
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Vector as V
import qualified Data.List as L
import Control.Monad -- when
import Debug.Trace

import Hello.Css.Match
import Hello.Css.MatchCache
import Hello.Css.MediaQuery
import Hello.Css.Parser
import Hello.Css.StyleSheet
import Hello.Css.Selector
import Hello.Css.Tokenizer
import Hello.Css.UserAgentStyle
import Hello.Html.Doctree
import Hello.Html.DoctreeNode

import Hello.Utils

--import Hello.Ffi.Css.Context
import Hello.Ffi.Css.DoctreeNode
import Hello.Ffi.Css.Doctree
import Hello.Ffi.Css.Parser
--import Hello.Ffi.Css.StyleSheet
import Hello.Ffi.Utils




#include "../../hello.h"



foreign export ccall "hll_matchCacheSetSize" hll_matchCacheSetSize :: Ptr FfiCssMatchCache -> CInt -> IO ()



data FfiCssMatchCache = FfiCssMatchCache {
    cCacheItems     :: Ptr CInt
  , cCacheItemsSize :: CInt
  } deriving (Show)




instance Storable FfiCssMatchCache where
  sizeOf    _ = #{size c_css_match_cache_t}
  alignment _ = #{alignment c_css_match_cache_t}

  peek ptr = do
    let a = (\hsc_ptr -> plusPtr hsc_ptr #{offset c_css_match_cache_t, c_cache_items}) ptr
    b <- #{peek c_css_match_cache_t, c_cache_items_size} ptr
    return (FfiCssMatchCache a b)

  poke ptr (FfiCssMatchCache a b) = do
    -- #{poke c_css_match_cache_t, c_cache_items}      ptr a
    #{poke c_css_match_cache_t, c_cache_items_size} ptr b




peekPtrCssMatchCache :: Ptr FfiCssMatchCache -> IO CssMatchCache
peekPtrCssMatchCache ptrStructMatchCache = do
  ffiMatchCache <- peek ptrStructMatchCache

  let array :: Ptr CInt = cCacheItems ffiMatchCache
  let size  :: Int      = fromIntegral . cCacheItemsSize $ ffiMatchCache

  cCache :: [CInt] <- peekArray size array
  let cache = map fromIntegral cCache

  return . matchCacheFromList $ cache




pokeCssMatchCache :: Ptr FfiCssMatchCache -> CssMatchCache -> IO ()
pokeCssMatchCache ptrStructMatchCache cache = do
  ffiMatchCache <- peek ptrStructMatchCache

  let array :: Ptr CInt = cCacheItems ffiMatchCache
  let cCache :: [CInt] = fmap fromIntegral (matchCacheToList cache)
  let cLen :: CInt = fromIntegral . length $ cCache
  pokeArray array cCache

  poke ptrStructMatchCache $ FfiCssMatchCache array cLen
  --pokeByteOff ptrStructMatchCache #{offset c_css_match_cache_t, c_cache_items_size} cLen




-- TODO: This function seems to just allocate a new vector of (-1) elements.
-- So far I haven't seen this function update a vector of some non-(-1)
-- elements by adding (-1)s at the end.
hll_matchCacheSetSize :: Ptr FfiCssMatchCache -> CInt -> IO ()
hll_matchCacheSetSize ptrStructMatchCache cNewSize = do
  oldMatchCache <- peekPtrCssMatchCache ptrStructMatchCache
  let newSize = fromIntegral cNewSize
  let newMatchCache = matchCacheIncreaseTo oldMatchCache newSize
  pokeCssMatchCache ptrStructMatchCache newMatchCache

  return ()




