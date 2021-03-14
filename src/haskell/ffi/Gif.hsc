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

This file is derived from dillo-3.0.5/src/gif.c.
Copyright assignments from that file:
Copyright (C) 1997 Raph Levien <raph@acm.org>
Copyright (C) 2000-2007 Jorge Arellano Cid <jcid@dillo.org>
-}


{-
References:
[1] https://www.w3.org/Graphics/GIF/spec-gif89a.txt
-}


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module GifFFI( hll_parseExtension
             ) where

import Prelude
import Foreign.C.String
import Foreign
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.E
import qualified Data.Text.Encoding.Error as T.E.E
import qualified Data.Text.IO as T.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Control.Applicative
import Control.Monad -- when
import Gif



foreign export ccall "hll_parseExtension" hll_parseExtension :: Ptr HelloGifGC -> CString -> Int -> IO Int

#include "../../gif.h"

-- [1] "23. Graphic Control Extension.", Required version: Gif89.
data HelloGifGC = HelloGifGC {
    transparentColorIndexC :: Int
  , delayTimeC             :: Int
  , userInputFlagC         :: Int
  , disposalMethodC        :: Int
  }

instance Storable HelloGifGC where
  sizeOf    _ = #{size hll_Gif}
  alignment _ = alignment (undefined :: Int)

  poke ptr hll_Gif = do
    #{poke hll_Gif, transparentColorIndexC} ptr $ transparentColorIndexC hll_Gif
    #{poke hll_Gif, delayTimeC} ptr             $ delayTimeC hll_Gif
    #{poke hll_Gif, userInputFlagC} ptr         $ userInputFlagC hll_Gif
    #{poke hll_Gif, disposalMethodC} ptr        $ disposalMethodC hll_Gif

  peek ptr = return HelloGifGC
    `ap` (#{peek hll_Gif, transparentColorIndexC} ptr)
    `ap` (#{peek hll_Gif, delayTimeC} ptr)
    `ap` (#{peek hll_Gif, userInputFlagC} ptr)
    `ap` (#{peek hll_Gif, disposalMethodC} ptr)




hll_parseExtension :: Ptr HelloGifGC -> CString -> Int -> IO Int
hll_parseExtension hll_gif cBuf size = do
  buf <- BSU.unsafePackCStringLen (cBuf, size)
  case parseExtension gifDefault buf of
    Just gif -> do
      -- A hack needed because pointer to C struct is not member of GIF
      -- struct and extension handlers update only fields of GIF, but not
      -- fields of HelloGifGC.
      when (BS.index buf 1 == extensionTypeGraphicControl) $ manipulatePtrGC hll_gif gif
      return (consumed gif)
    Nothing  -> return (-1)
    where
      manipulatePtrGC hll_gif gif =
        -- Set Graphic Control Extension fields in pointer passed from C code.
        poke hll_gif $ HelloGifGC (transparentColorIndex gif) (delayTime gif) (userInputFlag gif) (disposalMethod gif)




-- TODO: make use of this function
hll_dataSubBlockGetAvailableBytes :: CString -> Int -> IO Int
hll_dataSubBlockGetAvailableBytes cBuf size = do
  buf <- BSU.unsafePackCStringLen (cBuf, size)
  let subBlockSize = fromIntegral (BS.index buf 0)
  if 0 == size
    then return (-1) -- There is not enough data for any kind of parsing.
    else
    if 0x00 == subBlockSize
    then return 0 -- [1] "16. Block Terminator."
    else
      if subBlockSize < size
      then return subBlockSize
      else return (-1)
