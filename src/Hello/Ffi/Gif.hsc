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

module Hello.Ffi.Gif( hll_parseExtension
                    )
  where

import Prelude
import Foreign.C.String
import Foreign
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Control.Monad -- when
import Gif



foreign export ccall "hll_parseExtension" hll_parseExtension :: Ptr HelloGifGC -> CString -> Int -> IO Int

#include "../hello.h"

-- [1] "23. Graphic Control Extension.", Required version: Gif89.
--
-- TODO: type of these values must be CInt, not Int. See similar data type in
-- Css/Parser.hsc.
data HelloGifGC = HelloGifGC {
    transparentColorIndexC :: Int
  , delayTimeC             :: Int
  , userInputFlagC         :: Int
  , disposalMethodC        :: Int
  }

instance Storable HelloGifGC where
  sizeOf    _ = #{size c_gif_t}
  alignment _ = alignment (undefined :: Int)

  poke ptr c_gif_t = do
    #{poke c_gif_t, c_transparent_color_index} ptr $ transparentColorIndexC c_gif_t
    #{poke c_gif_t, c_delay_time} ptr              $ delayTimeC c_gif_t
    #{poke c_gif_t, c_user_input_flag} ptr         $ userInputFlagC c_gif_t
    #{poke c_gif_t, c_disposal_method} ptr         $ disposalMethodC c_gif_t

  peek ptr = return HelloGifGC
    `ap` (#{peek c_gif_t, c_transparent_color_index} ptr)
    `ap` (#{peek c_gif_t, c_delay_time}              ptr)
    `ap` (#{peek c_gif_t, c_user_input_flag}         ptr)
    `ap` (#{peek c_gif_t, c_disposal_method}         ptr)




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
      manipulatePtrGC hllGif gif =
        -- Set Graphic Control Extension fields in pointer passed from C code.
        poke hllGif $ HelloGifGC (transparentColorIndex gif) (delayTime gif) (userInputFlag gif) (disposalMethod gif)




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
