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

This file is derived from dillo-3.0.5/src/gif.c
-}


{-
References:
[1] https://www.w3.org/Graphics/GIF/spec-gif89a.txt
-}


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module GIF( hll_parseExtension

            --these are for tests.
          , parseExtension
          , gifDefault
          , gifIncreasedSize
          , gifForward
          , GIF (..)
          ) where

import Prelude
import Foreign.C.String
import Foreign
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.E
import qualified Data.Text.IO as T.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Control.Applicative
import Control.Monad -- when



foreign export ccall "hll_parseExtension" hll_parseExtension :: Ptr HelloGifGC -> CString -> Int -> IO Int

#include "../gif.h"

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



extensionIntroducer           = 0x21 -- First byte of Extension. "23. Graphic Control Extension."
extensionTypeGraphicControl   = 0xf9 -- "23. Graphic Control Extension."
extensionTypeComment          = 0xfe -- "24. Comment Extension."
extensionTypePlainText        = 0x01 -- "25. Plain Text Extension."
extensionTypeApplication      = 0xff -- "26. Application Extension."



data GIF = GIF {
    parsed   :: Bool
  , consumed :: Int -- Count of bytes consumed from byte string.

  -- [1] "24. Comment Extension."
  , comment  :: T.Text

  -- [1] "23. Graphic Control Extension.", Required version: Gif89.
  , transparentColorIndex :: Int
  , delayTime             :: Int
  , userInputFlag         :: Int
  , disposalMethod        :: Int
  } deriving (Show, Eq)



{-
Try to parse all bytes shown in [1] "24. Comment Extension.", from Extension
Introducer to Block Terminator, inclusive.

TODO: we can/should peek behind the end of this block and verify that there
is a Block Terminator right after this sub-block. The GIF spec tells us
clearly that there is only one non-empty sub-block, followed by Block
Terminator.
-}
handleExtensionSubBlockComment :: GIF -> BS.ByteString -> Maybe GIF
handleExtensionSubBlockComment gif buf
  | BS.length buf == 0           = Just gif -- Check length of the buf first, before trying to get from it a subBlockSize.
  | subBlockSize == 0            = Nothing -- By mistake a Block Terminator has been passed to the
                                           -- function. Block Terminator should be handled by caller of
                                           -- this function, not by a function dedicated to parse a
                                           -- comment.
  | subBlockSize > BS.length buf = Just gif -- Not enough input data.
  | otherwise                    = Just gif { comment = commentText, consumed = gifIncreasedSize gif (oldConsumed + subBlockSize + 1) } -- +1 for sub-block size byte
    -- TODO: the new comment must be appended to existing comment for multi-sub-block comments.
  where
    commentBytes = BS.take subBlockSize (BS.drop 1 buf)
    commentText = T.E.decodeUtf8 commentBytes
    subBlockSize = fromIntegral (BS.index buf 0)
    oldConsumed = consumed gif



{-
Try to parse all bytes shown in [1] "23. Graphic Control Extension.",
from Extension Introducer to Block Terminator, inclusive.

TODO: review these conversions from Word8 to integer. Maybe some of these
values in GC should be bytes?
-}
handleExtensionSubBlockGraphicControl :: GIF -> BS.ByteString -> Maybe GIF
handleExtensionSubBlockGraphicControl gif buf =
  -- Default value of transparentColorIndex field, set in Dillo's a_Gif_new(), was -1.
  Just gif {   consumed              = gifIncreasedSize gif (subBlockSize + 1)
             , transparentColorIndex = if tcf then (fromIntegral (BS.index buf 4)) else -1 -- Transparent color index, may not be valid (unless flag is set).
             , delayTime             = pairToUint (BS.index buf 2) (BS.index buf 3)
             , userInputFlag         = fromIntegral ((flags `shiftR` 1) .&. 0x01)
             , disposalMethod        = fromIntegral ((flags `shiftR` 2) .&. 0x07)
           }
  where
    {- TODO: we can/should peek behind the end of this block and verify that there
       is a Block Terminator right after this sub-block. The GIF spec tells us
       clearly that there is only one non-empty sub-block, followed by Block Terminator.
    -}
    subBlockSize = fromIntegral (BS.index buf 0)
    flags = BS.index buf 1
    tcf = flags .&. 0x01 > 0 :: Bool -- "Transparent Color Flag"



-- TODO: return uint?
pairToUint :: Word8 -> Word8 -> Int
pairToUint lsb msb = fromIntegral ((msb `shiftL` 8) .|. lsb)



isBlockTerminator :: BS.ByteString -> Bool
isBlockTerminator buf = 0 == getSubBlockSize buf



getSubBlockSize :: BS.ByteString -> Int
getSubBlockSize buf = fromIntegral (BS.index buf 0)



{-
Comment Extension, Plain Text Extension and Application Extension can contain
multiple sub-blocks followed by Block Terminator. This function traverses
over all these sub-blocks.

For Graphic Control Extension the traversal is shorted because there is only
one sub-block followed by Block Terminator.
-}
parseSubBlocks :: GIF -> BS.ByteString -> (GIF -> BS.ByteString -> Maybe GIF) -> Maybe GIF
parseSubBlocks gif buf subBlockParser
  | BS.length buf == 0           = Just gif -- Not enough space even for block terminator
  | BS.length buf < subBlockSize = Just gif -- Not enough data to parse sub-block in full
  | isBlockTerminator buf        = Just (gifForward gif 1) -- +1 for Terminator that we have just detected.
  | otherwise =
      case subBlockParser gif buf of
        Just gif2 -> parseSubBlocks gif2 (BS.drop (1 + subBlockSize) buf) subBlockParser -- +1 for sub-block size byte.
        Nothing   -> Nothing
  where subBlockSize = getSubBlockSize buf




{-
Parse single extension. The byte string passed to the function should start
with Extension Introducer byte (0x21) and should end with Block Terminator.

On succcess, all bytes of the extension (from introducer to the terminator
inclusive) are consumed.
-}
parseExtension :: GIF -> BS.ByteString -> Maybe GIF
parseExtension gif buf
  | BS.length buf < 3                 = Just (gifNotEnoughData gif) -- 3: Extension Introducer, Extension Label and at least one byte of some data, perhaps Block Terminator
  | introducer /= extensionIntroducer = Nothing
  | otherwise                         =
    case dispatchSubBlocks gif (BS.drop 2 buf) label of -- 2: Drop Extension Introducer and Extension Label
      Just result -> Just (if (consumed result > consumed gif) then (gifForward result 2) else result)
      Nothing -> Nothing
  where
    introducer = BS.index buf 0
    label = BS.index buf 1
    dispatchSubBlocks gif buf label
      | label == extensionTypeGraphicControl = parseSubBlocks gif buf handleExtensionSubBlockGraphicControl
      | label == extensionTypeComment        = parseSubBlocks gif buf handleExtensionSubBlockComment
      | label == extensionTypePlainText      = Nothing -- TODO: add code that consumes bytes in this extension
      | label == extensionTypeApplication    = Nothing -- TODO: add code that consumes bytes in this extension
      | otherwise                            = Nothing -- Invalid extension label



gifForward :: GIF -> Int -> GIF
gifForward gif n = gif { consumed = (consumed gif) + n }



gifIncreasedSize :: GIF -> Int -> Int
gifIncreasedSize gif n = (consumed gif) + n



gifNotEnoughData :: GIF -> GIF
gifNotEnoughData gif = gif { consumed = 0 }


gifDefault = GIF {
    parsed = False
  , consumed = 0

  , comment = ""

  , transparentColorIndex = 0 -- TODO: Default value of transparentColorIndex field, set in Dillo's a_Gif_new(), was -1.
  , delayTime = 0
  , userInputFlag = 0
  , disposalMethod = 0
  }
