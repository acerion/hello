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

module Gif( parseExtension
          , gifDefault
          , gifForward
          , Gif (..)
          , extensionTypeGraphicControl
          ) where

import Prelude
import Foreign
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.E
import qualified Data.Text.Encoding.Error as T.E.E
import qualified Data.ByteString as BS




extensionIntroducer           = 0x21 -- First byte of Extension. "23. Graphic Control Extension."
extensionTypeGraphicControl   = 0xf9 -- "23. Graphic Control Extension."
extensionTypeComment          = 0xfe -- "24. Comment Extension."
extensionTypePlainText        = 0x01 -- "25. Plain Text Extension."
extensionTypeApplication      = 0xff -- "26. Application Extension."



data Gif = Gif {

    consumed :: Int -- Count of bytes consumed from byte string.

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
-}
handleExtensionSubBlockComment :: Gif -> BS.ByteString -> Maybe Gif
handleExtensionSubBlockComment gif buf
  | BS.length buf == 0            = Just (gifNotEnoughData gif) -- Check length of the buf first, before trying to get from it a subBlockSize.
  | subBlockSize == 0             = Nothing -- By mistake a Block Terminator has been passed to the
                                            -- function. Block Terminator should be handled by caller of
                                            -- this function, not by a function dedicated to parse a
                                            -- comment.
  | subBlockSize >= BS.length buf = Just (gifNotEnoughData gif) -- Not enough input data.
  | otherwise                     = Just gif { comment  = gifIncreasedComment gif commentText
                                            , consumed = gifIncreasedConsumed gif (1 + subBlockSize) -- 1 for sub-block size byte
                                            }
  where
    commentBytes = BS.take subBlockSize (BS.drop 1 buf)
    -- Use lenientDecode to not throw exception on invalid byte
    -- sequences. GIF spec only "recommends" using ASCII character set.
    commentText = T.E.decodeUtf8With T.E.E.lenientDecode commentBytes
    subBlockSize = fromIntegral (BS.index buf 0)
    -- oldConsumed = consumed gif



{-
Try to parse all bytes shown in [1] "23. Graphic Control Extension.",
from Extension Introducer to Block Terminator, inclusive.

TODO: review these conversions from Word8 to integer. Maybe some of these
values in GC should be bytes?
-}
handleExtensionSubBlockGraphicControl :: Gif -> BS.ByteString -> Maybe Gif
handleExtensionSubBlockGraphicControl gif buf =
  -- Default value of transparentColorIndex field, set in Dillo's a_Gif_new(), was -1.
  Just gif {   consumed              = gifIncreasedConsumed gif (1 + subBlockSize)
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
parseSubBlocks :: Gif -> BS.ByteString -> (Gif -> BS.ByteString -> Maybe Gif) -> Maybe Gif
parseSubBlocks gif buf subBlockParser
  | BS.length buf == 0            = Just (gifNotEnoughData gif) -- Not enough space even for block terminator
  | BS.length buf <= subBlockSize = Just (gifNotEnoughData gif) -- Not enough data to parse sub-block in full
  | isBlockTerminator buf         = Just (gifForward gif 1)     -- +1 for Terminator that we have just detected.
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
parseExtension :: Gif -> BS.ByteString -> Maybe Gif
parseExtension gif buf
  | BS.length buf < 3                 = Just (gifNotEnoughData gif) -- 3: Extension Introducer, Extension Label and at least one byte of some data, perhaps Block Terminator
  | introducer /= extensionIntroducer = Nothing
  | otherwise                         =
    case dispatchSubBlocks gif (BS.drop 2 buf) label of -- 2: Drop Extension Introducer and Extension Label
      Just gifUpdated -> Just (if (gifParseSuccess gif gifUpdated) then (gifForward gifUpdated 2) else gif)
      Nothing -> Nothing
  where
    introducer = BS.index buf 0
    label = BS.index buf 1
    dispatchSubBlocks gif' buf' label'
      | label' == extensionTypeGraphicControl = parseSubBlocks gif' buf' handleExtensionSubBlockGraphicControl
      | label' == extensionTypeComment        = parseSubBlocks gif' buf' handleExtensionSubBlockComment
      | label' == extensionTypePlainText      = Nothing -- TODO: add code that consumes bytes in this extension
      | label' == extensionTypeApplication    = Nothing -- TODO: add code that consumes bytes in this extension
      | otherwise                             = Nothing -- Invalid extension label



gifForward :: Gif -> Int -> Gif
gifForward gif n = gif { consumed = (consumed gif) + n }



-- Increase 'consumed' field of gif by given amount
--
-- As the parser is successfully parsing more and more of byte stream, the
-- count of consumed bytes must be increased.
gifIncreasedConsumed :: Gif -> Int -> Int
gifIncreasedConsumed gif n = (consumed gif) + n



-- Append given text to gif's 'comment' field
--
-- Since specification allows multiple sub-blocks in Comment Extension, a
-- text from given sub-block must be *added* to text from parsed sub-blocks.
gifIncreasedComment :: Gif -> T.Text -> T.Text
gifIncreasedComment gif text = T.concat [comment gif, text]



gifNotEnoughData :: Gif -> Gif
gifNotEnoughData gif = gif { consumed = 0 }



-- If parser has correctly parsed and consumed a chunk of data in full,
-- return True.
gifParseSuccess gifPre gifPost = consumed gifPost > consumed gifPre



gifDefault = Gif {
    consumed = 0

  , comment = ""

  , transparentColorIndex = 0 -- TODO: Default value of transparentColorIndex field, set in Dillo's a_Gif_new(), was -1.
  , delayTime = 0
  , userInputFlag = 0
  , disposalMethod = 0
  }
