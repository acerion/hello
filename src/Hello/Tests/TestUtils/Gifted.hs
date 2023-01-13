{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




module Hello.Tests.Utils.Gifted
  (
    buildExtensionComment
  , subBlockSizeLimit
  )
where




{-
Miscellaneous code for GIF image format.

Technical reference documents:
1. https://www.w3.org/Graphics/GIF/spec-gif89a.txt
-}




import Data.Word (Word8)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.E
import qualified Data.ByteString as BS




-- [1] "15. Data Sub-blocks.".
-- "A data sub-block may contain from 0 to 255 data bytes."
subBlockSizeLimit :: Int
subBlockSizeLimit = 255

-- [1] "16. Block Terminator."
blockTerminator :: Word8
blockTerminator = 0x00




-- Build Comment Extension from given Text.
-- See "24. Comment Extension." in [1]
buildExtensionComment :: T.Text -> BS.ByteString
buildExtensionComment text = BS.concat [extensionHeader, commentSubBlocks chunks]
  where extensionHeader = BS.pack [0x21, 0xfe]
        chunks = T.chunksOf subBlockSizeLimit text

        -- Build Comment sub-blocks from given chunks of text.  The chunks
        -- are no longer than 255 characters, so they will fit nicely into a
        -- limited space of a sub-block.
        --
        -- TODO: what if there is some non-ASCII character in input text? A
        -- chunk may have 255 characters, but the characters will be encoded
        -- in e.g. 257 bytes.
        commentSubBlocks :: [T.Text] -> BS.ByteString
        commentSubBlocks (x:xs) = BS.concat [BS.pack [fromIntegral . T.length $ x], T.E.encodeUtf8 x, commentSubBlocks xs]
        commentSubBlocks []     = BS.pack [blockTerminator]
