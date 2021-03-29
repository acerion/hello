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
-}




module CssParserFFI(hll_nextToken
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
import CssParser




foreign export ccall "hll_nextToken" hll_nextToken :: Ptr HelloCssParser -> CString -> IO CString
foreign export ccall "hll_parseRgbFunction" hll_parseRgbFunction :: Ptr HelloCssParser -> CString -> IO Int

#include "../hello.h"

data HelloCssParser = HelloCssParser {
    spaceSeparatedC :: Int
  , bufOffsetC      :: Int
  , tokenTypeC      :: Int
  , withinBlockC    :: Int
  } deriving (Show)


instance Storable HelloCssParser where
  sizeOf    _ = #{size hll_CssParser}
  alignment _ = alignment (undefined :: Int) -- #{alignment hll_CssParser} --


  poke ptr (HelloCssParser spaceSeparatedC bufOffsetC tokenTypeC withinBlockC) = do
    #{poke hll_CssParser, spaceSeparatedC} ptr spaceSeparatedC
    #{poke hll_CssParser, bufOffsetC}      ptr bufOffsetC
    #{poke hll_CssParser, tokenTypeC}      ptr tokenTypeC
    #{poke hll_CssParser, withinBlockC}    ptr withinBlockC

{-
  poke ptr hll_CssParser = do
    #{poke hll_CssParser, spaceSeparatedC} ptr $ spaceSeparatedC hll_CssParser
    #{poke hll_CssParser, bufOffsetC} ptr      $ bufOffsetC hll_CssParser
    #{poke hll_CssParser, tokenTypeC} ptr      $ tokenTypeC hll_CssParser
    #{poke hll_CssParser, withinBlockC} ptr    $ withinBlockC hll_CssParser
-}
  peek ptr = do
    a <- #{peek hll_CssParser, spaceSeparatedC} ptr
    b <- #{peek hll_CssParser, bufOffsetC} ptr
    c <- #{peek hll_CssParser, tokenTypeC} ptr
    d <- #{peek hll_CssParser, withinBlockC} ptr
    return (HelloCssParser a b c d)
{-
  peek ptr = return HelloCssParser
    `ap` (#{peek hll_CssParser, spaceSeparatedC} ptr)
    `ap` (#{peek hll_CssParser, bufOffsetC} ptr)
    `ap` (#{peek hll_CssParser, tokenTypeC} ptr)
    `ap` (#{peek hll_CssParser, withinBlockC} ptr)
-}


hll_nextToken :: Ptr HelloCssParser -> CString -> IO CString
hll_nextToken hll_cssparser cBuf = do
  buf <- BSU.unsafePackCString cBuf
  oldParser <- peek hll_cssparser
  let inBlock = withinBlockC oldParser
  let (parser, token) = nextToken defaultParser{ remainder   = T.E.decodeLatin1 buf
                                               , withinBlock = inBlock > 0
                                               , bufOffset   = bufOffsetC oldParser
                                               }

  manipulateOutPtr hll_cssparser buf parser token inBlock
  (newCString . T.unpack . tokenValue $ token)
    where
      -- Set fields in pointer to struct passed from C code.
      manipulateOutPtr :: Ptr HelloCssParser -> BS.ByteString -> CssParser -> CssToken -> Int -> IO ()
      manipulateOutPtr hll_cssparser buf parser token inBlock = do
        poke hll_cssparser $ HelloCssParser (if spaceSeparated parser then 1 else 0) (bufOffset parser) (getTokenType token) inBlock





hll_parseRgbFunction :: Ptr HelloCssParser -> CString -> IO Int
hll_parseRgbFunction hll_cssparser cBuf = do
  buf <- BSU.unsafePackCString cBuf
  oldParser <- peek hll_cssparser
  let inBlock = withinBlockC oldParser
  let (parser, color) = parseRgbFunctionInt defaultParser{ remainder = T.E.decodeLatin1 buf
                                                         , withinBlock = inBlock > 0
                                                         , bufOffset = bufOffsetC oldParser
                                                         }

  putStr "\nParser = "
  putStr (show parser)
  putStr "\nColor = "
  putStr (show color)
  putStr "\n\n"

  manipulateOutPtr hll_cssparser buf parser inBlock
  case color of
    Just value -> return value
    Nothing    -> return 999999999 -- FIXME: Magic value treated as error
  where
      -- Set fields in pointer to struct passed from C code.
      manipulateOutPtr :: Ptr HelloCssParser -> BS.ByteString -> CssParser -> Int -> IO ()
      manipulateOutPtr hll_cssparser buf parser inBlock = do
        poke hll_cssparser $ HelloCssParser (if spaceSeparated parser then 1 else 0) (bufOffset parser) 2 inBlock




getTokenType parser = case (tokenType parser) of
                        Just t | t == TokenInt    -> 0
                               | t == TokenFloat  -> 1
                               | t == TokenColor  -> 2
                               | t == TokenSymbol -> 3
                               | t == TokenString -> 4
                               | t == TokenChar   -> 5
                               | t == TokenEnd    -> 6
                               | otherwise        -> 7
                        Nothing -> 6

