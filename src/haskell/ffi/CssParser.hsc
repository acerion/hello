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




foreign export ccall "hll_nextToken" hll_nextToken :: Ptr HelloCssParser -> CString -> Int -> IO CString

#include "../hello.h"

data HelloCssParser = HelloCssParser {
    spaceSeparatedC :: Bool
  , consumedLenC    :: Int
  , tokenTypeC      :: Int
  }


instance Storable HelloCssParser where
  sizeOf    _ = #{size hll_CssParser}
  alignment _ = alignment (undefined :: Int)

  poke ptr hll_CssParser = do
    #{poke hll_CssParser, spaceSeparatedC} ptr $ spaceSeparatedC hll_CssParser
    #{poke hll_CssParser, consumedLenC} ptr    $ consumedLenC hll_CssParser
    #{poke hll_CssParser, tokenTypeC} ptr      $ tokenTypeC hll_CssParser

  peek ptr = return HelloCssParser
    `ap` (#{peek hll_CssParser, spaceSeparatedC} ptr)
    `ap` (#{peek hll_CssParser, consumedLenC} ptr)
    `ap` (#{peek hll_CssParser, tokenTypeC} ptr)




hll_nextToken :: Ptr HelloCssParser -> CString -> Int -> IO CString
hll_nextToken hll_cssparser cBuf inBlock = do
  buf <- BSU.unsafePackCString cBuf
  let parser = nextToken defaultParser{remainder = T.E.decodeLatin1 buf,
                                       withinBlock = inBlock > 0 }

  manipulateOutPtr hll_cssparser buf parser
  (newCString . T.unpack . tokenValue $ parser)
  where
    manipulateOutPtr hll_cssparser buf parser =
      -- Set fields in pointer to struct passed from C code.
      poke hll_cssparser $ HelloCssParser (spaceSeparated parser) (consumedLen buf parser) (getTokenType parser)
    consumedLen buf parser = (T.length . T.E.decodeLatin1 $ buf) - (T.length . remainder $ parser)
    getTokenType parser = case (tokenType parser) of
                            Just t | t == TokenInt    -> 0
                                   | t == TokenFloat  -> 1
                                   | t == TokenColor  -> 2
                                   | t == TokenSymbol -> 3
                                   | t == TokenString -> 4
                                   | t == TokenChar   -> 5
                                   | t == TokenEnd    -> 6
                                   | otherwise        -> 7
                            Nothing -> 0


