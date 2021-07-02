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


{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}


module CssFfi() where




import Prelude
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import qualified Data.Text as T
import qualified Data.Text.Read as T.R
import qualified Data.Text.Encoding as T.E
import qualified Data.Text.Encoding.Error as T.E.E
import qualified Data.Text.IO as T.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Vector as V
import qualified Data.Sequence as S
import qualified Data.Foldable as Foldable
import Control.Applicative
import Control.Monad -- when
import Debug.Trace
import CssParserFFI
import CssParser
import Css




#include "../hello.h"




data FfiDoctreeNode = FfiDoctreeNode {
    uniqueNumC      :: CInt -- unique ascending id
  , htmlElementIdxC :: CInt -- Index to html.cc::Tags

  , elementSelectorPseudoClassC     :: CString
  , elementSelectorIdC              :: CString
  , elementSelectorClassC           :: CString
  , elementSelectorClassSizeC       :: CInt

  , parentC    :: Ptr FfiDoctreeNode
  , siblingC   :: Ptr FfiDoctreeNode
  , lastChildC :: Ptr FfiDoctreeNode
  } deriving (Show)




instance Storable FfiDoctreeNode where
  sizeOf    _ = #{size c_doctree_node_t}
  alignment _ = #{alignment c_doctree_node_t}

  poke ptr (FfiDoctreeNode a b c d e f g h i) = do
    #{poke c_doctree_node_t, c_unique_num}                    ptr a
    #{poke c_doctree_node_t, c_html_element_idx}              ptr b
    #{poke c_doctree_node_t, c_element_selector_pseudo_class} ptr c
    #{poke c_doctree_node_t, c_element_selector_id}           ptr d
    #{poke c_doctree_node_t, c_element_selector_class}        ptr e
    #{poke c_doctree_node_t, c_element_selector_class_size}   ptr f
    #{poke c_doctree_node_t, c_parent}                        ptr g
    #{poke c_doctree_node_t, c_sibling}                       ptr h
    #{poke c_doctree_node_t, c_last_child}                    ptr i

  peek ptr = do
    a <- #{peek c_doctree_node_t, c_unique_num}                    ptr
    b <- #{peek c_doctree_node_t, c_html_element_idx}              ptr
    c <- #{peek c_doctree_node_t, c_element_selector_pseudo_class} ptr
    d <- #{peek c_doctree_node_t, c_element_selector_id}           ptr
    e <- #{peek c_doctree_node_t, c_element_selector_class}        ptr
    f <- #{peek c_doctree_node_t, c_element_selector_class_size}   ptr
    g <- #{peek c_doctree_node_t, c_parent}                        ptr
    h <- #{peek c_doctree_node_t, c_sibling}                       ptr
    i <- #{peek c_doctree_node_t, c_last_child}                    ptr
    return (FfiDoctreeNode a b c d e f g h i)




foreign export ccall "hll_simpleSelectorMatches" hll_simpleSelectorMatches :: Ptr FfiCssSimpleSelector -> Ptr FfiDoctreeNode -> IO Int




-- Convert array of C strings to list of text. The size of array is n.
-- char * array[n] -> n -> acc -> result.
cStringArrayLenToTextList :: Ptr CString -> Int -> [T.Text] -> IO [T.Text]
cStringArrayLenToTextList cStringArray 0 acc = return acc
cStringArrayLenToTextList cStringArray n acc = do
  s :: CString <- peekElemOff cStringArray (n - 1)
  bs <- BSU.unsafePackCString s
  let t = T.E.decodeLatin1 bs
  cStringArrayLenToTextList cStringArray (n - 1) (t : acc)




hll_simpleSelectorMatches :: Ptr FfiCssSimpleSelector -> Ptr FfiDoctreeNode -> IO Int
hll_simpleSelectorMatches ptrStructSimpleSelector ptrStructDoctreeNode = do

  simSel :: CssSimpleSelector <- ffiCssSimpleSelectorToCssSimpleSelector ptrStructSimpleSelector
  dtn    :: DoctreeNode <- ffiDoctreeNodeToDoctreeNode ptrStructDoctreeNode
{-
  putStrLn ("FFI: simSel: " ++ show simSel)
  putStrLn ("FFI: dtn: "
            ++ "htmlElementIdx = " ++ (show $ htmlElementIdx dtn)
            ++ ", selPseudoClass = " ++ (show $ selPseudoClass dtn)
            ++ ", selId = " ++ (show $ selId dtn)
            ++ ", selClass = " ++ (show $ selClass dtn))
-}
  if simpleSelectorMatches simSel dtn
    then return 1 -- True
    else return 0 -- False



ffiCssSimpleSelectorToCssSimpleSelector :: Ptr FfiCssSimpleSelector -> IO CssSimpleSelector
ffiCssSimpleSelectorToCssSimpleSelector ptrStructSimpleSelector = do

  ffiSimSel :: FfiCssSimpleSelector <- peek ptrStructSimpleSelector

  i <- cStringToText . selectorIdC $ ffiSimSel

  let pcOffset = (#offset c_css_simple_selector_t, c_selector_pseudo_class)
  let pcStringArray :: Ptr CString = plusPtr ptrStructSimpleSelector pcOffset
  pc <- cStringArrayLenToTextList pcStringArray (fromIntegral . selectorPseudoClassSizeC $ ffiSimSel) []

  let cOffset = (#offset c_css_simple_selector_t, c_selector_class)
  let cStringArray :: Ptr CString = plusPtr ptrStructSimpleSelector cOffset
  c  <- cStringArrayLenToTextList cStringArray (fromIntegral . selectorClassSizeC $ ffiSimSel) []

  return CssSimpleSelector{selectorPseudoClass = pc
                          , selectorId         = i
                          , selectorClass      = c
                          , selectorElement    = fromIntegral . selectorElementC $ ffiSimSel
                          , combinator         = fromIntegral . combinatorC $ ffiSimSel
                          }




cStringToText :: CString -> IO T.Text
cStringToText ptr = do
  if nullPtr == ptr
    then return ""
    else do
    bs :: BS.ByteString <- BSU.unsafePackCString ptr
    return (T.E.decodeLatin1 bs)




ffiDoctreeNodeToDoctreeNode :: Ptr FfiDoctreeNode -> IO DoctreeNode
ffiDoctreeNodeToDoctreeNode ptrStructDoctreeNode = do

  ffiDtn <- peek ptrStructDoctreeNode
  pc <- cStringToText . elementSelectorPseudoClassC $ ffiDtn
  i  <- cStringToText . elementSelectorIdC $ ffiDtn

  let cOffset = (#offset c_doctree_node_t, c_element_selector_class)
  let cStringArray :: Ptr CString = plusPtr ptrStructDoctreeNode cOffset
  c  <- cStringArrayLenToTextList cStringArray (fromIntegral . elementSelectorClassSizeC $ ffiDtn) []

  return DoctreeNode{ uniqueNum = fromIntegral . uniqueNumC $ ffiDtn

                    , htmlElementIdx = fromIntegral . htmlElementIdxC $ ffiDtn
                    , selPseudoClass = pc
                    , selId          = i
                    , selClass       = c

                    , parent = undefined
                    , sibling = undefined
                    , lastChild = undefined
                    }


