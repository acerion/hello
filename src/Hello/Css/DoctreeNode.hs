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

This file is derived from dillo-3.0.5/src/css.cc (and doctree.hh).
Copyright assignments from css.cc file:
Copyright 2008-2014 Johannes Hofmann <Johannes.Hofmann@gmx.de>
-}




{-# LANGUAGE OverloadedStrings #-}




module Hello.Css.DoctreeNode
  (
    DoctreeItems (..)
  , Doctree (..)
  , defaultDoctree

  , DoctreeNode (..)
  , defaultDoctreeNode

  , getDtnParent
  , getDtnSibling

  )
  where




import qualified Data.Map as M
import qualified Data.Text as T
import Debug.Trace




type DoctreeItems = M.Map Int DoctreeNode
data Doctree = Doctree {
    topNode  :: Int
  , rootNode :: Int
  , nodes    :: DoctreeItems
  } deriving (Show)




defaultDoctree = Doctree {
    topNode  = -1
  , rootNode = -1
  , nodes    = M.empty
  }




data DoctreeNode = DoctreeNode {
    uniqueNum      :: Int -- unique ascending id
  , htmlElementIdx :: Int -- Index to html.cc::Tags
  , thisPtr        :: Int -- pointer to this element

  , selPseudoClass  :: T.Text
  , selId           :: T.Text
  , selClass        :: [T.Text]

  , dtnParentNum    :: Int
  , dtnSiblingNum   :: Int
  , dtnLastChildNum :: Int
  } deriving (Show)




defaultDoctreeNode = DoctreeNode
  { uniqueNum = (-1)
  , htmlElementIdx = (-1)
  , thisPtr = 0

  , selPseudoClass = ""
  , selId = ""
  , selClass = []

  , dtnParentNum    = 0
  , dtnSiblingNum   = 0
  , dtnLastChildNum = 0
  }




getDtnParent tree dtn = M.lookup (dtnParentNum dtn) tree


getDtnSibling tree dtn = M.lookup (dtnSiblingNum dtn) tree



