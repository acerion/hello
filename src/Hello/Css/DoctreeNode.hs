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
    DoctreeNode (..)
  , defaultDoctreeNode
  )
  where




import qualified Data.Text as T
import Debug.Trace




data DoctreeNode = DoctreeNode {
    uniqueNum      :: Int -- unique ascending id
  , htmlElementIdx :: Int -- Index to html.cc::Tags

  , selPseudoClass  :: T.Text
  , selId           :: T.Text
  , selClass        :: [T.Text]

  , dtnParent    :: Int -- Maybe DoctreeNode
  , dtnSibling   :: Int -- Maybe DoctreeNode
  , dtnLastChild :: Int -- Maybe DoctreeNode
  , dtnRootNode  :: Int -- Maybe DoctreeNode -- TODO: this field belongs to DocTree, not to DoctreeNode
  } deriving (Show)




defaultDoctreeNode = DoctreeNode
  { uniqueNum = (-1)
  , htmlElementIdx = (-1)

  , selPseudoClass = ""
  , selId = ""
  , selClass = []

  , dtnParent    = 0
  , dtnSibling   = 0
  , dtnLastChild = 0
  , dtnRootNode  = 0
  }
