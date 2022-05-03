{-
Copyright (C) 2021-2022 Kamil Ignacak acerion@wp.pl

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




module Hello.Html.Doctree
  (
    DoctreeItems (..)
  , Doctree (..)
  , doctreeCtor
  , defaultDoctree

  , doctreePushNode
  , doctreePopNode

  , getDtnParent
  , getDtnSibling
  )
  where




import qualified Data.Map as M
import qualified Data.Text as T
import Debug.Trace

import Hello.Html.DoctreeNode




type DoctreeItems = M.Map Int DoctreeNode
data Doctree = Doctree {
    topNodeNum  :: Int
  , rootNode    :: Int
  , root        :: DoctreeNode
  , nodes       :: DoctreeItems
  } deriving (Show)




defaultDoctree = Doctree {
    topNodeNum  = -1
  , rootNode    = -1
  , root        = defaultDoctreeNode
  , nodes       = M.empty
  }




getDtnParent tree dtn = M.lookup (dtnParentNum dtn) tree


getDtnSibling tree dtn = M.lookup (dtnSiblingNum dtn) tree




doctreeCtor :: Doctree
doctreeCtor = defaultDoctree




doctreePushNode :: Doctree -> Int -> IO (Doctree, DoctreeNode)
doctreePushNode doctree elementIdx = do

  putStrLn ("---- push " ++ (show elementIdx) ++ "\n")

  let currentNodeNum = M.size . nodes $ doctree

  let dtn = makeNewDtn elementIdx currentNodeNum
  (doctree2, dtn2) <- setRelations doctree dtn

  -- Insert node, update reference to top node.
  let doctree3 = doctree2 { topNodeNum = uniqueNum dtn2
                          , nodes      = M.insert currentNodeNum dtn2 (nodes doctree2)
                          }

  return (doctree3, dtn2)




{-
Set properties.
dtn->c_this_ptr = dtn;
dtn->c_unique_num = this_num;
dtn->c_html_element_idx = element_idx;
-}
makeNewDtn elementIdx num = defaultDoctreeNode { uniqueNum = num
                                               , htmlElementIdx = elementIdx
                                               }



{-
   /* Set relations. */
   c_doctree_node_t * parent = NULL;
   if (doctree->c_top_node_num == ROOT_NODE_NUM) {
      /* This is a first real element in html document, placed in the tree
         under a root element. */
      dtn->c_parent_num = ROOT_NODE_NUM;
      parent = doctree->c_root_node;
      dtn->c_sibling_num = parent->c_last_child_num;
      parent->c_last_child_num = dtn->c_unique_num;

   } else {
      /* This is an n-th element in html document, placed in the tree under
         some tree node. */
      dtn->c_parent_num = doctree->c_top_node_num;
      parent = doctree->c_nodes_array[dtn->c_parent_num];
      dtn->c_sibling_num = parent->c_last_child_num;
      parent->c_last_child_num = dtn->c_unique_num;
   }
-}
setRelations doctree dtn = if topNodeNum doctree == (-1)
                           then
                             do
                               let dtn3 = dtn { dtnParentNum = (-1) }
                               let parent = root doctree
                               let dtn4 = dtn3 { dtnSiblingNum = dtnLastChildNum parent }
                               let parent2 = parent { dtnLastChildNum = uniqueNum dtn4 }
                               let tree = doctree { root = parent2 }
                               return (tree, dtn4)
                           else
                             do
                               let dtn3 = dtn { dtnParentNum = topNodeNum doctree }
                               let parent = (nodes doctree) M.! (dtnParentNum dtn3)
                               let dtn4 = dtn3 { dtnSiblingNum = dtnLastChildNum parent }
                               let parent2 = parent { dtnLastChildNum = uniqueNum dtn4 }
                               let tree = doctree { nodes = M.insert (uniqueNum parent2) parent2 (nodes doctree) }
                               return (tree, dtn4)




doctreePopNode :: Doctree -> Doctree
doctreePopNode doctree = if uniqueNum dtn == 0 -- We are popping the element of html document that was added to the tree as the first one. What should now remain on top of the doctree is a tree's root element.
                         then doctree { topNodeNum = (-1) }
                         else doctree { topNodeNum = dtnParentNum dtn }

  where
    -- c_doctree_node_t * dtn = doctree->c_nodes_array[doctree->c_top_node_num];
    dtn = (nodes doctree) M.! (topNodeNum doctree)

{-
   if (0 == dtn->c_unique_num) {

      doctree->c_top_node_num = ROOT_NODE_NUM;
   } else {
      doctree->c_top_node_num = dtn->c_parent_num;
   }

-}

