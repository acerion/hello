{-
Copyright (C) 2021-2023 Kamil Ignacak acerion@wp.pl

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
    Doctree (..)
  , doctreeCtor
  , defaultDoctree

  , doctreePushNode
  , doctreePopNode

  , getDtnUnsafe
  , getDtnParent
  , getDtnSibling

  , adjustTopNode
  )
where




import qualified Data.Map as M
--import Debug.Trace

import Hello.Html.DoctreeNode




type DoctreeItems = M.Map Int DoctreeNode
data Doctree = Doctree {
    topNodeNum  :: Int
  , rootNode    :: Int
  , root        :: DoctreeNode
  , nodes       :: DoctreeItems
  } deriving (Show, Eq)




defaultDoctree :: Doctree
defaultDoctree = Doctree {
    topNodeNum  = -1
  , rootNode    = -1
  , root        = defaultDoctreeNode
  , nodes       = M.empty
  }




getDtnUnsafe :: Doctree -> Int -> DoctreeNode
getDtnUnsafe tree dtnNum = (nodes tree) M.! dtnNum


getDtnParent :: Doctree -> DoctreeNode -> Maybe DoctreeNode
getDtnParent tree dtn = M.lookup (dtnParentNum dtn) (nodes tree)


getDtnSibling :: Doctree -> DoctreeNode -> Maybe DoctreeNode
getDtnSibling tree dtn = M.lookup (dtnSiblingNum dtn) (nodes tree)




doctreeCtor :: Doctree
doctreeCtor = defaultDoctree




doctreePushNode :: Doctree -> Int -> Doctree
doctreePushNode doctree elementIdx = insertNode (updateParent doctree (uniqueNum dtn)) dtn
  where
    currentNodeNum = M.size . nodes $ doctree
    dtn            = makeNewDtn (if pushingFirstNode then rootParent else someParent) elementIdx currentNodeNum

    -- Push node into tree.
    insertNode tree node = tree { topNodeNum = uniqueNum node
                                , nodes      = M.insert currentNodeNum node (nodes tree)
                                }

    -- Update properties of parent of currently pushed node.
    updateParent tree nodeNum = if pushingFirstNode
                                then tree { root = rootParent { dtnLastChildNum = nodeNum } }
                                else tree { nodes = M.adjust (\x -> x { dtnLastChildNum = nodeNum }) parentNum (nodes tree)}

    pushingFirstNode = topNodeNum doctree == (-1)
    parentNum  = topNodeNum doctree
    rootParent = root doctree
    someParent = (nodes doctree) M.! parentNum

    makeNewDtn parent elemIdx num = defaultDoctreeNode { uniqueNum      = num
                                                       , htmlElementIdx = elemIdx
                                                       , dtnParentNum   = uniqueNum parent
                                                       , dtnSiblingNum  = dtnLastChildNum parent
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




-- Adjust doctree's node that is a current 'top node'.
-- Second argument is a function that adjusts the node.
adjustTopNode :: Doctree -> (DoctreeNode -> DoctreeNode) -> Doctree
adjustTopNode doctree f = doctree { nodes = M.adjust f (uniqueNum dtn) (nodes doctree)}
  where
    dtn = (nodes doctree) M.! (topNodeNum doctree)






