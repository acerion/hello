{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}




module Hello.Tests.Html.Doctree
  (
    testsHtmlDoctree
  )
where




--import Debug.Trace
import Test.HUnit

import Data.Map

import Hello.Html.Doctree
import Hello.Html.DoctreeNode




push :: Bool
push = True

pop :: Bool
pop  = False

-- Doctree push and pop operation that were executed when a test html
-- document was parsed by C++ dillo code and a doctree was built. If we now
-- play back the ops and repeat doctree push and pop function calls, we
-- should get the same doctree.
--
-- Second element of the tuple is an index of html element, passed to 'push'
-- function. For pop operations the second element is ignored.
ops :: [(Bool, Int)]
ops = [ ( push,  42 )
      , ( push,  39 )
      , ( push,  55 )
      , ( pop,   55 )
      , ( push,  81 )
      , ( pop,   81 )
      , ( push,  55 )
      , ( pop,   55 )
      , ( push,  55 )
      , ( pop,   55 )
      , ( push,  55 )
      , ( pop,   55 )
      , ( push,  51 )
      , ( pop,   51 )
      , ( pop,   39 )
      , ( push,  11 )
      , ( push,  0  )
      , ( pop,   0  )
      , ( push,  0  )
      , ( pop,   0  )
      , ( push,  77 )

      , ( push,  82 )
      , ( push,  78 )
      , ( push,  0  )
      , ( pop,   0  )
      , ( push,  33 )
      , ( pop,   33 )
      , ( push,  61 )
      , ( pop,   61 )
      , ( push,  61 )
      , ( push,  12 )
      , ( pop,   12 )
      , ( push,  12 )
      , ( pop,   12 )
      , ( pop,   61 )
      , ( push,  61 )
      , ( pop,   61 )
      , ( push,  34 )
      , ( pop,   34 )
      , ( push,  61 )
      , ( pop,   61 )
      , ( push,  85 )
      , ( push,  50 )
      , ( push,  0  )
      , ( pop,   0  )
      , ( pop,   50 )
      , ( push,  50 )
      , ( push,  0  )
      , ( pop,   0  )
      , ( pop,   50 )
      , ( push,  50 )
      , ( push,  0  )
      , ( pop,   0  )
      , ( pop,   50 )
      , ( push,  50 )
      , ( push,  0  )
      , ( pop,   0  )
      , ( pop,   50 )
      , ( push,  50 )
      , ( push,  0  )
      , ( pop,   0  )
      , ( pop,   50 )
      , ( push,  50 )
      , ( push,  0  )
      , ( pop,   0  )
      , ( pop,   50 )
      , ( pop,   85 )
      , ( pop,   78 )
      , ( pop,   82 )

      , ( pop,   77 )
      , ( push,  12 )
      , ( pop,   12 )
      , ( push,  12 )
      , ( pop,   12 )
      , ( push,  77 )

      , ( push,  82 )
      , ( push,  78 )
      , ( push,  34 )
      , ( push,  0  )
      , ( pop,   0  )
      , ( pop,   34 )
      , ( push,  61 )
      , ( pop,   61 )
      , ( push,  61 )
      , ( pop,   61 )
      , ( push,  61 )
      , ( pop,   61 )
      , ( push,  61 )
      , ( pop,   61 )
      , ( push,  61 )
      , ( pop,   61 )
      , ( push,  61 )
      , ( push,  12 )
      , ( pop,   12 )
      , ( pop,   61 )
      , ( push,  85 )
      , ( push,  50 )
      , ( push,  83 )
      , ( pop,   83 )
      , ( pop,   50 )
      , ( push,  50 )
      , ( pop,   50 )
      , ( push,  50 )
      , ( pop,   50 )
      , ( push,  50 )
      , ( push,  83 )
      , ( pop,   83 )
      , ( push,  83 )
      , ( pop,   83 )
      , ( pop,   50 )
      , ( push,  50 )
      , ( push,  83 )
      , ( pop,   83 )
      , ( pop,   50 )
      , ( pop,   85 )
      , ( push,  61 )
      , ( pop,   61 )
      , ( push,  61 )
      , ( pop,   61 )
      , ( push,  85 )
      , ( push,  50 )
      , ( push,  7 )
      , ( pop,   7 )
      , ( pop,   50 )
      , ( push,  50 )
      , ( push,  7 )
      , ( pop,   7 )
      , ( pop,   50 )
      , ( pop,   85 )
      , ( push,  0  )
      , ( pop,   0  )
      , ( pop,   78 )
      , ( pop,   82 )

      , ( pop,   77 )
      , ( push,  12 )
      , ( pop,   12 )
      , ( push,  12 )
      , ( pop,   12 )
      , ( push,  77 )

      , ( push,  82 )
      , ( push,  78 )
      , ( push,  34 )
      , ( push,  0  )
      , ( pop,   0  )
      , ( pop,   34 )
      , ( push,  58 )
      , ( push,  50 )
      , ( push,  43 )
      , ( pop,   43 )
      , ( push,  43 )
      , ( pop,   43 )
      , ( pop,   50 )
      , ( push,  50 )
      , ( push,  43 )
      , ( pop,   43 )
      , ( push,  12 )
      , ( pop,   12 )
      , ( push,  62 )
      , ( pop,   62 )
      , ( pop,   50 )
      , ( push,  50 )
      , ( push,  83 )
      , ( pop,   83 )
      , ( push,  83 )
      , ( pop,   83 )
      , ( push,  83 )
      , ( pop,   83 )
      , ( push,  0  )
      , ( pop,   0  )
      , ( pop,   50 )
      , ( push,  50 )
      , ( push,  43 )
      , ( pop,   43 )
      , ( push,  12 )
      , ( pop,   12 )
      , ( push,  62 )
      , ( push,  7 )
      , ( pop,   7 )
      , ( pop,   62 )
      , ( push,  83 )
      , ( pop,   83 )
      , ( push,  83 )
      , ( pop,   83 )
      , ( pop,   50 )
      , ( push,  50 )
      , ( push,  12 )
      , ( pop,   12 )
      , ( push,  62 )
      , ( push,  7 )
      , ( pop,   7 )
      , ( pop,   62 )
      , ( push,  83 )
      , ( pop,   83 )
      , ( push,  83 )
      , ( pop,   83 )
      , ( push,  43 )
      , ( pop,   43 )
      , ( pop,   50 )
      , ( push,  50 )
      , ( push,  43 )
      , ( pop,   43 )
      , ( push,  12 )
      , ( pop,   12 )
      , ( push,  62 )
      , ( push,  7 )
      , ( pop,   7 )
      , ( pop,   62 )
      , ( push,  83 )
      , ( pop,   83 )
      , ( push,  83 )
      , ( pop,   83 )
      , ( push,  83 )
      , ( pop,   83 )
      , ( push,  83 )
      , ( pop,   83 )
      , ( pop,   50 )
      , ( push,  50 )
      , ( push,  12 )
      , ( pop,   12 )
      , ( push,  62 )
      , ( push,  7 )
      , ( push,  43 )
      , ( pop,   43 )
      , ( pop,   7 )
      , ( pop,   62 )
      , ( push,  83 )
      , ( pop,   83 )
      , ( pop,   50 )
      , ( push,  50 )
      , ( push,  43 )
      , ( pop,   43 )
      , ( push,  12 )
      , ( pop,   12 )
      , ( push,  62 )
      , ( push,  7 )
      , ( pop,   7 )
      , ( pop,   62 )
      , ( push,  83 )
      , ( pop,   83 )
      , ( push,  83 )
      , ( pop,   83 )
      , ( pop,   50 )
      , ( push,  50 )
      , ( push,  12 )
      , ( pop,   12 )
      , ( push,  62 )
      , ( push,  7 )
      , ( pop,   7 )
      , ( pop,   62 )
      , ( push,  62 )
      , ( push,  7 )
      , ( pop,   7 )
      , ( pop,   62 )
      , ( push,  83 )
      , ( pop,   83 )
      , ( push,  83 )
      , ( pop,   83 )
      , ( push,  7 )
      , ( pop,   7 )
      , ( push,  43 )
      , ( pop,   43 )
      , ( push,  83 )
      , ( pop,   83 )
      , ( push,  43 )
      , ( pop,   43 )
      , ( pop,   50 )
      , ( push,  50 )
      , ( push,  43 )
      , ( pop,   43 )
      , ( push,  12 )
      , ( pop,   12 )
      , ( push,  62 )
      , ( push,  7 )
      , ( pop,   7 )
      , ( pop,   62 )
      , ( pop,   50 )
      , ( pop,   58 )
      , ( push,  61 )
      , ( push,  83 )
      , ( pop,   83 )
      , ( push,  0  )
      , ( pop,   0  )
      , ( pop,   61 )
      , ( push,  61 )
      , ( push,  0  )
      , ( pop,   0  )
      , ( pop,   61 )
      , ( push,  0  )
      , ( pop,   0  )
      , ( pop,   78 )
      , ( pop,   82 )

      , ( pop,   77 )
      , ( push,  12 )
      , ( pop,   12 )
      , ( push,  12 )
      , ( pop,   12 )
      , ( push,  12 )
      , ( pop,   12 )
      , ( push,  77 )

      , ( push,  82 )
      , ( push,  78 )
      , ( push,  34 )
      , ( push,  0  )
      , ( pop,   0  )
      , ( pop,   34 )
      , ( push,  61 )
      , ( push,  43 )
      , ( pop,   43 )
      , ( push,  83 )
      , ( pop,   83 )
      , ( push,  43 )
      , ( pop,   43 )
      , ( pop,   61 )
      , ( push,  0  )
      , ( pop,   0  )
      , ( pop,   78 )
      , ( pop,   82 )

      , ( pop,   77 )
      , ( push,  12 )
      , ( pop,   12 )
      , ( push,  12 )
      , ( pop,   12 )
      , ( push,  77 )

      , ( push,  82 )
      , ( push,  78 )
      , ( push,  34 )
      , ( push,  0  )
      , ( pop,   0  )
      , ( pop,   34 )
      , ( push,  61 )
      , ( push,  0  )
      , ( pop,   0  )
      , ( pop,   61 )
      , ( push,  61 )
      , ( push,  0  )
      , ( pop,   0  )
      , ( pop,   61 )
      , ( push,  61 )
      , ( push,  0  )
      , ( pop,   0  )
      , ( pop,   61 )
      , ( push,  61 )
      , ( push,  0  )
      , ( pop,   0  )
      , ( pop,   61 )
      , ( push,  61 )
      , ( pop,   61 )
      , ( push,  61 )
      , ( push,  0  )
      , ( pop,   0  )
      , ( pop,   61 )
      , ( push,  61 )
      , ( push,  0  )
      , ( pop,   0  )
      , ( pop,   61 )
      , ( push,  61 )
      , ( push,  0  )
      , ( pop,   0  )
      , ( pop,   61 )
      , ( push,  61 )
      , ( push,  0  )
      , ( pop,   0  )
      , ( pop,   61 )
      , ( push,  61 )
      , ( push,  0  )
      , ( pop,   0  )
      , ( pop,   61 )
      , ( push,  12 )
      , ( pop,   12 )
      , ( push,  0  )
      , ( pop,   0  )
      , ( pop,   78 )
      , ( pop,   82 )

      , ( pop,   77 )
      , ( push,  12 )
      , ( pop,   12 )
      , ( push,  12 )
      , ( pop,   12 )
      , ( push,  77 )

      , ( push,  82 )
      , ( push,  78 )
      , ( push,  34 )
      , ( push,  0  )
      , ( pop,   0  )
      , ( pop,   34 )
      , ( push,  85 )
      , ( push,  50 )
      , ( pop,   50 )
      , ( pop,   85 )
      , ( pop,   78 )
      , ( pop,   82 )
      , ( push,  82 )
      , ( push,  78 )
      , ( push,  34 )
      , ( push,  0  )
      , ( pop,   0  )
      , ( pop,   34 )
      , ( push,  61 )
      , ( pop,   61 )
      , ( pop,   78 )
      , ( pop,   82 )

      , ( pop,   77 )
      , ( push,  77 )

      , ( push,  82 )
      , ( push,  78 )
      , ( push,  0  )
      , ( pop,   0  )
      , ( push,  12 )
      , ( pop,   12 )
      , ( push,  0  )
      , ( pop,   0  )
      , ( push,  61 )
      , ( pop,   61 )
      , ( pop,   78 )
      , ( pop,   82 )

      , ( pop,   77 )
      , ( pop,   11 )
      , ( pop,   42 )
      ]




-- A doctree that was a result of executing the Haskell doctree push and pop
-- operations in parallel with C++ doctree push and pop operations.
--
-- The layout of the nodes in the definition below reflects parent-child
-- relations in the tree. It also reflects layout of the input html document.
expectedDoctree :: Doctree
expectedDoctree = Doctree {topNodeNum = -1, rootNode = 0,
                           root = DoctreeNode {uniqueNum = -1, htmlElementIdx = -1, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 0, dtnSiblingNum = 0, dtnLastChildNum = 0},
                           nodes = fromList [

   (0,DoctreeNode                              {uniqueNum = 0,   htmlElementIdx = 42, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = -1,  dtnSiblingNum = 0,   dtnLastChildNum = 8  }),
      (1,DoctreeNode                           {uniqueNum = 1,   htmlElementIdx = 39, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 0,   dtnSiblingNum = 0,   dtnLastChildNum = 7  }),
         (2,DoctreeNode                        {uniqueNum = 2,   htmlElementIdx = 55, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 1,   dtnSiblingNum = 0,   dtnLastChildNum = 0  }),

         (3,DoctreeNode                        {uniqueNum = 3,   htmlElementIdx = 81, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 1,   dtnSiblingNum = 2,   dtnLastChildNum = 0  }),

         (4,DoctreeNode                        {uniqueNum = 4,   htmlElementIdx = 55, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 1,   dtnSiblingNum = 3,   dtnLastChildNum = 0  }),

         (5,DoctreeNode                        {uniqueNum = 5,   htmlElementIdx = 55, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 1,   dtnSiblingNum = 4,   dtnLastChildNum = 0  }),

         (6,DoctreeNode                        {uniqueNum = 6,   htmlElementIdx = 55, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 1,   dtnSiblingNum = 5,   dtnLastChildNum = 0  }),

         (7,DoctreeNode                        {uniqueNum = 7,   htmlElementIdx = 51, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 1,   dtnSiblingNum = 6,   dtnLastChildNum = 0  }),


      (8,DoctreeNode                           {uniqueNum = 8,   htmlElementIdx = 11, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 0,   dtnSiblingNum = 1,   dtnLastChildNum = 202}),
         (9,DoctreeNode                        {uniqueNum = 9,   htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 8,   dtnSiblingNum = 0,   dtnLastChildNum = 0  }),

         (10,DoctreeNode                       {uniqueNum = 10,  htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 8,   dtnSiblingNum = 9,   dtnLastChildNum = 0  }),

         (11,DoctreeNode                       {uniqueNum = 11,  htmlElementIdx = 77, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 8,   dtnSiblingNum = 10,  dtnLastChildNum = 12 }),

            (12,DoctreeNode                    {uniqueNum = 12,  htmlElementIdx = 82, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 11,  dtnSiblingNum = 0,   dtnLastChildNum = 13 }),
               (13,DoctreeNode                 {uniqueNum = 13,  htmlElementIdx = 78, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 12,  dtnSiblingNum = 0,   dtnLastChildNum = 23 }),
                  (14,DoctreeNode              {uniqueNum = 14,  htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 13,  dtnSiblingNum = 0,   dtnLastChildNum = 0  }),

                  (15,DoctreeNode              {uniqueNum = 15,  htmlElementIdx = 33, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 13,  dtnSiblingNum = 14,  dtnLastChildNum = 0  }),

                  (16,DoctreeNode              {uniqueNum = 16,  htmlElementIdx = 61, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 13,  dtnSiblingNum = 15,  dtnLastChildNum = 0  }),

                  (17,DoctreeNode              {uniqueNum = 17,  htmlElementIdx = 61, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 13,  dtnSiblingNum = 16,  dtnLastChildNum = 19 }),
                     (18,DoctreeNode           {uniqueNum = 18,  htmlElementIdx = 12, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 17,  dtnSiblingNum = 0,   dtnLastChildNum = 0  }),

                     (19,DoctreeNode           {uniqueNum = 19,  htmlElementIdx = 12, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 17,  dtnSiblingNum = 18,  dtnLastChildNum = 0  }),


                  (20,DoctreeNode              {uniqueNum = 20,  htmlElementIdx = 61, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 13,  dtnSiblingNum = 17,  dtnLastChildNum = 0  }),

                  (21,DoctreeNode              {uniqueNum = 21,  htmlElementIdx = 34, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 13,  dtnSiblingNum = 20,  dtnLastChildNum = 0  }),

                  (22,DoctreeNode              {uniqueNum = 22,  htmlElementIdx = 61, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 13,  dtnSiblingNum = 21,  dtnLastChildNum = 0  }),

                  (23,DoctreeNode              {uniqueNum = 23,  htmlElementIdx = 85, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 13,  dtnSiblingNum = 22,  dtnLastChildNum = 34 }),
                     (24,DoctreeNode           {uniqueNum = 24,  htmlElementIdx = 50, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 23,  dtnSiblingNum = 0,   dtnLastChildNum = 25 }),
                         (25,DoctreeNode       {uniqueNum = 25,  htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 24,  dtnSiblingNum = 0,   dtnLastChildNum = 0  }),


                     (26,DoctreeNode           {uniqueNum = 26,  htmlElementIdx = 50, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 23,  dtnSiblingNum = 24,  dtnLastChildNum = 27 }),
                        (27,DoctreeNode        {uniqueNum = 27,  htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 26,  dtnSiblingNum = 0,   dtnLastChildNum = 0  }),


                     (28,DoctreeNode           {uniqueNum = 28,  htmlElementIdx = 50, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 23,  dtnSiblingNum = 26,  dtnLastChildNum = 29 }),
                        (29,DoctreeNode        {uniqueNum = 29,  htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 28,  dtnSiblingNum = 0,   dtnLastChildNum = 0  }),


                    (30,DoctreeNode            {uniqueNum = 30,  htmlElementIdx = 50, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 23,  dtnSiblingNum = 28,  dtnLastChildNum = 31 }),
                       (31,DoctreeNode         {uniqueNum = 31,  htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 30,  dtnSiblingNum = 0,   dtnLastChildNum = 0  }),


                    (32,DoctreeNode            {uniqueNum = 32,  htmlElementIdx = 50, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 23,  dtnSiblingNum = 30,  dtnLastChildNum = 33 }),
                       (33,DoctreeNode         {uniqueNum = 33,  htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 32,  dtnSiblingNum = 0,   dtnLastChildNum = 0  }),


                    (34,DoctreeNode            {uniqueNum = 34,  htmlElementIdx = 50, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 23,  dtnSiblingNum = 32,  dtnLastChildNum = 35 }),
                       (35,DoctreeNode         {uniqueNum = 35,  htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 34,  dtnSiblingNum = 0,   dtnLastChildNum = 0  }),







         (36,DoctreeNode                       {uniqueNum = 36,  htmlElementIdx = 12, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 8,   dtnSiblingNum = 11,  dtnLastChildNum = 0  }),

         (37,DoctreeNode                       {uniqueNum = 37,  htmlElementIdx = 12, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 8,   dtnSiblingNum = 36,  dtnLastChildNum = 0  }),

         (38,DoctreeNode                       {uniqueNum = 38,  htmlElementIdx = 77, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 8,   dtnSiblingNum = 37,  dtnLastChildNum = 39 }),

            (39,DoctreeNode                    {uniqueNum = 39,  htmlElementIdx = 82, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 38,  dtnSiblingNum = 0,   dtnLastChildNum = 40 }),
               (40,DoctreeNode                 {uniqueNum = 40,  htmlElementIdx = 78, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 39,  dtnSiblingNum = 0,   dtnLastChildNum = 67 }),
                  (41,DoctreeNode              {uniqueNum = 41,  htmlElementIdx = 34, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 40,  dtnSiblingNum = 0,   dtnLastChildNum = 42 }),
                     (42,DoctreeNode           {uniqueNum = 42,  htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 41,  dtnSiblingNum = 0,   dtnLastChildNum = 0  }),


                  (43,DoctreeNode              {uniqueNum = 43,  htmlElementIdx = 61, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 40,  dtnSiblingNum = 41,  dtnLastChildNum = 0  }),

                  (44,DoctreeNode              {uniqueNum = 44,  htmlElementIdx = 61, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 40,  dtnSiblingNum = 43,  dtnLastChildNum = 0  }),

                  (45,DoctreeNode              {uniqueNum = 45,  htmlElementIdx = 61, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 40,  dtnSiblingNum = 44,  dtnLastChildNum = 0  }),

                  (46,DoctreeNode              {uniqueNum = 46,  htmlElementIdx = 61, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 40,  dtnSiblingNum = 45,  dtnLastChildNum = 0  }),

                  (47,DoctreeNode              {uniqueNum = 47,  htmlElementIdx = 61, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 40,  dtnSiblingNum = 46,  dtnLastChildNum = 0  }),

                  (48,DoctreeNode              {uniqueNum = 48,  htmlElementIdx = 61, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 40,  dtnSiblingNum = 47,  dtnLastChildNum = 49 }),
                     (49,DoctreeNode           {uniqueNum = 49,  htmlElementIdx = 12, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 48,  dtnSiblingNum = 0,   dtnLastChildNum = 0  }),


                  (50,DoctreeNode              {uniqueNum = 50,  htmlElementIdx = 85, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 40,  dtnSiblingNum = 48,  dtnLastChildNum = 58 }),
                     (51,DoctreeNode           {uniqueNum = 51,  htmlElementIdx = 50, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 50,  dtnSiblingNum = 0,   dtnLastChildNum = 52 }),
                        (52,DoctreeNode        {uniqueNum = 52,  htmlElementIdx = 83, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 51,  dtnSiblingNum = 0,   dtnLastChildNum = 0  }),


                     (53,DoctreeNode           {uniqueNum = 53,  htmlElementIdx = 50, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 50,  dtnSiblingNum = 51,  dtnLastChildNum = 0  }),

                     (54,DoctreeNode           {uniqueNum = 54,  htmlElementIdx = 50, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 50,  dtnSiblingNum = 53,  dtnLastChildNum = 0  }),

                     (55,DoctreeNode           {uniqueNum = 55,  htmlElementIdx = 50, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 50,  dtnSiblingNum = 54,  dtnLastChildNum = 57 }),
                        (56,DoctreeNode        {uniqueNum = 56,  htmlElementIdx = 83, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 55,  dtnSiblingNum = 0,   dtnLastChildNum = 0  }),

                        (57,DoctreeNode        {uniqueNum = 57,  htmlElementIdx = 83, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 55,  dtnSiblingNum = 56,  dtnLastChildNum = 0  }),


                     (58,DoctreeNode           {uniqueNum = 58,  htmlElementIdx = 50, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 50,  dtnSiblingNum = 55,  dtnLastChildNum = 59 }),
                        (59,DoctreeNode        {uniqueNum = 59,  htmlElementIdx = 83, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 58,  dtnSiblingNum = 0,   dtnLastChildNum = 0  }),



                  (60,DoctreeNode              {uniqueNum = 60,  htmlElementIdx = 61, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 40,  dtnSiblingNum = 50,  dtnLastChildNum = 0  }),

                  (61,DoctreeNode              {uniqueNum = 61,  htmlElementIdx = 61, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 40,  dtnSiblingNum = 60,  dtnLastChildNum = 0  }),

                  (62,DoctreeNode              {uniqueNum = 62,  htmlElementIdx = 85, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 40,  dtnSiblingNum = 61,  dtnLastChildNum = 65 }),
                     (63,DoctreeNode           {uniqueNum = 63,  htmlElementIdx = 50, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 62,  dtnSiblingNum = 0,   dtnLastChildNum = 64 }),
                        (64,DoctreeNode        {uniqueNum = 64,  htmlElementIdx = 7,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 63,  dtnSiblingNum = 0,   dtnLastChildNum = 0  }),


                     (65,DoctreeNode           {uniqueNum = 65,  htmlElementIdx = 50, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 62,  dtnSiblingNum = 63,  dtnLastChildNum = 66 }),
                        (66,DoctreeNode        {uniqueNum = 66,  htmlElementIdx = 7,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 65,  dtnSiblingNum = 0,   dtnLastChildNum = 0  }),



                  (67,DoctreeNode              {uniqueNum = 67,  htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 40,  dtnSiblingNum = 62,  dtnLastChildNum = 0  }),





         (68,DoctreeNode                       {uniqueNum = 68,  htmlElementIdx = 12, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 8,   dtnSiblingNum = 38,  dtnLastChildNum = 0  }),

         (69,DoctreeNode                       {uniqueNum = 69,  htmlElementIdx = 12, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 8,   dtnSiblingNum = 68,  dtnLastChildNum = 0  }),

         (70,DoctreeNode                       {uniqueNum = 70,  htmlElementIdx = 77, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 8,   dtnSiblingNum = 69,  dtnLastChildNum = 71 }),

            (71,DoctreeNode                    {uniqueNum = 71,  htmlElementIdx = 82, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 70,  dtnSiblingNum = 0,   dtnLastChildNum = 72 }),
               (72,DoctreeNode                 {uniqueNum = 72,  htmlElementIdx = 78, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 71,  dtnSiblingNum = 0,   dtnLastChildNum = 146}),
                  (73,DoctreeNode              {uniqueNum = 73,  htmlElementIdx = 34, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 72,  dtnSiblingNum = 0,   dtnLastChildNum = 74 }),
                     (74,DoctreeNode           {uniqueNum = 74,  htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 73,  dtnSiblingNum = 0,   dtnLastChildNum = 0  }),


                  (75,DoctreeNode              {uniqueNum = 75,  htmlElementIdx = 58, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 72,  dtnSiblingNum = 73,  dtnLastChildNum = 136}),
                     (76,DoctreeNode           {uniqueNum = 76,  htmlElementIdx = 50, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 75,  dtnSiblingNum = 0,   dtnLastChildNum = 78 }),
                        (77,DoctreeNode        {uniqueNum = 77,  htmlElementIdx = 43, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 76,  dtnSiblingNum = 0,   dtnLastChildNum = 0  }),

                        (78,DoctreeNode        {uniqueNum = 78,  htmlElementIdx = 43, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 76,  dtnSiblingNum = 77,  dtnLastChildNum = 0  }),


                     (79,DoctreeNode           {uniqueNum = 79,  htmlElementIdx = 50, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 75,  dtnSiblingNum = 76,  dtnLastChildNum = 82 }),
                        (80,DoctreeNode        {uniqueNum = 80,  htmlElementIdx = 43, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 79,  dtnSiblingNum = 0,   dtnLastChildNum = 0  }),

                        (81,DoctreeNode        {uniqueNum = 81,  htmlElementIdx = 12, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 79,  dtnSiblingNum = 80,  dtnLastChildNum = 0  }),

                        (82,DoctreeNode        {uniqueNum = 82,  htmlElementIdx = 62, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 79,  dtnSiblingNum = 81,  dtnLastChildNum = 0  }),


                     (83,DoctreeNode           {uniqueNum = 83,  htmlElementIdx = 50, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 75,  dtnSiblingNum = 79,  dtnLastChildNum = 87 }),
                        (84,DoctreeNode        {uniqueNum = 84,  htmlElementIdx = 83, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 83,  dtnSiblingNum = 0,   dtnLastChildNum = 0  }),

                        (85,DoctreeNode        {uniqueNum = 85,  htmlElementIdx = 83, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 83,  dtnSiblingNum = 84,  dtnLastChildNum = 0  }),

                        (86,DoctreeNode        {uniqueNum = 86,  htmlElementIdx = 83, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 83,  dtnSiblingNum = 85,  dtnLastChildNum = 0  }),

                        (87,DoctreeNode        {uniqueNum = 87,  htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 83,  dtnSiblingNum = 86,  dtnLastChildNum = 0  }),


                     (88,DoctreeNode           {uniqueNum = 88,  htmlElementIdx = 50, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 75,  dtnSiblingNum = 83,  dtnLastChildNum = 94 }),
                        (89,DoctreeNode        {uniqueNum = 89,  htmlElementIdx = 43, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 88,  dtnSiblingNum = 0,   dtnLastChildNum = 0  }),

                        (90,DoctreeNode        {uniqueNum = 90,  htmlElementIdx = 12, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 88,  dtnSiblingNum = 89,  dtnLastChildNum = 0  }),

                        (91,DoctreeNode        {uniqueNum = 91,  htmlElementIdx = 62, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 88,  dtnSiblingNum = 90,  dtnLastChildNum = 92 }),
                           (92,DoctreeNode     {uniqueNum = 92,  htmlElementIdx = 7,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 91,  dtnSiblingNum = 0,   dtnLastChildNum = 0  }),


                        (93,DoctreeNode        {uniqueNum = 93,  htmlElementIdx = 83, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 88,  dtnSiblingNum = 91,  dtnLastChildNum = 0  }),

                        (94,DoctreeNode        {uniqueNum = 94,  htmlElementIdx = 83, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 88,  dtnSiblingNum = 93,  dtnLastChildNum = 0  }),


                     (95,DoctreeNode           {uniqueNum = 95,  htmlElementIdx = 50, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 75,  dtnSiblingNum = 88,  dtnLastChildNum = 101}),
                        (96,DoctreeNode        {uniqueNum = 96,  htmlElementIdx = 12, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 95,  dtnSiblingNum = 0,   dtnLastChildNum = 0  }),

                        (97,DoctreeNode        {uniqueNum = 97,  htmlElementIdx = 62, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 95,  dtnSiblingNum = 96,  dtnLastChildNum = 98 }),
                           (98,DoctreeNode     {uniqueNum = 98,  htmlElementIdx = 7,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 97,  dtnSiblingNum = 0,   dtnLastChildNum = 0  }),


                        (99,DoctreeNode        {uniqueNum = 99,  htmlElementIdx = 83, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 95,  dtnSiblingNum = 97,  dtnLastChildNum = 0  }),

                        (100,DoctreeNode       {uniqueNum = 100, htmlElementIdx = 83, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 95,  dtnSiblingNum = 99,  dtnLastChildNum = 0  }),

                        (101,DoctreeNode       {uniqueNum = 101, htmlElementIdx = 43, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 95,  dtnSiblingNum = 100, dtnLastChildNum = 0  }),


                     (102,DoctreeNode          {uniqueNum = 102, htmlElementIdx = 50, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 75,  dtnSiblingNum = 95,  dtnLastChildNum = 110}),
                        (103,DoctreeNode       {uniqueNum = 103, htmlElementIdx = 43, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 102, dtnSiblingNum = 0,   dtnLastChildNum = 0  }),

                        (104,DoctreeNode       {uniqueNum = 104, htmlElementIdx = 12, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 102, dtnSiblingNum = 103, dtnLastChildNum = 0  }),

                        (105,DoctreeNode       {uniqueNum = 105, htmlElementIdx = 62, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 102, dtnSiblingNum = 104, dtnLastChildNum = 106}),
                           (106,DoctreeNode    {uniqueNum = 106, htmlElementIdx = 7,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 105, dtnSiblingNum = 0,   dtnLastChildNum = 0  }),


                        (107,DoctreeNode       {uniqueNum = 107, htmlElementIdx = 83, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 102, dtnSiblingNum = 105, dtnLastChildNum = 0  }),

                        (108,DoctreeNode       {uniqueNum = 108, htmlElementIdx = 83, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 102, dtnSiblingNum = 107, dtnLastChildNum = 0  }),

                        (109,DoctreeNode       {uniqueNum = 109, htmlElementIdx = 83, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 102, dtnSiblingNum = 108, dtnLastChildNum = 0  }),

                        (110,DoctreeNode       {uniqueNum = 110, htmlElementIdx = 83, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 102, dtnSiblingNum = 109, dtnLastChildNum = 0  }),


                     (111,DoctreeNode          {uniqueNum = 111, htmlElementIdx = 50, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 75,  dtnSiblingNum = 102, dtnLastChildNum = 116}),
                        (112,DoctreeNode       {uniqueNum = 112, htmlElementIdx = 12, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 111, dtnSiblingNum = 0,   dtnLastChildNum = 0  }),

                        (113,DoctreeNode       {uniqueNum = 113, htmlElementIdx = 62, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 111, dtnSiblingNum = 112, dtnLastChildNum = 114}),
                           (114,DoctreeNode    {uniqueNum = 114, htmlElementIdx = 7,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 113, dtnSiblingNum = 0,   dtnLastChildNum = 115}),
                              (115,DoctreeNode {uniqueNum = 115, htmlElementIdx = 43, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 114, dtnSiblingNum = 0,   dtnLastChildNum = 0  }),



                        (116,DoctreeNode       {uniqueNum = 116, htmlElementIdx = 83, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 111, dtnSiblingNum = 113, dtnLastChildNum = 0  }),


                     (117,DoctreeNode          {uniqueNum = 117, htmlElementIdx = 50, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 75,  dtnSiblingNum = 111, dtnLastChildNum = 123}),
                        (118,DoctreeNode       {uniqueNum = 118, htmlElementIdx = 43, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 117, dtnSiblingNum = 0,   dtnLastChildNum = 0  }),

                        (119,DoctreeNode       {uniqueNum = 119, htmlElementIdx = 12, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 117, dtnSiblingNum = 118, dtnLastChildNum = 0  }),

                        (120,DoctreeNode       {uniqueNum = 120, htmlElementIdx = 62, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 117, dtnSiblingNum = 119, dtnLastChildNum = 121}),
                           (121,DoctreeNode    {uniqueNum = 121, htmlElementIdx = 7,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 120, dtnSiblingNum = 0,   dtnLastChildNum = 0  }),


                        (122,DoctreeNode       {uniqueNum = 122, htmlElementIdx = 83, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 117, dtnSiblingNum = 120, dtnLastChildNum = 0  }),

                        (123,DoctreeNode       {uniqueNum = 123, htmlElementIdx = 83, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 117, dtnSiblingNum = 122, dtnLastChildNum = 0  }),


                     (124,DoctreeNode          {uniqueNum = 124, htmlElementIdx = 50, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 75,  dtnSiblingNum = 117, dtnLastChildNum = 135}),
                        (125,DoctreeNode       {uniqueNum = 125, htmlElementIdx = 12, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 124, dtnSiblingNum = 0,   dtnLastChildNum = 0  }),

                        (126,DoctreeNode       {uniqueNum = 126, htmlElementIdx = 62, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 124, dtnSiblingNum = 125, dtnLastChildNum = 127}),
                           (127,DoctreeNode    {uniqueNum = 127, htmlElementIdx = 7,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 126, dtnSiblingNum = 0,   dtnLastChildNum = 0  }),


                        (128,DoctreeNode       {uniqueNum = 128, htmlElementIdx = 62, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 124, dtnSiblingNum = 126, dtnLastChildNum = 129}),
                           (129,DoctreeNode    {uniqueNum = 129, htmlElementIdx = 7,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 128, dtnSiblingNum = 0,   dtnLastChildNum = 0  }),


                        (130,DoctreeNode       {uniqueNum = 130, htmlElementIdx = 83, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 124, dtnSiblingNum = 128, dtnLastChildNum = 0  }),

                        (131,DoctreeNode       {uniqueNum = 131, htmlElementIdx = 83, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 124, dtnSiblingNum = 130, dtnLastChildNum = 0  }),

                        (132,DoctreeNode       {uniqueNum = 132, htmlElementIdx = 7,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 124, dtnSiblingNum = 131, dtnLastChildNum = 0  }),

                        (133,DoctreeNode       {uniqueNum = 133, htmlElementIdx = 43, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 124, dtnSiblingNum = 132, dtnLastChildNum = 0  }),

                        (134,DoctreeNode       {uniqueNum = 134, htmlElementIdx = 83, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 124, dtnSiblingNum = 133, dtnLastChildNum = 0  }),

                        (135,DoctreeNode       {uniqueNum = 135, htmlElementIdx = 43, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 124, dtnSiblingNum = 134, dtnLastChildNum = 0  }),


                     (136,DoctreeNode          {uniqueNum = 136, htmlElementIdx = 50, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 75,  dtnSiblingNum = 124, dtnLastChildNum = 139}),
                        (137,DoctreeNode       {uniqueNum = 137, htmlElementIdx = 43, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 136, dtnSiblingNum = 0,   dtnLastChildNum = 0  }),

                        (138,DoctreeNode       {uniqueNum = 138, htmlElementIdx = 12, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 136, dtnSiblingNum = 137, dtnLastChildNum = 0  }),

                        (139,DoctreeNode       {uniqueNum = 139, htmlElementIdx = 62, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 136, dtnSiblingNum = 138, dtnLastChildNum = 140}),
                           (140,DoctreeNode    {uniqueNum = 140, htmlElementIdx = 7,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 139, dtnSiblingNum = 0,   dtnLastChildNum = 0  }),




                  (141,DoctreeNode             {uniqueNum = 141, htmlElementIdx = 61, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 72,  dtnSiblingNum = 75,  dtnLastChildNum = 143}),
                     (142,DoctreeNode          {uniqueNum = 142, htmlElementIdx = 83, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 141, dtnSiblingNum = 0,   dtnLastChildNum = 0  }),

                     (143,DoctreeNode          {uniqueNum = 143, htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 141, dtnSiblingNum = 142, dtnLastChildNum = 0  }),


                  (144,DoctreeNode             {uniqueNum = 144, htmlElementIdx = 61, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 72,  dtnSiblingNum = 141, dtnLastChildNum = 145}),
                     (145,DoctreeNode          {uniqueNum = 145, htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 144, dtnSiblingNum = 0,   dtnLastChildNum = 0  }),


                  (146,DoctreeNode             {uniqueNum = 146, htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 72,  dtnSiblingNum = 144, dtnLastChildNum = 0  }),





         (147,DoctreeNode                      {uniqueNum = 147, htmlElementIdx = 12, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 8,   dtnSiblingNum = 70,  dtnLastChildNum = 0  }),

         (148,DoctreeNode                      {uniqueNum = 148, htmlElementIdx = 12, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 8,   dtnSiblingNum = 147, dtnLastChildNum = 0  }),

         (149,DoctreeNode                      {uniqueNum = 149, htmlElementIdx = 12, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 8,   dtnSiblingNum = 148, dtnLastChildNum = 0  }),

         (150,DoctreeNode                      {uniqueNum = 150, htmlElementIdx = 77, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 8,   dtnSiblingNum = 149, dtnLastChildNum = 151}),

            (151,DoctreeNode                   {uniqueNum = 151, htmlElementIdx = 82, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 150, dtnSiblingNum = 0,   dtnLastChildNum = 152}),
               (152,DoctreeNode                {uniqueNum = 152, htmlElementIdx = 78, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 151, dtnSiblingNum = 0,   dtnLastChildNum = 159}),
                  (153,DoctreeNode             {uniqueNum = 153, htmlElementIdx = 34, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 152, dtnSiblingNum = 0,   dtnLastChildNum = 154}),
                     (154,DoctreeNode          {uniqueNum = 154, htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 153, dtnSiblingNum = 0,   dtnLastChildNum = 0  }),


                  (155,DoctreeNode             {uniqueNum = 155, htmlElementIdx = 61, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 152, dtnSiblingNum = 153, dtnLastChildNum = 158}),
                     (156,DoctreeNode          {uniqueNum = 156, htmlElementIdx = 43, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 155, dtnSiblingNum = 0,   dtnLastChildNum = 0  }),

                     (157,DoctreeNode          {uniqueNum = 157, htmlElementIdx = 83, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 155, dtnSiblingNum = 156, dtnLastChildNum = 0  }),

                     (158,DoctreeNode          {uniqueNum = 158, htmlElementIdx = 43, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 155, dtnSiblingNum = 157, dtnLastChildNum = 0  }),


                  (159,DoctreeNode             {uniqueNum = 159, htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 152, dtnSiblingNum = 155, dtnLastChildNum = 0  }),





         (160,DoctreeNode                      {uniqueNum = 160, htmlElementIdx = 12, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 8,   dtnSiblingNum = 150, dtnLastChildNum = 0  }),

         (161,DoctreeNode                      {uniqueNum = 161, htmlElementIdx = 12, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 8,   dtnSiblingNum = 160, dtnLastChildNum = 0  }),

         (162,DoctreeNode                      {uniqueNum = 162, htmlElementIdx = 77, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 8,   dtnSiblingNum = 161, dtnLastChildNum = 163}),

            (163,DoctreeNode                   {uniqueNum = 163, htmlElementIdx = 82, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 162, dtnSiblingNum = 0,   dtnLastChildNum = 164}),
               (164,DoctreeNode                {uniqueNum = 164, htmlElementIdx = 78, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 163, dtnSiblingNum = 0,   dtnLastChildNum = 187}),
                  (165,DoctreeNode             {uniqueNum = 165, htmlElementIdx = 34, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 164, dtnSiblingNum = 0,   dtnLastChildNum = 166}),
                     (166,DoctreeNode          {uniqueNum = 166, htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 165, dtnSiblingNum = 0,   dtnLastChildNum = 0  }),


                  (167,DoctreeNode             {uniqueNum = 167, htmlElementIdx = 61, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 164, dtnSiblingNum = 165, dtnLastChildNum = 168}),
                     (168,DoctreeNode          {uniqueNum = 168, htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 167, dtnSiblingNum = 0,   dtnLastChildNum = 0  }),


                  (169,DoctreeNode             {uniqueNum = 169, htmlElementIdx = 61, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 164, dtnSiblingNum = 167, dtnLastChildNum = 170}),
                     (170,DoctreeNode          {uniqueNum = 170, htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 169, dtnSiblingNum = 0,   dtnLastChildNum = 0  }),


                  (171,DoctreeNode             {uniqueNum = 171, htmlElementIdx = 61, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 164, dtnSiblingNum = 169, dtnLastChildNum = 172}),
                     (172,DoctreeNode          {uniqueNum = 172, htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 171, dtnSiblingNum = 0,   dtnLastChildNum = 0  }),


                  (173,DoctreeNode             {uniqueNum = 173, htmlElementIdx = 61, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 164, dtnSiblingNum = 171, dtnLastChildNum = 174}),
                     (174,DoctreeNode          {uniqueNum = 174, htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 173, dtnSiblingNum = 0,   dtnLastChildNum = 0  }),


                  (175,DoctreeNode             {uniqueNum = 175, htmlElementIdx = 61, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 164, dtnSiblingNum = 173, dtnLastChildNum = 0  }),

                  (176,DoctreeNode             {uniqueNum = 176, htmlElementIdx = 61, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 164, dtnSiblingNum = 175, dtnLastChildNum = 177}),
                     (177,DoctreeNode          {uniqueNum = 177, htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 176, dtnSiblingNum = 0,   dtnLastChildNum = 0  }),


                  (178,DoctreeNode             {uniqueNum = 178, htmlElementIdx = 61, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 164, dtnSiblingNum = 176, dtnLastChildNum = 179}),
                     (179,DoctreeNode          {uniqueNum = 179, htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 178, dtnSiblingNum = 0,   dtnLastChildNum = 0  }),


                  (180,DoctreeNode             {uniqueNum = 180, htmlElementIdx = 61, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 164, dtnSiblingNum = 178, dtnLastChildNum = 181}),
                     (181,DoctreeNode          {uniqueNum = 181, htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 180, dtnSiblingNum = 0,   dtnLastChildNum = 0  }),


                  (182,DoctreeNode             {uniqueNum = 182, htmlElementIdx = 61, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 164, dtnSiblingNum = 180, dtnLastChildNum = 183}),
                     (183,DoctreeNode          {uniqueNum = 183, htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 182, dtnSiblingNum = 0,   dtnLastChildNum = 0  }),


                  (184,DoctreeNode             {uniqueNum = 184, htmlElementIdx = 61, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 164, dtnSiblingNum = 182, dtnLastChildNum = 185}),
                     (185,DoctreeNode          {uniqueNum = 185, htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 184, dtnSiblingNum = 0,   dtnLastChildNum = 0  }),


                  (186,DoctreeNode             {uniqueNum = 186, htmlElementIdx = 12, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 164, dtnSiblingNum = 184, dtnLastChildNum = 0  }),

                  (187,DoctreeNode             {uniqueNum = 187, htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 164, dtnSiblingNum = 186, dtnLastChildNum = 0  }),





         (188,DoctreeNode                {uniqueNum = 188, htmlElementIdx = 12, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 8,   dtnSiblingNum = 162, dtnLastChildNum = 0  }),

         (189,DoctreeNode                {uniqueNum = 189, htmlElementIdx = 12, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 8,   dtnSiblingNum = 188, dtnLastChildNum = 0  }),

         (190,DoctreeNode                {uniqueNum = 190, htmlElementIdx = 77, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 8,   dtnSiblingNum = 189, dtnLastChildNum = 197}),

            (191,DoctreeNode                   {uniqueNum = 191, htmlElementIdx = 82, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 190, dtnSiblingNum = 0,   dtnLastChildNum = 192}),
               (192,DoctreeNode                {uniqueNum = 192, htmlElementIdx = 78, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 191, dtnSiblingNum = 0,   dtnLastChildNum = 195}),
                  (193,DoctreeNode             {uniqueNum = 193, htmlElementIdx = 34, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 192, dtnSiblingNum = 0,   dtnLastChildNum = 194}),
                     (194,DoctreeNode          {uniqueNum = 194, htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 193, dtnSiblingNum = 0,   dtnLastChildNum = 0  }),


                  (195,DoctreeNode             {uniqueNum = 195, htmlElementIdx = 85, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 192, dtnSiblingNum = 193, dtnLastChildNum = 196}),
                     (196,DoctreeNode          {uniqueNum = 196, htmlElementIdx = 50, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 195, dtnSiblingNum = 0,   dtnLastChildNum = 0  }),




            (197,DoctreeNode                   {uniqueNum = 197, htmlElementIdx = 82, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 190, dtnSiblingNum = 191, dtnLastChildNum = 198}),
               (198,DoctreeNode                {uniqueNum = 198, htmlElementIdx = 78, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 197, dtnSiblingNum = 0,   dtnLastChildNum = 201}),
                  (199,DoctreeNode             {uniqueNum = 199, htmlElementIdx = 34, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 198, dtnSiblingNum = 0,   dtnLastChildNum = 200}),
                     (200,DoctreeNode          {uniqueNum = 200, htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 199, dtnSiblingNum = 0,   dtnLastChildNum = 0  }),


                  (201,DoctreeNode             {uniqueNum = 201, htmlElementIdx = 61, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 198, dtnSiblingNum = 199, dtnLastChildNum = 0  }),





         (202,DoctreeNode                      {uniqueNum = 202, htmlElementIdx = 77, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 8,   dtnSiblingNum = 190, dtnLastChildNum = 203}),
            (203,DoctreeNode                   {uniqueNum = 203, htmlElementIdx = 82, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 202, dtnSiblingNum = 0,   dtnLastChildNum = 204}),
               (204,DoctreeNode                {uniqueNum = 204, htmlElementIdx = 78, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 203, dtnSiblingNum = 0,   dtnLastChildNum = 208}),
                  (205,DoctreeNode             {uniqueNum = 205, htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 204, dtnSiblingNum = 0,   dtnLastChildNum = 0  }),


                  (206,DoctreeNode             {uniqueNum = 206, htmlElementIdx = 12, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 204, dtnSiblingNum = 205, dtnLastChildNum = 0  }),

                  (207,DoctreeNode             {uniqueNum = 207, htmlElementIdx = 0,  selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 204, dtnSiblingNum = 206, dtnLastChildNum = 0  }),

                  (208,DoctreeNode             {uniqueNum = 208, htmlElementIdx = 61, selPseudoClass = [], selId = "", selClass = [], dtnParentNum = 204, dtnSiblingNum = 207, dtnLastChildNum = 0  })
   ]}




runDoctreeOps :: Doctree -> [(Bool, Int)] -> IO Doctree
runDoctreeOps doctree (x:xs) = do
  if fst x
    then do
    let newDoctree = doctreePushNode doctree (snd x)
    runDoctreeOps newDoctree xs
    else do
    let newDoctree = doctreePopNode doctree
    runDoctreeOps newDoctree xs
runDoctreeOps doctree []     = return doctree




{- -------------------------------------------------------------------------- -}




testCases :: [Test]
testCases = [
  TestCase (do result <- runDoctreeOps (defaultDoctree {rootNode = 0}) ops
               assertEqual "doctrees aren't equal" expectedDoctree result)
  ]




testsHtmlDoctree :: IO String
testsHtmlDoctree = do
  testCounts <- runTestTT (TestList testCases)
  if errors testCounts + failures testCounts == 0
    then return ""
    else return "[EE] Hello.Tests.Html.Doctree failed"


