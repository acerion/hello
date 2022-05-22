{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-} -- For specifying expected integer values of CssValueTypeMultiEnum.




module Hello.Tests.Css.DeclarationSet
  (
    testsCssDeclarationSet
  )
where




import qualified Data.Text as T
import qualified Data.Sequence as S

import Test.HUnit
import Debug.Trace

import Hello.Css.Tokenizer
import Hello.Css.Parser
import Hello.Utils




myFromList = S.fromList




{- -------------------------------------------------------------------------- -}




declarationsSetAppendData = [
  -- Two disjoint sequences.
  -- target:
  ( CssDeclarationSet { isSafe = True
                      , items  = myFromList
                        [
                          CssDeclaration { property = 1, declValue = CssValueTypeColor 11, important = True  }
                        , CssDeclaration { property = 3, declValue = CssValueTypeColor 33, important = False }
                        , CssDeclaration { property = 4, declValue = CssValueTypeColor 44, important = True  }
                        ]
                      }
  -- incoming:
  , CssDeclarationSet { isSafe = True
                      , items  = myFromList
                        [
                          CssDeclaration { property = 7, declValue = CssValueTypeColor 77, important = False }
                        , CssDeclaration { property = 8, declValue = CssValueTypeColor 88, important = True  }
                        , CssDeclaration { property = 9, declValue = CssValueTypeColor 99, important = False }
                        ]
                      }

  -- merged:
  , CssDeclarationSet { isSafe = True
                      , items  = myFromList
                        [
                          CssDeclaration { property = 1, declValue = CssValueTypeColor 11, important = True  }
                        , CssDeclaration { property = 3, declValue = CssValueTypeColor 33, important = False }
                        , CssDeclaration { property = 4, declValue = CssValueTypeColor 44, important = True  }
                        , CssDeclaration { property = 7, declValue = CssValueTypeColor 77, important = False }
                        , CssDeclaration { property = 8, declValue = CssValueTypeColor 88, important = True  }
                        , CssDeclaration { property = 9, declValue = CssValueTypeColor 99, important = False }
                        ]
                      }
  )
  ,

  -- Two sequences where the second one contains a property existing in first one (with the same value of 'property' field).
  -- target:
  ( CssDeclarationSet { isSafe = True
                      , items  = myFromList
                        [
                          CssDeclaration { property = 1, declValue = CssValueTypeColor 12, important = True  }
                        , CssDeclaration { property = 3, declValue = CssValueTypeColor 23, important = False }   -- <---- this entry will be replaced/updated...
                        , CssDeclaration { property = 4, declValue = CssValueTypeColor 34, important = True  }
                        ]
                      }
  -- incoming:
  , CssDeclarationSet { isSafe = True
                      , items  = myFromList
                        [
                          CssDeclaration { property = 7, declValue = CssValueTypeColor 45, important = False }
                        , CssDeclaration { property = 3, declValue = CssValueTypeColor 56, important = True  }   -- <---- ... with this one.
                        , CssDeclaration { property = 9, declValue = CssValueTypeColor 67, important = False }
                        ]
                      }
  -- merged:
  , CssDeclarationSet { isSafe = True,
                        items  = myFromList
                        [
                          CssDeclaration { property = 1, declValue = CssValueTypeColor 12, important = True  }
                        , CssDeclaration { property = 3, declValue = CssValueTypeColor 56, important = True  }   -- <---- And here is the result of updating.
                        , CssDeclaration { property = 4, declValue = CssValueTypeColor 34, important = True  }
                        , CssDeclaration { property = 7, declValue = CssValueTypeColor 45, important = False }
                        -- , CssDeclaration { property = 3, declValue = CssValueTypeColor 56, important = True  }
                        , CssDeclaration { property = 9, declValue = CssValueTypeColor 67, important = False }
                        ]
                      }
  )
  ]





-- On success return empty string. On failure return string representation of
-- remainder string in a row, for which test failed.
declarationsSetAppendTest :: [(CssDeclarationSet, CssDeclarationSet, CssDeclarationSet)] -> T.Text
declarationsSetAppendTest []     = ""
declarationsSetAppendTest (x:xs) = if expectedMerged /= merged
                                   then (T.pack . show $ expectedMerged)
                                   else declarationsSetAppendTest xs
  where
    target         = triplet1st x
    incoming       = triplet2nd x
    expectedMerged = triplet3rd x
    merged         = declarationsSetAppend target incoming




{- -------------------------------------------------------------------------- -}




testCases = [
  -- If some error is found, test function returns some data (e.g. non-empty
  -- string or test index) which can help identify which test failed.
     TestCase (do
                 assertEqual "manual tests of declarationsSetAppend" "" (declarationsSetAppendTest declarationsSetAppendData))
  ]




testsCssDeclarationSet :: IO String
testsCssDeclarationSet = do
  counts <- runTestTT (TestList (testCases))
  if (errors counts + failures counts == 0)
    then return ""
    else return "[EE] Tests.Css.DeclarationSet failed"

