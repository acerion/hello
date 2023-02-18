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
--import Debug.Trace

import Hello.Css.Declaration
import Hello.Css.Distance
import Hello.Css.Parser.Property

import Hello.Utils




myFromList :: [a] -> S.Seq a
myFromList = S.fromList




{- -------------------------------------------------------------------------- -}



declarationsSetAppendData :: [(CssDeclarationSet, CssDeclarationSet, CssDeclarationSet)]
declarationsSetAppendData = [
  -- Two disjoint sequences.
  -- target:
  ( CssDeclarationSet { isSafe = True
                      , items  = myFromList
                        [
                          CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 11), important = True  }
                        , CssDeclaration { property = CssPropertyBackgroundPosition (CssValueBackgroundPositionXY 0 0), important = False }
                        , CssDeclaration { property = CssPropertyBackgroundRepeat CssValueBackgroundRepeatNoRepeat, important = True  }
                        ]
                      }
  -- incoming:
  , CssDeclarationSet { isSafe = True
                      , items  = myFromList
                        [
                          CssDeclaration { property = CssPropertyBorderBottomWidth (CssValueBorderWidthDistance (CssDistanceAbsPx 1.0)), important = False }
                        , CssDeclaration { property = CssPropertyBorderCollapse CssValueBorderCollapseSeparate, important = True  }
                        , CssDeclaration { property = CssPropertyBorderLeftColor (CssValueBorderColor 99), important = False }
                        ]
                      }

  -- merged:
  , CssDeclarationSet { isSafe = True
                      , items  = myFromList
                        [
                          CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 11), important = True  }
                        , CssDeclaration { property = CssPropertyBackgroundPosition (CssValueBackgroundPositionXY 0 0), important = False }
                        , CssDeclaration { property = CssPropertyBackgroundRepeat CssValueBackgroundRepeatNoRepeat, important = True  }
                        , CssDeclaration { property = CssPropertyBorderBottomWidth (CssValueBorderWidthDistance (CssDistanceAbsPx 1.0)), important = False }
                        , CssDeclaration { property = CssPropertyBorderCollapse CssValueBorderCollapseSeparate, important = True  }
                        , CssDeclaration { property = CssPropertyBorderLeftColor (CssValueBorderColor 99), important = False }
                        ]
                      }
  )
  ,

  -- Two sequences where the second one contains a property existing in first one (with the same type of 'property' field).
  -- target:
  ( CssDeclarationSet { isSafe = True
                      , items  = myFromList
                        [
                          CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 12), important = True  }
                        , CssDeclaration { property = CssPropertyBackgroundPosition (CssValueBackgroundPositionXY 0 0), important = False }   -- <---- this entry will be replaced/updated...
                        , CssDeclaration { property = CssPropertyBackgroundRepeat CssValueBackgroundRepeatRepeatX, important = True  }
                        ]
                      }
  -- incoming:
  , CssDeclarationSet { isSafe = True
                      , items  = myFromList
                        [
                          CssDeclaration { property = CssPropertyBorderBottomWidth (CssValueBorderWidthDistance (CssDistanceAbsPx 1.0)), important = False }
                        , CssDeclaration { property = CssPropertyBackgroundPosition (CssValueBackgroundPositionXY 11 12), important = True  }   -- <---- ... with this one.
                        , CssDeclaration { property = CssPropertyBorderLeftColor (CssValueBorderColor 67), important = False }
                        ]
                      }
  -- merged:
  , CssDeclarationSet { isSafe = True,
                        items  = myFromList
                        [
                          CssDeclaration { property = CssPropertyBackgroundColor (CssValueBackgroundColorColor 12), important = True  }
                        , CssDeclaration { property = CssPropertyBackgroundPosition (CssValueBackgroundPositionXY 11 12), important = True  }   -- <---- And here is the result of updating.
                        , CssDeclaration { property = CssPropertyBackgroundRepeat CssValueBackgroundRepeatRepeatX, important = True  }
                        , CssDeclaration { property = CssPropertyBorderBottomWidth (CssValueBorderWidthDistance (CssDistanceAbsPx 1.0)), important = False }
                        , CssDeclaration { property = CssPropertyBorderLeftColor (CssValueBorderColor 67), important = False }
                        ]
                      }
  )
  ]





-- On success return empty string. On failure return string representation of
-- remainder string in a row, for which test failed.
declarationsSetAppendTest :: [(CssDeclarationSet, CssDeclarationSet, CssDeclarationSet)] -> T.Text
declarationsSetAppendTest []     = ""
declarationsSetAppendTest (x:xs) = if expectedMerged /= merged
                                   then T.pack . show $ expectedMerged
                                   else declarationsSetAppendTest xs
  where
    target         = triplet1st x
    incoming       = triplet2nd x
    expectedMerged = triplet3rd x
    merged         = declarationsSetAppend target incoming




{- -------------------------------------------------------------------------- -}




testCases :: [Test]
testCases = [
  -- If some error is found, test function returns some data (e.g. non-empty
  -- string or test index) which can help identify which test failed.
     TestCase (do
                 assertEqual "manual tests of declarationsSetAppend" "" (declarationsSetAppendTest declarationsSetAppendData))
  ]




testsCssDeclarationSet :: IO String
testsCssDeclarationSet = do
  testCounts <- runTestTT (TestList testCases)
  if errors testCounts + failures testCounts == 0
    then return ""
    else return "[EE] Tests.Css.DeclarationSet failed"

