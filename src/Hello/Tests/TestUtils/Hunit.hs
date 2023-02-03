{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}




module Hello.Tests.Utils.Hunit
  (
    assertSuccess
  )
where



import qualified Data.Text as T
import Control.Monad
import Test.HUnit



assertSuccess :: String -> [T.Text] -> Assertion
assertSuccess description result =
  unless (null result) (assertFailure msg)
  where msg = description ++ "\n" ++
              "Details of failed test:\n" ++ T.unpack (T.intercalate "\n" result) ++ "\n"

