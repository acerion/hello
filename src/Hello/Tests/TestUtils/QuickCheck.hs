{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




module Hello.Tests.Utils.QuickCheck
  (
    qcResultIsSuccess
  )
where




import Test.QuickCheck




-- Simple binary answer to question of whether Result variable is a success
-- or something else.
qcResultIsSuccess :: Result -> Bool
qcResultIsSuccess result =
  case result of
    Success _ _ _ _ _ _ -> True
    _                   -> False
