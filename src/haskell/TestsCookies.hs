{-# LANGUAGE OverloadedStrings #-}


module Main (main) where

import Test.HUnit
import System.Exit

import Cookies





lineToRuleCases = [

  -- Empty line.
    TestCase(assertEqual "parse empty line"    Nothing (Cookies.lineToRule ""))

  -- Comments.
  , TestCase(assertEqual "parse empty comment" Nothing (Cookies.lineToRule "#"))
  , TestCase(assertEqual "parse comment"       Nothing (Cookies.lineToRule "# Something"))

  -- The second case tests url = "one" with malformed action = "two"
  , TestCase(assertEqual "parse one word"    Nothing (Cookies.lineToRule "one"))
  , TestCase(assertEqual "parse two words"   Nothing (Cookies.lineToRule "one two"))
  , TestCase(assertEqual "parse three words" Nothing (Cookies.lineToRule "one two three"))

  -- Test conversion of action string to lowercase.
  , TestCase(assertEqual "parse DENY" (Just Cookies.CookieRule { domain = "url", action = CookieActionDeny }) (Cookies.lineToRule "url DENY"))
  , TestCase(assertEqual "parse Deny" (Just Cookies.CookieRule { domain = "url", action = CookieActionDeny }) (Cookies.lineToRule "url Deny"))
  , TestCase(assertEqual "parse deny" (Just Cookies.CookieRule { domain = "url", action = CookieActionDeny }) (Cookies.lineToRule "url deny"))
  , TestCase(assertEqual "parse deNY" (Just Cookies.CookieRule { domain = "url", action = CookieActionDeny }) (Cookies.lineToRule "url deNY"))

  -- Tests conversion of domain string to lowercase.
  , TestCase(assertEqual "to lowercase 1"
             (Just Cookies.CookieRule { domain = "default", action = CookieActionDeny })          (Cookies.lineToRule "DEFAULT DENY"))
  , TestCase(assertEqual "to lowercase 2"
             (Just Cookies.CookieRule { domain = "default", action = CookieActionAccept })        (Cookies.lineToRule "Default ACCEPT"))
  , TestCase(assertEqual "to lowercase 3"
             (Just Cookies.CookieRule { domain = ".some-2.wi-ld.domain5.com", action = CookieActionAcceptSession }) (Cookies.lineToRule ".Some-2.Wi-LD.DOmAIN5.com ACCEPT_SESSION"))
  , TestCase(assertEqual "to lowercase 4"
             (Just Cookies.CookieRule { domain = "another.case-domain.com", action = CookieActionAcceptSession }) (Cookies.lineToRule "ANOTHER.CASE-DOMAIN.com ACCEPT_SESSION"))

  -- Malformed action should lead to rejecting a line.
  , TestCase(assertEqual "parse malformed action 1" Nothing (Cookies.lineToRule "url ACCEP"))
  , TestCase(assertEqual "parse malformed action 2" Nothing (Cookies.lineToRule "default ACCEP"))

  -- Tests of "default" domain.
  , TestCase(assertEqual "parse default deny"
             (Just Cookies.CookieRule { domain = "default", action = CookieActionDeny })          (Cookies.lineToRule "default DENY"))
  , TestCase(assertEqual "parse default accept"
             (Just Cookies.CookieRule { domain = "default", action = CookieActionAccept })        (Cookies.lineToRule "default ACCEPT"))
  , TestCase(assertEqual "parse default accept session"
             (Just Cookies.CookieRule { domain = "default", action = CookieActionAcceptSession }) (Cookies.lineToRule "default ACCEPT_SESSION"))
  ]



sortRulesCases = [

  -- Empty list of rules.
  TestCase(assertEqual ""
           (          [])
           (sortRules [])
          )

  -- Single rule.
  , TestCase(assertEqual ""
             (          [ Cookies.CookieRule { domain = "url.com", action = CookieActionDeny }])
             (sortRules [ Cookies.CookieRule { domain = "url.com", action = CookieActionDeny }])
            )

  -- Two sorted rules.
  , TestCase(assertEqual "two sorted rules"
             (          [ Cookies.CookieRule { domain = "longer.com", action = CookieActionDeny },
                          Cookies.CookieRule { domain = "url.com", action = CookieActionDeny }])
             (sortRules [ Cookies.CookieRule { domain = "longer.com", action = CookieActionDeny },
                          Cookies.CookieRule { domain = "url.com", action = CookieActionDeny }])
            )

  -- Two unsorted rules.
  , TestCase(assertEqual "two unsorted rules"
             (          [ Cookies.CookieRule { domain = "longer.com", action = CookieActionDeny },
                          Cookies.CookieRule { domain = "url.com", action = CookieActionDeny }])
             (sortRules [ Cookies.CookieRule { domain = "url.com", action = CookieActionDeny },
                          Cookies.CookieRule { domain = "longer.com", action = CookieActionDeny }])
            )

  -- Three rules.
  , TestCase(assertEqual "three rules"
             (          [ Cookies.CookieRule { domain = "thelongest.com", action = CookieActionDeny },
                          Cookies.CookieRule { domain = "longer.com", action = CookieActionDeny },
                          Cookies.CookieRule { domain = "url.com", action = CookieActionDeny }])
             (sortRules [ Cookies.CookieRule { domain = "url.com", action = CookieActionDeny },
                          Cookies.CookieRule { domain = "thelongest.com", action = CookieActionDeny },
                          Cookies.CookieRule { domain = "longer.com", action = CookieActionDeny }])
            )
  ]



main :: IO ()
main = do
  counts <- runTestTT (TestList (lineToRuleCases ++ sortRulesCases))
  if (errors counts + failures counts == 0)
    then exitSuccess
    else exitFailure
