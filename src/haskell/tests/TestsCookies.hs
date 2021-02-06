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



-- Rules are sorted by length of domain (from longest to shortest). To detect
-- error where in production code we would compare by string contents, make
-- sure that all domains start with the same letter. This doesn't prevent all
-- errors because even bad code that compares "aaa.com" with "aaaaaa.com"
-- will recognize that '.' /= 'a', but maybe it will at least help somewhat.
sortRulesCases = [

  -- Empty list of rules.
  TestCase(assertEqual "sort empty"
           (          [])
           (sortRules [])
          )

  -- Single rule.
  , TestCase(assertEqual "sort single rule"
             (          [ Cookies.CookieRule { domain = "aaa.com", action = CookieActionDeny }])
             (sortRules [ Cookies.CookieRule { domain = "aaa.com", action = CookieActionDeny }])
            )

  -- Two sorted rules.
  , TestCase(assertEqual "two sorted rules"
             (          [ Cookies.CookieRule { domain = "aaaaaa.com", action = CookieActionDeny },
                          Cookies.CookieRule { domain = "aaa.com", action = CookieActionDeny }])
             (sortRules [ Cookies.CookieRule { domain = "aaaaaa.com", action = CookieActionDeny },
                          Cookies.CookieRule { domain = "aaa.com", action = CookieActionDeny }])
            )

  , TestCase(assertEqual "two unsorted rules"
             (          [ Cookies.CookieRule { domain = "aaaaaa.com", action = CookieActionDeny },
                          Cookies.CookieRule { domain = "aaa.com", action = CookieActionDeny }])
             (sortRules [ Cookies.CookieRule { domain = "aaa.com", action = CookieActionDeny },
                          Cookies.CookieRule { domain = "aaaaaa.com", action = CookieActionDeny }])
            )

  , TestCase(assertEqual "three rules"
             (          [ Cookies.CookieRule { domain = "aaaaaaaaa.com", action = CookieActionDeny },
                          Cookies.CookieRule { domain = "aaaaaa.com", action = CookieActionDeny },
                          Cookies.CookieRule { domain = "aaa.com", action = CookieActionDeny }])
             (sortRules [ Cookies.CookieRule { domain = "aaa.com", action = CookieActionDeny },
                          Cookies.CookieRule { domain = "aaaaaaaaa.com", action = CookieActionDeny },
                          Cookies.CookieRule { domain = "aaaaaa.com", action = CookieActionDeny }])
            )
  ]



main :: IO ()
main = do
  counts <- runTestTT (TestList (lineToRuleCases ++ sortRulesCases))
  if (errors counts + failures counts == 0)
    then exitSuccess
    else exitFailure
