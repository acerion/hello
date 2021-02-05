{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Cookies(lookupActionForDomain,
              ) where

import Prelude
import Foreign.C.String
import Foreign
import System.Directory
import System.IO
import Control.Monad -- when
import Data.List -- sortBy
import Control.Exception (evaluate)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO




{-
See doc/cookies.md for more information.

Right now every action related to lookup of domain/action triggers re-reading
the cookiesrc file and processing its contents. This way I don't have to use
global variable to store the cookies config, but in the long run this will be
too ineffective. TODO: consider putting cookies config in global variable.

TODO: Ensure that case of action strings and domains is handled properly,
i.e. that toLower is called at proper places.
-}




foreign export ccall "hll_lookupActionForDomain" hll_lookupActionForDomain :: CString -> IO Int


hll_lookupActionForDomain :: CString -> IO Int
hll_lookupActionForDomain dom = do
  domainString <- peekCString dom
  let domain = T.pack domainString
  cookiesConfig <- getCookiesConfig
  -- putStr ("hello: cookies: " ++ (show cookiesConfig) ++ "\n") -- For debug only.
  let action = lookupActionForDomain domain (rules cookiesConfig) (defaultAction cookiesConfig)
  T.IO.putStr (T.concat ["hello: cookies: ", domain, " = ", T.pack (show action), "\n"])
  case action of
    -- Convert to values of CookieAction enum in C file.
    CookieActionAccept        -> return 0
    CookieActionAcceptSession -> return 1
    CookieActionDeny          -> return 2



data CookieAction = CookieActionAccept
                  | CookieActionAcceptSession
                  | CookieActionDeny
                  deriving (Show, Eq)



data CookieRule = CookieRule {
    domain :: T.Text -- always lower-case
  , action :: CookieAction
  } deriving (Eq)

instance Show CookieRule where
  show rule = show (domain rule) ++ "\t" ++ show (action rule) ++ "\n"



data CookiesConfig = CookiesConfig {
  rules :: [CookieRule],
  defaultAction :: CookieAction, -- default action for domains not explicitly listed
  cookiesEnabled :: Bool -- is any action Accept or AcceptSession?
  }

instance Show CookiesConfig where
  show c =
    "hello: cookies: Cookies enabled = " ++ show (cookiesEnabled c) ++ "\n"
    ++ "hello: cookies: Default action = " ++ show (defaultAction c) ++ "\n"
    ++ "\n"
    ++ "hello: cookies: Rules =\n" ++ show (rules c) ++ "\n"


{-
main = do
  lookupActionTop "dillo.org"
-}


{-
Convert contents of config file, split into lines, into set of rules.

TEST: parseLines (lines "en.wikipedia.org ACCEPT\npolandl.wikipedia.org DENY\ndillo.org ACCEPT\nen.wikipedia.org BAD_RULE\nfr.wikipedia.org ACCEPT THIRD")
-}
parseLines :: [T.Text] -> [CookieRule]
parseLines []     = []
parseLines (x:xs) =
  case (parseLine x) of
    Just rule -> rule : (parseLines xs)
    Nothing   -> (parseLines xs)



{-
Convert single line from config line into single rule.

TEST: parseLine "# commented line"
-}
parseLine :: T.Text -> Maybe CookieRule
parseLine line =
  if (Data.List.length tokens /= 2) || ((T.head line) == '#') -- TODO: ensure that line is non-empty, otherwise head will fail
  then Nothing -- If there is a valid domain, but overall the line is
               -- malformed, global default action will be used for that
               -- domain.
  else Just CookieRule { domain = T.toLower (tokens !! 0),
                         action = stringToAction (tokens !! 1)
                       }
  where tokens = (T.words line)



{-
Convert string representation of action into action enum.
-}
stringToAction :: T.Text -> CookieAction
stringToAction str =
  case T.toLower str of
    "accept"         -> CookieActionAccept
    "accept_session" -> CookieActionAcceptSession
    "deny"           -> CookieActionDeny
    _                -> CookieActionDeny -- Malformed action string



{-
Put list of rules into config. Get additional properties from the list and
put it in the config too.
-}
rulesToConfig :: [CookieRule] -> CookiesConfig
rulesToConfig inputRules =
  CookiesConfig { rules          = sortRules (Data.List.filter isNotDefaultRule inputRules),
                  defaultAction  = findDefaultAction inputRules,
                  cookiesEnabled = Data.List.length (Data.List.filter isNotDenyRule inputRules) /= 0
                }



{-
Sort rules by length of domain.  The rules should be ordered by domain
length, with longest first, so the first match is the most specific.

TODO: consider replacing sorting of the list with inserting new rule at
specific place (at place that ensures being sorted by domain length).
-}
sortRules :: [CookieRule] -> [CookieRule]
sortRules rules = sortBy domainLengths rules
  where domainLengths a b = compare (T.length (domain b)) (T.length (domain a))



{-
Find "default" rule on list of rules, return action for that rule. If not
found, return program's hardcoded global default action.
-}
findDefaultAction :: [CookieRule] -> CookieAction
findDefaultAction []     = CookieActionDeny -- hardcoded default action
findDefaultAction (x:xs) = if isDefaultRule x
                           then (action x)
                           else findDefaultAction xs



isDefaultRule :: CookieRule -> Bool
isDefaultRule rule = domain rule == "default"

isNotDefaultRule :: CookieRule -> Bool
isNotDefaultRule x = not (isDefaultRule x)

isNotDenyRule :: CookieRule -> Bool
isNotDenyRule rule = action rule /= CookieActionDeny




{-
Lookup action for given domain in list of cookie rules. Return given
default action if domain is not found.

TODO: original code used dStrAsciiCasecmp() function that was commented with
"ASCII functions to avoid the case difficulties introduced by I/i in Turkic
locales.". Test behaviour of the code for this case.

TODO: Check example host ".host.com" in doc/cookies.md: it starts with a
dot. Right now this code does not support such domain names. Look at original
code (Cookies_control_check_domain()) to see how it was done, and implement
it here.
-}
lookupActionForDomain :: T.Text -> [CookieRule] -> CookieAction -> CookieAction
lookupActionForDomain dom rules def = lookupCS (T.toLower dom) rules def
  where lookupCS _ [] defaultAction            = defaultAction
        lookupCS domainLC (x:xs) defaultAction = if (domainLC == (domain x))
                                                 then (action x)
                                                 else lookupCS dom xs defaultAction



{-
Top level function for opening config file, getting contents, looking up
action and printing debugs.
-}
lookupActionTop :: T.Text -> IO ()
lookupActionTop domain = do
   -- The config file may be empty, which will produce config with only
   -- default rule and default action.
  cookiesConfig <- getCookiesConfig
  putStr ("hello: cookies: " ++ (show cookiesConfig) ++ "\n") -- For debug only.

  let action = lookupActionForDomain domain (rules cookiesConfig) (defaultAction cookiesConfig)
  T.IO.putStr (T.concat [domain, " = ", T.pack (show action), "\n"])



{-
Get configuration of cookies.  Read config file for cookies, convert it to
CookiesConfig, return the config.  If the file is empty or can't be read,
return config with basic, default set of options.
-}
getCookiesConfig :: IO CookiesConfig
getCookiesConfig = do
  let progName = "hello" -- TODO set to program name
  contents <- getFileContents progName
  let cookiesConfig = rulesToConfig (parseLines (T.lines contents))
  return cookiesConfig

  where
    getFileContents progName = do
      let fileName = "cookiesrc"
      (configDir, fileName) <- getConfigLocation progName fileName
      exists2 <- doesDirectoryExist configDir
      case exists2 of
        False -> return "" -- empty contents of config file
        True  -> do
          let fullPath = configDir ++ "/" ++ fileName
          -- let fullPath = "/home/kamil/dummy_test_nonexistent"
          handle    <- getFileHandle fullPath
          contents  <- T.IO.hGetContents handle
          evaluate (T.length contents) -- ensure that all contents is read before closing handle
          hClose handle
          return contents



{-
Return tuple: dir with cookies rc file + the cookies file name
-}
getConfigLocation :: String -> String -> IO (String, String)
getConfigLocation progName fileName = do
  configDir <- (getXdgDirectory XdgConfig progName)
  createDirectoryIfMissing True configDir
  return (configDir, fileName)



{-
Get handle to config file. If file did not exist, create it and pre-fill with
default rules.
-}
getFileHandle :: String -> IO Handle
getFileHandle fullPath = do
  let headerString = "# domain action\n" :: String
  let defaultRule = "DEFAULT DENY\n" :: String
  -- let defaultRule = "en.wikipedia.org ACCEPT\npoland.wikipedia.org DENY\ndillo.org ACCEPT\n\ndillo.com ACCEPT_SESSION\n"
  exists <- doesFileExist fullPath
  when (not exists) $ writeFile fullPath (headerString ++ defaultRule)
  openFile fullPath ReadMode


