module Web.Heroku ( dbConnParams ) where

import System.Environment
import Network.URI
import Data.Text

-- | read the DATABASE_URL environment variable
-- and return an alist of connection parameters with the following keys:
-- user, password, host, port, dbname
--
-- warning: just calls error if it can't parse correctly
dbConnParams :: IO [(Text, Text)]
dbConnParams = do
  durl <- getEnv "DATABASE_URL"
  let muri = parseAbsoluteURI durl
  let (auth, path) = case muri of
                      Nothing ->  invalid
                      Just uri -> if uriScheme uri /= "postgres" then invalid
                                    else case uriAuthority uri of
                                           Nothing   -> invalid
                                           Just a -> (a, uriPath uri)
  let (user,password) = userAndPassword auth
  return [
          (pack "user",     user)
         ,(pack "password", password)
         ,(pack "host",     pack $ uriRegName auth)
         ,(pack "port",     pack $ uriPort auth)
         ,(pack "dbname",   pack $ path)
         ]
  where
    -- userAndPassword :: UriAuth -> String
    userAndPassword = (breakOn $ pack ":") . pack . uriUserInfo

    -- should be an error 
    invalid = error "could not parse heroku DATABASE_URL"
