module Web.Heroku ( dbConnParams ) where

import System.Environment
import Network.URI
import Data.Text

-- | read the DATABASE_URL environment variable
-- and return an alist of connection parameters with the following keys:
-- user, password, host, port, dbname
dbConnParams :: IO [(Text, Text)]
dbConnParams = do
  durl <- getEnv "DATABASE_URL"
  let muri = parseAbsoluteURI durl
  auth = case muri of
            Nothing ->  invalid
            Just uri -> if uriScheme uri /= "postgres" then invalid
                          else case uriAuthority uri of
                                  Nothing   -> invalid
                                  Just a -> a
  let (user,password) = userAndPassword auth
  return [
          ("user",     pack user),
          ("password", pack password),
          ("host",     pack $ uriRegName auth),
          ("port",     pack $ uriPort auth)
          ("dbname",   pack $ uriPath uri)
         ]
  where
    userAndPassword :: UriAuth -> String
    userAndPassword = split ":" uriUserInfo 

    -- should be an error 
    invalid = error "could not parse heroku DATABASE_URL"
