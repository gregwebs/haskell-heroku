module Web.Heroku.Postgres ( 
    dbConnParams 
  , parseDatabaseUrl
  ) where

import Data.Text
import Web.Heroku.Internal (dbConnParams', parseDatabaseUrl')

dbConnParams :: IO [(Text, Text)]
dbConnParams = dbConnParams' "DATABASE_URL" parseDatabaseUrl

parseDatabaseUrl :: String -> [(Text, Text)]
parseDatabaseUrl = parseDatabaseUrl' "postgres:"
