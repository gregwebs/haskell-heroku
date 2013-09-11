module Web.Heroku.MongoDB ( 
    mongoLabConnParams
  , mongoHQConnParams
  , parseDatabaseUrl
  ) where

import Data.Text
import Web.Heroku.Internal (dbConnParams', parseDatabaseUrl')

mongoLabConnParams :: IO [(Text, Text)]
mongoLabConnParams = dbConnParams' "MONGOLAB_URI" parseDatabaseUrl

mongoHQConnParams :: IO [(Text, Text)]
mongoHQConnParams = dbConnParams' "MONGOHQ_URL" parseDatabaseUrl

parseDatabaseUrl :: String -> [(Text, Text)]
parseDatabaseUrl = parseDatabaseUrl' "mongodb:"
