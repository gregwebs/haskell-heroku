module Web.Heroku ( 
    dbConnParams 
  , parseDatabaseUrl
  ) where

import System.Environment
import Network.URI
import Data.Text
import Prelude
import Web.Heroku.Postgres (dbConnParams, parseDatabaseUrl)
