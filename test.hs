module Main where

import Web.Heroku

main = print $ show $
  parseDatabaseUrl "postgres://db:pass@ec2-1-1-1-1.compute-1.amazonaws.com/db"
