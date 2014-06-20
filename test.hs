{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Heroku
import Test.Hspec

main :: IO ()
main = hspec $
  describe "parseDatabaseUrl" $
    it "extracts individual items" $
      parseDatabaseUrl "postgres://db:pass@ec2-1-1-1-1.compute-1.amazonaws.com:1234/db" `shouldBe`
        [ ("user","db")
        , ("password","pass")
        , ("host","ec2-1-1-1-1.compute-1.amazonaws.com")
        , ("port","1234")
        , ("dbname","db")
        ]
