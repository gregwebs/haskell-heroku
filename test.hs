{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Web.Heroku
import Web.Heroku.RabbitMQ

main :: IO ()
main = hspec $ do
  describe "parseDatabaseUrl" $
    it "extracts individual items" $
      parseDatabaseUrl "postgres://db:pass@ec2-1-1-1-1.compute-1.amazonaws.com:1234/db" `shouldBe`
        [ ("user","db")
        , ("password","pass")
        , ("host","ec2-1-1-1-1.compute-1.amazonaws.com")
        , ("port","1234")
        , ("dbname","db")
        ]
  describe "parseAmqpUrl" $
    it "extracts amqp connection settings" $
      parseAmqpUrl "amqp://ebA3ec8f:54EnktG1MgGvwPftC4y7BfZIQTYqrtvD@massive-scale-12.bigwig.lshift.net:10210/Uao4j39t8qD_" `shouldBe`
        AmqpSettings
            { amqpHostName    = "massive-scale-12.bigwig.lshift.net"
            , amqpVirtualHost = "Uao4j39t8qD_"
            , amqpUser        = "ebA3ec8f"
            , amqpPass        = "54EnktG1MgGvwPftC4y7BfZIQTYqrtvD"
            , amqpPort        = 10210
            }

