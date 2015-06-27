module Web.Heroku.RabbitMQ 
  ( AmqpSettings(..)
  , amqpConnSettings
  , parseAmqpUrl
  ) where

import Control.Monad
import Data.Text ( Text, pack )
import System.Environment

data AmqpSettings = AmqpSettings
    { amqpHostName    :: String
    , amqpVirtualHost :: Text
    , amqpUser        :: Text
    , amqpPass        :: Text
    , amqpPort        :: Int 
    } deriving (Show, Eq)

amqpConnSettings :: IO AmqpSettings
amqpConnSettings = liftM parseAmqpUrl (getEnv "RABBITMQ_BIGWIG_URL")

parseAmqpUrl :: String -> AmqpSettings
parseAmqpUrl = parse . pieces "" [] . trimProtocol
  where
    parse :: [String] -> AmqpSettings
    parse [ user
          , pass
          , host
          , port
          , vhst 
          ] = AmqpSettings host (pack vhst) (pack user) (pack pass) (read port)
    parse _ = error "Unexpected environment variable format."

    trimProtocol :: String -> String
    trimProtocol ('a':'m':'q':'p':':':'/':'/':rest) = rest
    trimProtocol str = str

    pieces acc ys [] = reverse (reverse acc:ys)
    pieces acc ys (x:xs) 
        | x `elem` "@:/" = pieces "" (reverse acc:ys) xs 
        | otherwise = pieces (x:acc) ys xs 

