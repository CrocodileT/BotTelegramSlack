{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Config

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B

data MyData = MyData
  { size  :: Int
  , color :: Text
  } deriving (Show, Generic)

instance ToJSON MyData
instance FromJSON MyData

main :: IO ()
main = runReq defaultHttpConfig $ do
  -- This is an example of what to do when URL is given dynamically. Of
  -- course in a real application you may not want to use 'fromJust'.
  let (url, options) = fromJust (parseUrlHttps "https://httpbin.org/get?foo=bar")
  response <- req GET _ NoReqBody jsonResponse $
    "from" =: (15 :: Int)           <>
    "to"   =: (67 :: Int)           <>
    basicAuth "username" "password" <>
    options                         <> -- contains the ?foo=bar part
    port 443 -- here you can put any port of course
  liftIO $ print (responseBody response :: Value)
