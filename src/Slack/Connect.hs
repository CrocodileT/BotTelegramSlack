{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Slack.Connect where

import Data.Aeson
import Data.List
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B
import Control.Time

import Config
import Slack.JsonSlack


helpForm :: String -> [B.ByteString]
helpForm mes = B.pack <$> ["chat.postMessage", tokenBootSlack, idChannelTest, "text=" ++ mes]

checkMessage :: String -> String
checkMessage message = 
  let badChar = ["&","$","+",",",":","=",";","@"] in help badChar where
  help [] = message
  help (c:cs) | c `isPrefixOf` message = badMessage
              | otherwise              = help cs

{-
"delay" is needed so that methods are not used too often, 
otherwise slack throws an error 
-}

connectSlack :: (HttpMethod method, HttpBodyAllowed (AllowsBody method) (ProvidesBody NoReqBody)) => 
  method -> [B.ByteString] -> Req Value
connectSlack method args = do
  delay (1 :: Integer)
  let urlHttps = (B.pack $ "https://slack.com/api/") <> (foldr (<>) "" args) <> (B.pack "&pretty=1")
      (url, options) = fromJust $ parseUrlHttps urlHttps
  res <- req method url NoReqBody jsonResponse options
  return $ responseBody res