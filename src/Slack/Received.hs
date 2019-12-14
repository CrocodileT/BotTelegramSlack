{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Slack.Received where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Maybe (fromJust)
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B
import Data.Aeson.Types 

import Config
import Slack.JsonSlack
import Slack.Connect

{-
The bot receives the parameter "ts" from the file, using it forms a request to slack, 
after receiving the message writes the parameter "ts" of the last message processed by it to the file
-}

readTs :: IO String
readTs = do
  str <- readFile "lastTime.txt"
  return (if str /= "" then str else "1.0")

writeTs :: [(String, String)] -> Req ()
writeTs []  = return ()
writeTs mes = do
  let ts = (snd . last) mes
  liftIO $ writeFile "lastTime.txt" ts

fromResultToList :: Result [(String, String)] -> [(String, String)]
fromResultToList (Success a) = a
fromResultToList (Error e) = [(e,"1.0")]

receiveSlack :: Req Value
receiveSlack = do 
  lastTs <- liftIO readTs
  let args = B.pack <$> ["channels.history", tokenSlack, idChannelTest, "&oldest=" ++ lastTs]
  connectSlack GET args 


received :: ([(String, String)] -> Req ()) -> Req Value -> Req String
received tsWrite recSlack = do
  res <- recSlack
  let resParse = (fromResultToList) $ (fromJust <$>) <$> (parse parseSlack res)
  tsWrite resParse
  return (create resParse)
  where
    create :: [(String,String)] -> String
    create []  = ""
    create mes = (fst . last) mes