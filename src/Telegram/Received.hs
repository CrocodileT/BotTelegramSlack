
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.Received where

import Data.Aeson
import Data.Aeson.Types
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B

import Config
import Telegram.JsonTelegram 
import Telegram.Connect

fromResultToList :: Result [UserInfo] -> [UserInfo]
fromResultToList (Success a) = a
fromResultToList (Error e) = [(e,0,0)]

getTelegram :: Integer -> Req Value
getTelegram offset = do
  res <- connectTelegram GET (B.pack <$> ["/getUpdates?offset=", show offset])
  return res

received :: Req Value -> Req [UserInfo]
received getTg = do
  answer <- getTg
  
  let resParse = fromResultToList $ parse parseTelegram answer
  return resParse