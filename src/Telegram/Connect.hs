
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.Connect where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.List
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B
import Data.Aeson.Types 
import qualified Data.HashMap.Strict as HM
import qualified Network.HTTP.Client as CL
import qualified Data.ByteString.Lazy as LB

import Config
import Telegram.JsonTelegram 

{- UserInfo
  fst - message
  snd - update_id
  last - chat_id(user_id)
-}
type UserInfo = (String, Integer, Integer)

checkMessage :: String -> Bool
checkMessage message = 
  let badChar = ["&","$","+",",",":","=",";","@"] in help badChar where
  help [] = False
  help (c:cs) | c `isPrefixOf` message = True
              | otherwise              = help cs

helpForm :: Integer -> String -> [B.ByteString]
helpForm chat_id mes = B.pack <$> ["/sendMessage?chat_id=", show chat_id, "&text=", mes]

addButton :: [B.ByteString] -> [B.ByteString]
addButton args = args <> [B.pack "&reply_markup="] <> [LB.toStrict $ encode $ buttons]

connectTelegram :: (HttpMethod method, HttpBodyAllowed (AllowsBody method) (ProvidesBody NoReqBody)) => 
  method -> [B.ByteString] -> Req Value
connectTelegram method args = do
  let urlHttps = (B.pack $ ("https://api.telegram.org/bot" ++ tokenTelegram)) <> (foldr (<>) "" args) 
      (url, options) = fromJust $ parseUrlHttps urlHttps
  res <- req method url NoReqBody jsonResponse options
  return $ responseBody res
 