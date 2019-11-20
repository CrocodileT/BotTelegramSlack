{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Writer
import Data.Aeson
import Data.List
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import qualified Data.Text as T
import GHC.Generics
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B
import Data.Aeson.Types 
import qualified Data.HashMap.Strict as HM
import qualified Network.HTTP.Client as CL
import qualified Data.ByteString.Lazy as LB

import Lib
import Config
import ParseJSON
import BotCommand


key1 :: KeyButton
key1 = KeyButton "1"

key2 :: KeyButton
key2 = KeyButton "2"

key3 :: KeyButton
key3 = KeyButton "3"

key4 :: KeyButton
key4 = KeyButton "4"

key5 :: KeyButton
key5 = KeyButton "5"

buttons :: ReplyKey
buttons = ReplyKey [[key1 , key2 , key3 ,key4 , key5]]

data KeyButton = KeyButton {
  text :: T.Text } deriving (Generic, Show)

data ReplyKey = ReplyKey {
  keyboard :: [[KeyButton]] } deriving (Generic, Show)

instance ToJSON KeyButton
instance FromJSON KeyButton

instance ToJSON ReplyKey
instance FromJSON ReplyKey


--proxyHttpConfig = defaultHttpConfig { httpConfigProxy = Just (CL.Proxy (B.pack $ "200.111.182.6") 443)}
proxyHttpConfig = defaultHttpConfig { httpConfigProxy = Just (CL.Proxy (B.pack $ "141.125.82.106") 80)}


fromResultToList :: Result [(String, Integer, Integer)] -> [(String, Integer, Integer)]
fromResultToList (Success a) = a
fromResultToList (Error e) = [(e,0,0)]

answerTelegram :: (HttpMethod method, HttpBodyAllowed (AllowsBody method) (ProvidesBody NoReqBody)) => 
  method -> [B.ByteString] -> Req (JsonResponse Value)
answerTelegram method args = do
  let urlHttps = (B.pack $ ("https://api.telegram.org/bot" ++ tokenTelegram)) <> (foldr (<>) "" args) 
      (url, options) = fromJust $ parseUrlHttps urlHttps
  req method url NoReqBody jsonResponse options

receiveTelegram :: [B.ByteString] -> Req (JsonResponse Value)
receiveTelegram args = answerTelegram GET args

sendTelegram :: [B.ByteString] -> Integer -> Req (JsonResponse Value)
sendTelegram args 1 = answerTelegram POST args
sendTelegram args countRepeat = do
  answerTelegram POST args
  sendTelegram args (countRepeat - 1)


addUser :: Integer -> Integer -> InfoUsers -> InfoUsers
addUser update_id chat_id users = (addUsers chat_id defaultRepeat (fst users), snd users)

updateUser :: Integer -> Integer -> InfoUsers -> Req a
updateUser update_id chat_id users = do
  let newRepeat = countRepeat chat_id (fst users)
  let newUsers = (updateUsers chat_id (fst users), snd users)
  --let buttons = ReplyKey { KeyButton "1", KeyButton "2", KeyButton "3", KeyButton "4", KeyButton "5" }
  let args = (B.pack <$> ["/sendMessage?chat_id=", show chat_id, "&text=", messageRepeat, (show newRepeat), "&reply_markup="]) <> [LB.toStrict $ encode $ buttons]
  sendTelegram args 1
  loop (update_id + 1) newUsers

checkButton :: Integer -> Integer -> String -> InfoUsers -> Req a
checkButton update_id chat_id message users = do
  let newRepeat = (read message :: Integer)
  let newUsers  = (updateRepeat chat_id newRepeat (fst users), snd users)
  loop (update_id + 1) newUsers

answerUser :: Integer -> Integer -> String -> InfoUsers -> Req a
answerUser update_id chat_id message users = do
  let newRepeat = countRepeat chat_id (fst users)
  let args = B.pack <$> ["/sendMessage?chat_id=", show chat_id, "&text=", message]
  sendTelegram args newRepeat
  loop (update_id + 1) users

sendHelp :: Integer -> Integer -> InfoUsers -> Req a
sendHelp update_id chat_id users = do
  let args = B.pack <$> ["/sendMessage?chat_id=", show chat_id, "&text=", messageHelp]
  sendTelegram args 1
  loop (update_id + 1) users

badInfo :: Integer -> Integer -> InfoUsers -> Req a
badInfo update_id chat_id users = do
  let args = B.pack <$> ["/sendMessage?chat_id=", show chat_id, "&text=", badMessage]
  sendTelegram args 1
  loop (update_id + 1) users

checkMessage :: String -> Bool
checkMessage message = 
  let badChar = ["&","$","+",",",":","/","=",";","?","@"] in help badChar where
  help [] = False
  help (c:cs) | c `isPrefixOf` message = True
              | otherwise              = help cs
  
     

loop :: Integer -> InfoUsers -> Req a
loop count users = do
  js <- receiveTelegram (B.pack <$> ["/getUpdates?offset=", show count])
  
  let resParse = fromResultToList $ parse parseTelegram (responseBody js)
  liftIO $ print $ resParse
  when (null resParse) (loop count users)
  
  let (message, update_id, chat_id) = head resParse


  let checkUserT = not $ checkUser chat_id (fst users)
  let newUsers = if checkUserT then addUser update_id chat_id users else users

  when (checkMessage message) (badInfo update_id chat_id newUsers)
  
  when (message == "/start" ) (loop (update_id + 1) newUsers)
  when (message == "/help"  ) (sendHelp update_id chat_id newUsers)
  when (message == "/repeat") (updateUser update_id chat_id newUsers)
  --liftIO $ print $ "info : (" ++ (show message) ++ " , " ++ (show update_id) ++ " , " ++ (show chat_id) ++ ")"

  let checkUpdateRepeat = checkRepeat chat_id (fst newUsers)
  when (checkUpdateRepeat) (checkButton update_id chat_id message newUsers)

  {-sendTelegram ["/sendMessage?chat_id=", show chat_id, "&text=", message] defaultRepeat
  loop (update_id + 1) newUsers-}
  answerUser update_id chat_id message newUsers

  
{-main :: IO ()
main = liftIO $ runReq proxyHttpConfig $ loop 0 (HM.empty, HM.empty)-}


dataSlack :: KeyButton
dataSlack = KeyButton "Hello, world"

main = runReq defaultHttpConfig $ do
  {-let urlHttps = (B.pack $ ("https://hooks.slack.com/services/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"))
      (url, options) = fromJust $ parseUrlHttps urlHttps
  ans <- req POST url (ReqBodyJson dataSlack) jsonResponse options
  liftIO $ print (responseBody ans :: Value)-}
  let urlHttps = (B.pack $ ("https://hooks.slack.com/services/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"))
      (url, options) = fromJust $ parseUrlHttps urlHttps
  ans <- req POST url (ReqBodyJson dataSlack) jsonResponse options
  liftIO $ print (responseBody ans :: Value)


-- https://slack.com/api/chat.postMessage/TQKC05FD3/DQKC05XM3/xoxp-835408185445-837610386518-837619345606-32ffc848362220d3f0b6e6fdb979abc2