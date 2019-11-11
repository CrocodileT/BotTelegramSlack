{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Writer
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import qualified Data.Text as T
import GHC.Generics
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B
import Data.Aeson.Types 
import qualified Data.HashMap.Strict as HM
import qualified Network.HTTP.Client as CL

import Lib
import Config
import ParseJSON
import BotCommand

--proxyHttpConfig = defaultHttpConfig { httpConfigProxy = Just (CL.Proxy (B.pack $ "200.111.182.6") 443)}
proxyHttpConfig = defaultHttpConfig { httpConfigProxy = Just (CL.Proxy (B.pack $ "141.125.82.106") 80)}


fromResultToList :: Result [(String, Integer, Integer)] -> [(String, Integer, Integer)]
fromResultToList (Success a) = a
fromResultToList (Error e) = [(e,0,0)]

answerTelegram :: (HttpMethod method, HttpBodyAllowed (AllowsBody method) (ProvidesBody NoReqBody)) => 
  method -> [String] -> Req (JsonResponse Value)
answerTelegram method args = do
  let urlHttps = B.pack $ "https://api.telegram.org/bot" ++ tokenTelegram ++ (foldr (++) "" args) 
      (url, options) = fromJust $ parseUrlHttps urlHttps
  req method url NoReqBody jsonResponse options

receiveTelegram :: [String] -> Req (JsonResponse Value)
receiveTelegram args = answerTelegram GET args

sendTelegram :: [String] -> Integer -> Req (JsonResponse Value)
sendTelegram args 1 = answerTelegram POST args
sendTelegram args countRepeat = do
  answerTelegram POST args
  sendTelegram args (countRepeat - 1)


addUser :: Integer -> Integer -> String -> InfoUsers -> InfoUsers
addUser update_id chat_id message users = (addUsers chat_id defaultRepeat (fst users), snd users)

updateUser :: Integer -> Integer -> InfoUsers -> Req a
updateUser update_id chat_id users = do
  let newRepeat = countRepeat chat_id (fst users)
  let newUsers = (updateUsers chat_id (fst users), snd users)
  sendTelegram ["/sendMessage?chat_id=", show chat_id, "&text=", messageRepeat, (show newRepeat)] defaultRepeat
  loop (update_id + 1) newUsers

checkButton :: Integer -> Integer -> String -> InfoUsers -> Req a
checkButton update_id chat_id message users = do
  let newRepeat = (read message :: Integer)
  let newUsers  = (updateRepeat chat_id newRepeat (fst users), snd users)
  loop (update_id + 1) newUsers

answerUser :: Integer -> Integer -> String -> InfoUsers -> Req a
answerUser update_id chat_id message users = do
  let newRepeat = countRepeat chat_id (fst users)
  sendTelegram ["/sendMessage?chat_id=", show chat_id, "&text=", message] newRepeat
  loop (update_id + 1) users

sendHelp :: Integer -> Integer -> InfoUsers -> Req a
sendHelp update_id chat_id users = do
  sendTelegram ["/sendMessage?chat_id=", show chat_id, "&text=", messageHelp] defaultRepeat
  loop (update_id + 1) users
      

loop :: Integer -> InfoUsers -> Req a
loop count users = do
  js <- receiveTelegram ["/getUpdates?offset=", show count]
  
  let resParse = fromResultToList $ parse parseTelegram (responseBody js)
  --liftIO $ print $ resParse
  when (null resParse) (loop count users)
  
  let (message, update_id, chat_id) = head resParse

  let checkUserT = not $ checkUser chat_id (fst users)
  let newUsers = if checkUserT then addUser update_id chat_id message users else users
  
  when (message == "/start" ) (loop (update_id + 1) newUsers)
  when (message == "/help"  ) (sendHelp update_id chat_id newUsers)
  when (message == "/repeat") (updateUser update_id chat_id newUsers)
  liftIO $ print $ "info : (" ++ (show message) ++ " , " ++ (show update_id) ++ " , " ++ (show chat_id) ++ ")"

  let checkUpdateRepeat = checkRepeat chat_id (fst newUsers)
  when (checkUpdateRepeat) (checkButton update_id chat_id message newUsers)

  {-sendTelegram ["/sendMessage?chat_id=", show chat_id, "&text=", message] defaultRepeat
  loop (update_id + 1) newUsers-}
  answerUser update_id chat_id message newUsers


main :: IO ()
main = liftIO $ runReq proxyHttpConfig $ loop 0 (HM.empty, HM.empty)
