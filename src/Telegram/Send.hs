
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.Send where

import Control.Monad
import Data.Aeson
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as HM

import Config
import Telegram.Connect
import qualified Telegram.DataBaseUsers as D




sendTelegram :: Integer -> [B.ByteString] -> Req Value
sendTelegram 1 args  = connectTelegram POST args
sendTelegram countRepeat args  = do
  connectTelegram POST args
  sendTelegram (countRepeat - 1) args

addUser ::UserInfo -> D.Users -> D.Users
addUser (_, _, chat_id) users = D.insertUser chat_id defaultRepeat users

checkCommand, updateUserRepeat, checkButton, answerUser, sendHelp, badInfo :: 
  UserInfo -> D.Users -> (Integer -> [B.ByteString] -> Req Value) -> Req (D.Users)
updateUserRepeat (_, _, chat_id) users sendTg = do
  let newRepeat = D.returnCountRepeat chat_id users
  let newUsers  = D.waitRepeatUsers chat_id users
  sendTg 1 (addButton $ helpForm chat_id (messageRepeat ++  (show newRepeat)))
  return newUsers

checkButton (message, _, chat_id) users sendTg = do
  let newRepeat = (read message :: Integer)
  let newUsers  = D.setNewRepeat chat_id newRepeat users
  sendTg 1 (helpForm chat_id $ successMessage ++ show (newRepeat))
  return newUsers

answerUser (message, _, chat_id) users sendTg = do
  let newRepeat = D.returnCountRepeat chat_id users
  sendTg newRepeat (helpForm chat_id message)
  return users

sendHelp (_, _, chat_id) users sendTg = do
  sendTg 1 (helpForm chat_id messageHelp)
  return users

badInfo (_, _, chat_id) users sendTg = do
  sendTg 1 (helpForm chat_id badMessage)
  return users


checkCommand info@("/start", _, _) users sendTg = return users
checkCommand info@("/help", _, _) users sendTg = sendHelp info users sendTg
{-
If the user has started to set the number of repetitions
Then for it the value of UpdateRepeat in the DataBase will be True 
and until he enters a number, the bot will ask him to enter a number
-}
checkCommand info@("/repeat", _, _) users sendTg = updateUserRepeat info users sendTg
checkCommand info@(message, _, chat_id) users sendTg = do
  let updateRepeat = D.returnUpdateRepeat chat_id users
  newUsers <- if updateRepeat 
                then checkButton info users sendTg
                else answerUser info users sendTg
  return newUsers

send :: UserInfo -> D.Users -> (Integer -> [B.ByteString] -> Req Value) -> Req (Integer, D.Users)
send info@(message, update_id, chat_id) users sendTg = do
  let resCheck = not $ D.memberUser chat_id users
  let newUsers = if resCheck 
                    then addUser info users 
                    else users
  
  updateUsers <- case checkMessage message of
                  True -> badInfo info newUsers sendTg
                  _    -> checkCommand info newUsers sendTg

  return (update_id + 1, updateUsers)