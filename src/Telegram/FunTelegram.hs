
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.FunTelegram where

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
import qualified Telegram.DataBaseUsers as D

{-
  The bot polls telegram about whether there are new messages or not.
  If new messages appeared (received returns a non-empty list),
  then use the send function to respond to the most recent message, and make the offset parameter offset
  With / repeat, each user can set their personal number of repetitions
    DataBaseUsers is used to store repetitions of each user, 
  User information is stored only while the bot is running. 
    Responses from telegram are processed in JsonTelegram
-}

proxyHttpConfig = defaultHttpConfig { httpConfigProxy = Just (CL.Proxy (B.pack $ "141.125.82.106") 80)}

{- UserInfo
  fst - message
  snd - update_id
  last - chat_id(user_id)
-}
type UserInfo = (String, Integer, Integer)


fromResultToList :: Result [UserInfo] -> [UserInfo]
fromResultToList (Success a) = a
fromResultToList (Error e) = [(e,0,0)]

----------Check url about bad char
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
 
----------

----------received
getTelegram :: Integer -> Req Value
getTelegram offset = do
  res <- connectTelegram GET (B.pack <$> ["/getUpdates?offset=", show offset])
  return res

received :: Req Value -> Req [UserInfo]
received getTg = do
  answer <- getTg
  
  let resParse = fromResultToList $ parse parseTelegram answer
  return resParse
----------

----------Send
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
----------

loop :: Integer -> D.Users -> Req ()
loop offset users = do
  result <- received $ getTelegram offset
  when (null result) (loop offset users)
  (newCount, newUsers) <- send (head result) users sendTelegram
  loop newCount newUsers

runTelegram :: IO ()
runTelegram = liftIO $ runReq proxyHttpConfig $ loop defaultRepeat HM.empty