
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FunTelegram where

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
import JsonTelegram 
import qualified DataBaseUsers as D

{-
  Бот постоянно опрашивает telegram о том, появились ли новые сообщения или нет.
  Если новые сообщения появились (received возвращает не пустой список),
  то с помощью функции send отвечаем на самое последнее сообщение, и делаем смещение по параметру offset
  С помощью /repeat, каждый пользователь может установить свое персональное количество повторов
  Для хранения повторов каждого пользователя используется DataBaseUsers, 
  Информация о пользователях хранится только во время работы бота. 
  Ответы от telegram обрабатываются в JsonTelegram
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

--Check url about bad char
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
  method -> [B.ByteString] -> Req (JsonResponse Value)
connectTelegram method args = do
  let urlHttps = (B.pack $ ("https://api.telegram.org/bot" ++ tokenTelegram)) <> (foldr (<>) "" args) 
      (url, options) = fromJust $ parseUrlHttps urlHttps
  req method url NoReqBody jsonResponse options

-----received
receivedTelegram :: Integer -> Req (JsonResponse Value)
receivedTelegram count = connectTelegram GET (B.pack <$> ["/getUpdates?offset=", show count])

received :: Integer -> Req [(String, Integer, Integer)]
received count = do
  answer <- receivedTelegram count
  
  let resParse = fromResultToList $ parse parseTelegram (responseBody answer)
  liftIO $ print $ resParse
  return resParse
-----

-----Send
sendTelegram :: Integer -> [B.ByteString] -> Req (JsonResponse Value)
sendTelegram 1 args  = connectTelegram POST args
sendTelegram countRepeat args  = do
  connectTelegram POST args
  sendTelegram (countRepeat - 1) args


addUser ::UserInfo -> D.Users -> D.Users
addUser (_, _, chat_id) users = D.insertUser chat_id defaultRepeat users

checkCommand, updateUserRepeat, checkButton, answerUser, sendHelp, badInfo :: UserInfo -> D.Users -> Req (D.Users)
updateUserRepeat (_, _, chat_id) users = do
  let newRepeat = D.returnCountRepeat chat_id users
  let newUsers = D.waitRepeatUsers chat_id users
  sendTelegram 1 (addButton $ helpForm chat_id (messageRepeat ++  (show newRepeat)))
  return newUsers

checkButton (message, _, chat_id) users = do
  let newRepeat = (read message :: Integer)
  let newUsers  = D.setNewRepeat chat_id newRepeat users
  sendTelegram 1 (helpForm chat_id $ successMessage ++ show (newRepeat))
  return newUsers

answerUser (message, _, chat_id) users = do
  let newRepeat = D.returnCountRepeat chat_id users
  sendTelegram newRepeat (helpForm chat_id message)
  return users

sendHelp (_, _, chat_id) users = do
  sendTelegram 1 (helpForm chat_id messageHelp)
  return users

badInfo (_, _, chat_id) users = do
  sendTelegram 1 (helpForm chat_id badMessage)
  return users


checkCommand info@("/start", _, _) users = return users
checkCommand info@("/help", _, _) users = sendHelp info users
{-
Если пользователь начал устанавливать количество повторов
То для него значение UpdateRepeat в DataBase будет True 
и пока он не введет число он не выйдет он бот будет просить его ввести число
-}
checkCommand info@("/repeat", _, _) users = updateUserRepeat info users
checkCommand info@(message, _, chat_id) users = do
  let updateRepeat = D.returnUpdateRepeat chat_id users
  newUsers <- if updateRepeat then checkButton info users
                else answerUser info users
  return newUsers

send :: UserInfo -> D.Users -> Req (Integer, D.Users)
send info@(message, update_id, chat_id) users = do
  let resCheck = not $ D.memberUser chat_id users
  let newUsers = if resCheck then addUser info users else users
  
  updateUsers <- case checkMessage message of
                  True -> badInfo info newUsers
                  _    -> checkCommand info newUsers

  return (update_id + 1, updateUsers)
-----

loop :: Integer -> D.Users -> Req ()
loop count users = do
  result <- received count
  when (null result) (loop count users)
  (newCount, newUsers) <- send (head result) users
  loop newCount newUsers

runTelegram :: IO ()
runTelegram = liftIO $ runReq proxyHttpConfig $ loop defaultRepeat HM.empty