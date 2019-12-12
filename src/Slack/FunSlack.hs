{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Slack.FunSlack where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.List
import Data.Char
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import qualified Data.Text as T
import GHC.Generics
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B
import Data.Aeson.Types 
import qualified Data.ByteString.Lazy as LB
import Control.Time

import Config
import Slack.JsonSlack


fromResultToList :: Result [(String, String)] -> [(String, String)]
fromResultToList (Success a) = a
fromResultToList (Error e) = [(e,"1.0")]


helpForm :: String -> [B.ByteString]
helpForm mes = B.pack <$> ["chat.postMessage", tokenBootSlack, idChannelTest, "text=" ++ mes]


checkMessage :: String -> String
checkMessage message = 
  let badChar = ["&","$","+",",",":","=",";","@"] in help badChar where
  help [] = message
  help (c:cs) | c `isPrefixOf` message = badMessage
              | otherwise              = help cs

-----File
readTs :: IO String
readTs = do
  str <- readFile "lastTime.txt"
  return (if str /= "" then str else "1.0")


writeTs :: [(String, String)] -> Req ()
writeTs []  = return ()
writeTs mes = do
  let ts = (snd . last) mes
  liftIO $ writeFile "lastTime.txt" ts
-----


connectSlack :: (HttpMethod method, HttpBodyAllowed (AllowsBody method) (ProvidesBody NoReqBody)) => 
  method -> [B.ByteString] -> Req Value
connectSlack method args = do
  delay (1 :: Integer)
  let urlHttps = (B.pack $ "https://slack.com/api/") <> (foldr (<>) "" args) <> (B.pack "&pretty=1")
      (url, options) = fromJust $ parseUrlHttps urlHttps
  res <- req method url NoReqBody jsonResponse options
  return $ responseBody res


-----Received
receiveSlack :: Req Value
receiveSlack = do 
  lastTs <- liftIO readTs
  let args = B.pack <$> ["channels.history", tokenSlack, idChannelTest, "&oldest=" ++ lastTs]
  connectSlack GET args 


received :: Req String
received = do
  res <- receiveSlack
  let resParse = (fromResultToList) $ (fromJust <$>) <$> (parse parseSlack (responseBody res :: Value))
  writeTs resParse
  return (create resParse)
  where
    create :: [(String,String)] -> String
    create []  = ""
    create mes = (fst . last) mes
-----

-----Send
sendSlack :: Integer -> [B.ByteString] -> Req Value
sendSlack 1 args = connectSlack POST args 
sendSlack repeat args = do
  connectSlack POST args 
  sendSlack (repeat - 1) args

checkCommands :: Integer -> String -> Req Integer
checkCommands repeat "_help" = do
  res <- sendSlack repeat $ helpForm messageHelp
  return repeat
checkCommands repeat "_repeat" = do
  sendSlack defaultRepeat $ helpForm messageSlackRepeat
  res <- helpLoop 
  sendSlack defaultRepeat $ helpForm $ successMessage ++ (show res)
  return res
  where
    helpLoop :: Req Integer
    helpLoop = do
        res <- received
        case res of
          ""       -> helpLoop 
          ":one:"  -> return 1
          ":two:"  -> return 2
          ":three:"-> return 3
          ":four:" -> return 4
          ":five:" -> return 5
          _ -> if all isDigit res then 
                return (read res :: Integer) else do
                  sendSlack defaultRepeat $ helpForm "pls enter correct number"
                  helpLoop
checkCommands repeat message = do
  res <- sendSlack repeat $ helpForm message
  return repeat

send :: Integer -> String -> Req Integer
send repeat message = checkCommands repeat $ checkMessage message
-----


loop :: Integer -> Req ()
loop repeat = do
  answer <- received
  liftIO $ print answer
  when (null answer) (loop repeat)

  newRepeat <- send repeat answer 
  loop newRepeat

runSlack :: IO ()
runSlack = liftIO $ runReq defaultHttpConfig $ loop defaultRepeat