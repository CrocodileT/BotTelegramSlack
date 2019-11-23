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
import Control.Time

import Lib
import Config
import ParseJSON
import BotCommand
import FunTelegram


fromResultToList' :: Result [(String, String)] -> [(String, String)]
fromResultToList' (Success a) = a
fromResultToList' (Error e) = [(e,"0")]

connectSlack :: (HttpMethod method, HttpBodyAllowed (AllowsBody method) (ProvidesBody NoReqBody)) => 
  method -> [B.ByteString] -> Req (JsonResponse Value)
connectSlack method args = do
  let urlHttps = (B.pack $ "https://slack.com/api/") <> (foldr (<>) "" args) <> (B.pack "&pretty=1")
      (url, options) = fromJust $ parseUrlHttps urlHttps
  req method url NoReqBody jsonResponse options

receiveSlack :: Integer -> Req (JsonResponse Value)
receiveSlack ts = do 
  liftIO $ print (show ts)
  let args = B.pack <$> ["channels.history", tokenSlack, idChannelTest, "&oldest=" ++ (show ts)]
  connectSlack GET args 

sendSlack :: Integer -> [B.ByteString] -> Req (JsonResponse Value)
sendSlack 1 args = connectSlack POST args 
sendSlack count args = do
  delay (1 :: Integer)
  connectSlack POST args 
  sendSlack (count - 1) args

helpForm :: String -> [B.ByteString]
helpForm mes = B.pack <$> ["chat.postMessage", tokenBootSlack, idChannelTest, "text=" ++ mes]

checkMes :: String -> String
checkMes message = 
  let badChar = ["&","$","+",",",":","/","=",";","?","@"] in help badChar where
  help [] = message
  help (c:cs) | c `isPrefixOf` message = badMessage
              | otherwise              = help cs

answerSlack :: [(String, String)] -> Req ()
answerSlack messages = do
  let ts = (snd . head) messages
  liftIO $ writeTs (takeWhile (/= '.') ts)
  mapM_ ((sendSlack defaultRepeat) . helpForm . checkMes . fst) messages

readTs :: IO Integer
readTs = do
  str <- readFile "lastTime.txt"
  let res = if str /= "" then read str :: Integer else 1
  return (res + 1)

writeTs :: String -> IO ()
writeTs num = writeFile "lastTime.txt" num

loop' :: Req ()
loop' = do
  delay (1 :: Integer)
  lastTs <- liftIO readTs
  res <- receiveSlack lastTs
  let resParse = (fromResultToList') $ (fromJust <$>) <$> (parse parseSlack (responseBody res :: Value))
  liftIO $ print $ resParse

  when (null resParse) (loop')

  answerSlack resParse
  loop'

main :: IO ()
main = liftIO $ runReq proxyHttpConfig $ loop'
