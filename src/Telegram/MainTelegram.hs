
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.MainTelegram where

import Control.Monad
import Control.Monad.IO.Class
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as HM
import qualified Network.HTTP.Client as CL

import Config
import Telegram.Received
import Telegram.Send
import qualified Telegram.DataBaseUsers as D (Users)

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

loop :: Integer -> D.Users -> Req ()
loop offset users = do
  result <- received $ getTelegram offset
  when (null result) (loop offset users)
  (newCount, newUsers) <- send (head result) users sendTelegram
  loop newCount newUsers

runTelegram :: IO ()
runTelegram = liftIO $ runReq proxyHttpConfig $ loop defaultRepeat HM.empty