{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Slack.MainSlack where

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
import Slack.Send
import Slack.Received

{-
The bot works for a workspace. 
Each time, it receives a history update and responds to all unanswered messages. 
In order not to receive a large array of chat history when restarting the bot, 
the bot saves the time of the last message from the chat with which it worked,
this parameter is called "ts" and is stored in a special file lastTime.txt
-}

loop :: Integer -> Req ()
loop repeat = do
  answer <- received writeTs receiveSlack
  liftIO $ print answer
  when (null answer) (loop repeat)

  newRepeat <- send repeat answer 
  loop newRepeat

runSlack :: IO ()
runSlack = liftIO $ runReq defaultHttpConfig $ loop defaultRepeat