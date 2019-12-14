{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Slack.Send where

import Control.Monad
import Data.Aeson
import Data.Char
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B

import Config
import Slack.Connect
import Slack.Received

{-
The bot processes the sent message.
If it's a regular message, then he just send it the right number of times. 
If it's "_help", then bot will send information about himself. 
If _repeat, then enters a loop waiting for the user to number or button(Emoji), get it to continue working further
-}

sendSlack :: Integer -> [B.ByteString] -> Req Value
sendSlack 1 args = connectSlack POST args 
sendSlack repeat args = do
  connectSlack POST args 
  sendSlack (repeat - 1) args

checkCommands :: Integer -> String -> (Integer -> [B.ByteString] -> Req Value) -> Req Integer
checkCommands repeat "_help" sendS = do
  res <- sendS repeat $ helpForm messageHelp
  return repeat
checkCommands repeat "_repeat" sendS = do
  sendS defaultRepeat $ helpForm messageSlackRepeat
  res <- helpLoop 
  sendS defaultRepeat $ helpForm $ successMessage ++ (show res)
  return res
  where
    helpLoop :: Req Integer
    helpLoop = do
        res <- received writeTs receiveSlack
        case res of
          ""       -> helpLoop 
          ":one:"  -> return 1
          ":two:"  -> return 2
          ":three:"-> return 3
          ":four:" -> return 4
          ":five:" -> return 5
          _ -> if all isDigit res then 
                return (read res :: Integer) else do
                sendS defaultRepeat $ helpForm "pls enter correct number"
                helpLoop
checkCommands repeat message sendS = do
  res <- sendS repeat $ helpForm message
  return repeat

send :: Integer -> String -> Req Integer
send repeat message = checkCommands repeat (checkMessage message) sendSlack