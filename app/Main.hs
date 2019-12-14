{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent

import Telegram.MainTelegram(runTelegram)
import Slack.FunSlack(runSlack)


{-
  create thread for runTelegram
  and use main thread for runSlack
-}
main :: IO ()
main = do
    forkIO runTelegram
    runSlack 
