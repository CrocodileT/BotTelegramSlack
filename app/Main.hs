{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent

import Lib
import FunTelegram(runTelegram)
import FunSlack(runSlack)



{-
  create thread for runTelegram
  and use main thread for runSlack
-}
main :: IO ()
main = do
    forkIO runTelegram
    runSlack 
