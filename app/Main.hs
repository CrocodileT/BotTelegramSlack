{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent

import Lib
import FunTelegram(runTelegram)
import FunSlack(runSlack)




main :: IO ()
main = do
    forkIO runTelegram
    runSlack 
