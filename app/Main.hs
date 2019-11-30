{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent

import Lib
import FunTelegram
import FunSlack




main :: IO ()
main = do
    forkIO runTelegram
    runSlack 
