{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Lib
import Config
import BotCommand
--import FunTelegram
import FunSlack


main :: IO ()
main = runSlack 
