{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Test.Tasty
--import Test.Tasty.HUnit

import Telegram.FunTelegram
import TelegramTest.ReceivedTest (receivedTest)
import TelegramTest.SendTest (sendTest)

main :: IO ()
main = defaultMain $ testGroup "Tests" [
    testGroup "Telegram" [receivedTest, sendTest]
    ]
