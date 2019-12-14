{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Test.Tasty

import TelegramTest.ReceivedTest (receivedTest)
import TelegramTest.SendTest (sendTest)
import qualified SlackTest.ReceivedTest as SR (receivedTest)
import qualified SlackTest.SendTest as SS (sendTest)

main :: IO ()
main = defaultMain $ testGroup "Tests" [
    testGroup "Telegram" [receivedTest, sendTest],
    testGroup "Slack" [SR.receivedTest, SS.sendTest]
    ]
