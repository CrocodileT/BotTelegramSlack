{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module TelegramTest.SendTest where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Aeson
import Data.Text as T
import Data.Scientific as Scientific
import GHC.Exts
import GHC.Generics
import Network.HTTP.Req
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as HM

import Telegram.FunTelegram
import qualified Telegram.DataBaseUsers as D

checkResult :: Eq a => Req a -> a -> IO Bool
checkResult r s = runReq defaultHttpConfig $ do
  res <- r
  return $ res == s

sendTest = testGroup "send tests:" [test1, test2, test3, test4, test5]
  
test1 = testCase "empty usersDataBase" $ do
  res <- send1
  if res 
    then return ()
    else assertFailure "error send test 1"

test2 = testCase "add user in usersDataBase" $ do
  res <- send2
  if res 
    then return ()
    else assertFailure "error send test 2"

test3 = testCase "check /start" $ do
  res <- send3
  if res 
    then return ()
    else assertFailure "error send test 3"

test4 = testCase "check /help" $ do
  res <- send4
  if res 
    then return ()
    else assertFailure "error send test 4"
    
test5 = testCase "check /repeat" $ do
  res <- send5
  if res 
    then return ()
    else assertFailure "error send test 5"


emptySend :: Integer -> [B.ByteString] -> Req Value
emptySend _ _= return Null 

user = HM.singleton 1 (D.UserRepeat 1 False)

send1 :: IO Bool 
send1 = checkResult (send ("hello", 1, 2) HM.empty emptySend) (2, user)

send2 :: IO Bool
send2 = checkResult (send ("hello", 1, 2) user emptySend) (2, D.insertUser 2 1 user)

send3 :: IO Bool
send3 = checkResult (send ("/start", 1, 1) HM.empty emptySend) (2, user)

send4 :: IO Bool
send4 = checkResult (send ("/help", 1, 2) user emptySend) (2, D.insertUser 2 1 user)

send5 :: IO Bool
send5 = checkResult (send ("/repeat", 1, 2) user emptySend) (2,D.waitRepeatUsers 2 (D.insertUser 2 1 user))

