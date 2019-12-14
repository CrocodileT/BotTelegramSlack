{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module SlackTest.SendTest where

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

import Slack.Send

checkResult :: Eq a => Req a -> Req a -> IO Bool
checkResult r s = runReq defaultHttpConfig $ do
  res <- r
  res' <- s
  return $ res == res'

sendTest = testGroup "send tests:" [test]
  
test = testCase "send smth" $ do
  res <- send'
  if res 
    then return ()
    else assertFailure "error send test"

emptySend :: Integer -> [B.ByteString] -> Req Value
emptySend _ _= return Null 

send' :: IO Bool 
send' = checkResult (checkCommands 1 "hello" emptySend) $ return 1