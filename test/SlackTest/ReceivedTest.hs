{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module SlackTest.ReceivedTest where

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

import Slack.FunSlack

checkResult :: Eq a => Req a -> a -> IO Bool
checkResult r s = runReq defaultHttpConfig $ do
  res <- r
  return $ res == s

receivedTest = testGroup "received tests:" [test1, test2]
  
test1 = testCase "single answer" $ do
  res <- checkResult (received emptyWriteTs rec1) "hello"
  if res 
    then return ()
    else assertFailure "error received test 1"

test2 = testCase "plural answer" $ do
  res <- checkResult (received emptyWriteTs rec2) "hello3"
  if res 
    then return ()
    else assertFailure "error received test 2"

emptyWriteTs :: [(String, String)] -> Req ()
emptyWriteTs _ = return ()

info :: (Text, Text) -> Value
info (text, ts) = Object $ fromList [ 
  ("user", Number 0),
  ("text", String text),
  ("ts"  , String ts)
  ]

rec1 :: Req Value
rec1 = return $ Object $ fromList [
  ("messages", Array $ fromList [
    info ("hello", "1")
    ])
  ]

rec2 :: Req Value
rec2 = return $ Object $ fromList [
  ("messages", Array $ fromList [
    info ("hello1", "1"),
    info ("hello2", "2"),
    info ("hello3", "3")
    ])
  ]


