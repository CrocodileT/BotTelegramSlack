{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module TelegramTest.ReceivedTest where

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

import Telegram.Received

checkResult :: Eq a => Req a -> a -> IO Bool
checkResult r s = runReq defaultHttpConfig $ do
  res <- r
  return $ res == s

receivedTest = testGroup "received tests:" [test1, test2, test3, test4]
  
test1 = testCase "single answer" $ do
  res <- checkResult (received rec1) [("hello", 1, 1)]
  if res 
    then return ()
    else assertFailure "error received test 1"

test2 = testCase "single answer (edited messages)" $ do
  res <- checkResult (received rec2) [("hello", 1, 1)]
  if res 
    then return ()
    else assertFailure "error received test 2"

test3 = testCase "plural answer" $ do
  res <- checkResult (received rec3) [("hello1", 1, 1), ("hello2", 1, 2), ("hello3", 1, 3)]
  if res 
    then return ()
    else assertFailure "error received test 3"

test4 = testCase "plural answer (usual and edited messages) " $ do
  res <- checkResult (received rec4) [("hello1", 1, 1), ("hello2", 1, 2), ("hello3", 1, 3)]
  if res 
    then return ()
    else assertFailure "error received test 4"

mes :: (Text, Scientific, Scientific) -> Value
mes (text, update_id, chat_id) = Object $ fromList [ 
  ("update_id", Number update_id),
  ("message", Object $ fromList [
    ("chat", Object $ fromList [("id", Number chat_id)]),
    ("text", String text)]
  )
  ]

editMes :: (Text, Scientific, Scientific) -> Value
editMes (text, update_id, chat_id) = Object $ fromList [ 
  ("update_id", Number update_id),
  ("edited_message", Object $ fromList [
    ("chat", Object $ fromList [("id", Number chat_id)]),
    ("text", String text)]
  )
  ] 

rec1 :: Req Value
rec1 = return $ Object $ fromList [
  ("result", Array $ fromList [
    mes ("hello", 1, 1)
    ])
  ]

rec2 :: Req Value
rec2 = return $ Object $ fromList [
  ("result", Array $ fromList [
    editMes ("hello", 1, 1)
    ])
  ]

rec3 :: Req Value
rec3 = return $ Object $ fromList [
  ("result", Array $ fromList [
    mes ("hello1", 1, 1),
    mes ("hello2", 1, 2),
    mes ("hello3", 1, 3)
    ])
  ]

rec4 :: Req Value
rec4 = return $ Object $ fromList [
  ("result", Array $ fromList [
    mes ("hello1", 1, 1),
    editMes ("hello2", 1, 2),
    mes ("hello3", 1, 3)
    ])
  ]