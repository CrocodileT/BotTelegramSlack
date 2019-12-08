{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Aeson
import GHC.Exts
import GHC.Generics
import Network.HTTP.Req
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as B

import Telegram.FunTelegram

main :: IO ()
main = do
    defaultMain (testGroup "Our Library Tests" [check])

checkResult :: Req [(String, Integer, Integer)] -> [(String, Integer, Integer)] -> IO Bool
checkResult r s = runReq defaultHttpConfig $ do
  res <- r
  return $ res == s
check1 = testCase "recived test 1" $ do
  res <- checkResult (received tg1) [("hello", 1, 2)]
  if res 
    then print "succesful recived test 1" 
    else assertFailure "error recived test 1"

tg1 :: Req Value
tg1 = return $ Object $ fromList [
  ("result", Array $ fromList [
    Object $ fromList [ 
      ("update_id", Number 1),
      ("message", Object $ fromList [
        ("chat", Object $ fromList [("id", Number 2)]),
        ("text", "hello")]
      )
    ]])
  ]