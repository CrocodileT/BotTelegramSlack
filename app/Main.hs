{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text
import GHC.Generics
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B
import qualified Network.HTTP.Client as CL
import qualified Data.HashMap.Strict as HM
import Data.Vector (fromList, toList)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
import Data.Aeson.Types 

import Lib
import Config
import ParseJSON

--proxyHttpConfig = defaultHttpConfig { httpConfigProxy = Just (CL.Proxy "187.62.220.89" 8080)}

help :: Result [a] -> [a]
help (Success a) = a
help _ = []

answerTelegram :: (HttpMethod method, HttpBodyAllowed (AllowsBody method) (ProvidesBody NoReqBody)) => 
  method -> [String] -> Req (JsonResponse Value)
answerTelegram method args = do
  let urlHttps = B.pack $ "https://api.telegram.org/bot" ++ tokenTelegram ++ (Prelude.foldr (++) "" args) 
      (url, options) = fromJust $ parseUrlHttps urlHttps
  req method url NoReqBody jsonResponse options

loop :: Integer -> IO ()
loop count = runReq defaultHttpConfig $ do
  js <- answerTelegram GET ["/getUpdates?offset=", show count]
  
  let resParse = help $ parse parseTelegram (responseBody js)
  when (Prelude.null resParse) (liftIO $ loop count)
  
  let (message, update_id, chat_id) = Prelude.head $ help $ parse parseTelegram (responseBody js)
  liftIO $ print $ "info : (" ++ (show message) ++ " , " ++ (show update_id) ++ " , " ++ (show chat_id) ++ ")"

  answerTelegram POST ["/sendMessage?chat_id=", show chat_id, "&text=", message]
  liftIO $ loop (update_id + 1)

main :: IO ()
main = loop 0
  --liftIO $ print $ encode (responseBody js :: Value)-}
