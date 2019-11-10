{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Lib
import Config

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
--import Data.HashMap.Strict (fromList)

--import qualified Text.URI as URI

--{-# LANGUAGE OverloadedStrings #-}


--{-# LANGUAGE DeriveGeneric     #-}
{-import Network.HTTP.Client
import Network.HTTP.Client.TLS   (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    request <- parseRequest $ "http://api.telegram.org/bot" ++ token ++ "/getUpdates"
    response <- httpLbs request manager

    putStrLn $ "The status code was: " ++
               show (statusCode $ responseStatus response)
    print $ responseBody response
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B
--import qualified Text.URI as URI-}


token :: Text
token = pack $ "bot" ++ tokenTelegram

--proxyHttpConfig = defaultHttpConfig { httpConfigProxy = Just (CL.Proxy "187.62.220.89" 8080)}

help :: Result [a] -> [a]
help (Success a) = a
help _ = []

main :: IO ()
main = runReq defaultHttpConfig $ do
  js <- req GET (https "api.telegram.org" /: token /: "getUpdates") NoReqBody jsonResponse mempty
  
  let (message, num, id) = Prelude.last $ help $ parse parseTelegram (responseBody js :: Value)
      method = "/sendMessage?chat_id=" ++ (show id) ++ "&text=test"
      (url, options) = fromJust (parseUrlHttps $ B.pack $ "https://api.telegram.org/bot" ++ tokenTelegram ++ method)
  --liftIO $ print url
  answ <- req POST url NoReqBody jsonResponse options
  liftIO $ print $ encode (responseBody answ ::Value)
   
  --liftIO $ print $ encode (responseBody js :: Value)-}

f = withObject "object" $ \obj -> do

  text <- case HM.lookup "text" obj of
    Just x -> parseJSON x
    _      -> fail "field text"

  message_id <- case HM.lookup "message_id" obj of
    Just x -> parseJSON x
    _      -> fail "field message_id"

  chat_id <- case HM.lookup "chat" obj of
    Just x -> g x
    _      -> fail "field chat_id"

  return (text, message_id, chat_id)

g = withObject "object" $ \obj -> do
  id <- case HM.lookup "id" obj of
    Just x -> parseJSON x
    _      -> fail "field id"
  return id

parseTuple = withObject "object" $ \obj -> do
 
  message <- case HM.lookup "message" obj of
    Just x -> parseJSON x
    _      -> fail "field message"
  
  f message



parseArray :: Value -> Parser [(String, Integer, Integer)]
parseArray (Array arr) = mapM parseTuple (toList arr)
parseArray _           = fail "expected an array"


parseTelegram :: Value -> Parser [(String, Integer, Integer)]
parseTelegram (Object obj) = 
  case HM.lookup "result" obj of
    Just x -> parseArray x
    _      -> fail "failed result"
parseTelegram _            = fail "expected an object"

{-main :: IO ()
main = do
  s <- T.encodeUtf8 <$> T.getLine
  --js <- decode s
  liftIO $ print $ (decode s :: Maybe Value)-}

 -- {"name":"Nightfall", "author":{ "name":"Isaac Asimov", "born":1920 } }

{-
 Object (fromList [
   ("ok",Bool True),
   ("result",Array [Object (fromList [
     ("update_id",Number 8.45400663e8),
     ("message",Object (fromList [
       ("entities",Array [Object (fromList [
         ("length",Number 5.0),
         ("offset",Number 0.0),
         ("type",String "bot_command")])]),
       ("text",String "/help"),
       ("from",Object (fromList [
         ("first_name",String "\1043\1088\1080\1096\1072"),
         ("username",String "Gmihtt"),
         ("is_bot",Bool False),
         ("id",Number 3.8187354e8),
         ("language_code",String "ru")])),
       ("chat",Object (fromList [
         ("first_name",String "\1043\1088\1080\1096\1072"),
         ("username",String "Gmihtt"),
         ("id",Number 3.8187354e8),
         ("type",String "private")])),
       ("message_id",Number 6.0),
       ("date",Number 1.573174483e9)]))]),

                  Object (fromList [
     ("update_id",Number 8.45400664e8),
     ("message",Object (fromList [
       ("entities",Array [Object (fromList [
         ("length",Number 7.0),
         ("offset",Number 0.0),
         ("type",String "bot_command")])]),
       ("text",String "/repeat"),
       ("from",Object (fromList [
         ("first_name",String "\1043\1088\1080\1096\1072"),
         ("username",String "Gmihtt"),
         ("is_bot",Bool False),
         ("id",Number 3.8187354e8),
         ("language_code",String "ru")])),
       ("chat",Object (fromList [
         ("first_name",String "\1043\1088\1080\1096\1072"),
         ("username",String "Gmihtt"),
         ("id",Number 3.8187354e8),
         ("type",String "private")])),
       ("message_id",Number 7.0),
       ("date",Number 1.573257658e9)]))])])])
-}