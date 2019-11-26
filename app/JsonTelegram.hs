{-# LANGUAGE OverloadedStrings #-}

module JsonTelegram (parseTelegram) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Vector (toList)
import Data.Aeson.Types 

parseChat :: Value -> Parser (String, Integer)
parseChat = withObject "object" $ \obj -> do
  text <- obj .: "text"
  chat_id <- case HM.lookup "chat" obj of
    Just chat -> parseId chat
    _      -> fail "field chat_id"

  return (text, chat_id)

parseId :: Value -> Parser Integer
parseId = withObject "object" $ \obj -> do
  id <- obj .: "id"
  return id

parseMessageT :: Value -> Parser (String, Integer, Integer)
parseMessageT = withObject "object" $ \obj -> do
  message <- obj .: "message" <|> obj .: "edited_message"

  update_id <- obj .: "update_id"
  (text, chat_id) <- parseChat message
  return (text, update_id, chat_id)

parseArrayT :: Value -> Parser [(String, Integer, Integer)]
parseArrayT = withArray "parseArray" $ \arr -> mapM parseMessageT (toList arr)

parseTelegram :: Value -> Parser [(String, Integer, Integer)]
parseTelegram (Object obj) = 
  case HM.lookup "result" obj of
    Just x -> parseArrayT x
    _      -> fail "failed result"
parseTelegram _            = fail "expected an object"