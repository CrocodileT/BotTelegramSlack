{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module JsonTelegram (parseTelegram, buttons) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Vector (toList)
import Data.Aeson.Types 
import GHC.Generics
import qualified Data.Text as T

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


key1, key2, key3, key4, key5 :: KeyButton
key1 = KeyButton "1"
key2 = KeyButton "2"
key3 = KeyButton "3"
key4 = KeyButton "4"
key5 = KeyButton "5"

buttons :: ReplyKey
buttons = ReplyKey [[key1 , key2 , key3 ,key4 , key5]]

data KeyButton = KeyButton {
  text :: T.Text } deriving (Generic, Show)

data ReplyKey = ReplyKey {
  keyboard :: [[KeyButton]] } deriving (Generic, Show)

instance ToJSON KeyButton
instance FromJSON KeyButton

instance ToJSON ReplyKey
instance FromJSON ReplyKey