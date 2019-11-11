{-# LANGUAGE OverloadedStrings #-}

module ParseJSON where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Vector (fromList, toList)
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

parseMessage :: Value -> Parser (String, Integer, Integer)
parseMessage = withObject "object" $ \obj -> do
  message <- obj .: "message" 
  update_id <- obj .: "update_id"
  (text, chat_id) <- parseChat message
  return (text, update_id, chat_id)

parseArray :: Value -> Parser [(String, Integer, Integer)]
parseArray = withArray "parseArray" $ \arr -> mapM parseMessage (toList arr)

parseTelegram :: Value -> Parser [(String, Integer, Integer)]
parseTelegram (Object obj) = 
  case HM.lookup "result" obj of
    Just x -> parseArray x
    _      -> fail "failed result"
parseTelegram _            = fail "expected an object"


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