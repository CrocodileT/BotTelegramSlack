{-# LANGUAGE OverloadedStrings #-}

module JsonSlack (parseSlack) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Vector (toList)
import Data.Aeson.Types 

parseTextS :: Object -> Parser (Maybe (String, String))
parseTextS obj = do
  text <- obj .: "text"
  ts <- obj .: "ts"
  return $ Just (text, ts)

parseMessageS :: Value -> Parser (Maybe (String, String))
parseMessageS = withObject "object" $ \obj -> do
  case HM.lookup "user" obj of
    Nothing -> return Nothing
    _       -> parseTextS obj

parseArrayS :: Value -> Parser [Maybe (String, String)]
parseArrayS = withArray "parseArray" $ \arr -> filter(/= Nothing) <$> (mapM parseMessageS (toList arr))

parseSlack :: Value -> Parser [Maybe (String, String)]
parseSlack (Object obj) = 
  case HM.lookup "messages" obj of
    Just x -> parseArrayS x
    _      -> fail "failed message"
parseSlack _            = fail "expected an object"