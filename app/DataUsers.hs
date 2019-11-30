module DataUsers where

import qualified Data.HashMap.Strict as HM

import Config

type UserId = Integer

type CountRepeat = Integer

type UpdateRepeat = Bool

data UserRepeat = UserRepeat CountRepeat UpdateRepeat

type Users = HM.HashMap UserId UserRepeat
type TelegramUsers = Users
type SlackUsers = Users
type InfoUsers = (TelegramUsers, SlackUsers)


--sendMessage 

addUsers :: Integer -> Integer -> Users -> Users
addUsers userId repeat users = HM.insert userId (UserRepeat repeat False) users

updateUsers :: Integer -> Users -> Users
updateUsers userId users = HM.adjust update userId users where
    update (UserRepeat countR updateR) = UserRepeat countR True

checkUser :: Integer -> Users -> Bool
checkUser userId users = HM.member userId users

countRepeat :: Integer -> Users -> Integer
countRepeat userId users = case users HM.! userId of
    UserRepeat countR _ -> countR

checkRepeat :: Integer -> Users -> Bool
checkRepeat userId users = case users HM.! userId of
    UserRepeat _ update -> update

updateRepeat :: Integer -> Integer -> Users -> Users
updateRepeat userId newRepeat users = HM.adjust update userId users where
    update (UserRepeat countR updateR) = UserRepeat newRepeat False
    

