module DataBaseUsers where

import qualified Data.HashMap.Strict as HM

import Config

type UserId = Integer

type CountRepeat = Integer

type UpdateRepeat = Bool

data UserRepeat = UserRepeat CountRepeat UpdateRepeat

type Users = HM.HashMap UserId UserRepeat


insertUser :: Integer -> Integer -> Users -> Users
insertUser userId repeat users = HM.insert userId (UserRepeat repeat False) users

updateUsers :: Integer -> Users -> Users
updateUsers userId users = HM.adjust update userId users where
    update (UserRepeat countR updateR) = UserRepeat countR True

memberUser :: Integer -> Users -> Bool
memberUser userId users = HM.member userId users

--Returns the count of message repeat for the user
countUserRepeat :: Integer -> Users -> Integer
countUserRepeat userId users = case users HM.! userId of
    UserRepeat countR _ -> countR

--Checks if the user has replays installed
checkRepeat :: Integer -> Users -> Bool
checkRepeat userId users = case users HM.! userId of
    UserRepeat _ update -> update

updateRepeat :: Integer -> Integer -> Users -> Users
updateRepeat userId newRepeat users = HM.adjust update userId users where
    update (UserRepeat countR updateR) = UserRepeat newRepeat False
    

