module DataBaseUsers where

import qualified Data.HashMap.Strict as HM

import Config

{-Users Data :
  CountRepeat - count of repeat, which installs user
  UpdateRepeat - waiting for a new CountRepeat from the user or not
-}
type CountRepeat = Integer
type UpdateRepeat = Bool
data UserRepeat = UserRepeat CountRepeat UpdateRepeat

{-Users :
  Database for users is a Map, find in the map is O(log(n)) and function insert in Map isn't used not often, 
  so i think this is effectively

  UsersId - key in Map
  UserRepeat - user info
-}
type UserId = Integer
type Users = HM.HashMap UserId UserRepeat


insertUser :: Integer -> Integer -> Users -> Users
insertUser userId repeat users = HM.insert userId (UserRepeat repeat False) users

--set UpdateRepeat, and wait answer of user
waitRepeatUsers :: Integer -> Users -> Users
waitRepeatUsers userId users = HM.adjust update userId users where
    update (UserRepeat countR updateR) = UserRepeat countR True

setNewRepeat :: Integer -> Integer -> Users -> Users
setNewRepeat userId newRepeat users = HM.adjust update userId users where
    update (UserRepeat countR updateR) = UserRepeat newRepeat False

memberUser :: Integer -> Users -> Bool
memberUser userId users = HM.member userId users

returnCountRepeat :: Integer -> Users -> Integer
returnCountRepeat userId users = case users HM.! userId of
    UserRepeat countR _ -> countR

returnUpdateRepeat :: Integer -> Users -> Bool
returnUpdateRepeat userId users = case users HM.! userId of
    UserRepeat _ update -> update

    

