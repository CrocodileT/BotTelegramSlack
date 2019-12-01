module Config where

import Tokens

defaultRepeat = 1    
tokenTelegram = tokenTg
messageHelp = "Commands: /help, /repeat"
messageRepeat = "You can change count of repeat message since 1 to 5.\n Command repeat installed default ="
messageSlackRepeat = "You can change count of repeat message since 1 to 5.\n Entger number or emoji: :one:,:two:,:three:,:four:,:five:"
badMessage = "You used bad char"
successMessage = "New repeat : "

tokenBootSlack = "?token=" ++ tokenSl
tokenChannelTest = tokenChat
tokenSlack = "?token=" ++ tokenChannelTest
idChannelTest = "&channel=" ++ idChat ++ "&"

