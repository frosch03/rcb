> module System.RCB.Configuration where
> import Network.Socket (PortNumber)
> import Data.RocketChat.Message.Datatype
> import Data.RocketChat.Message.Method
> import Data.RocketChat.Message.Algo


This is the configuration file of the haskell rocket chat client.

First of all the server connection one will connect to needs to be
 defined:

> rct_server = "c.frosch03.de"
> rct_port   = 443 :: PortNumber
> rct_path   = "/websocket/"
     
With the rct_username the username of the user to be used is defined

> rct_username = "lambdabot"


The password is stored within the rct_password variable

> rct_password = "1xunil3"


Another userfull value for now is the user id of the user one want's
to subscribe to

> rct_subscribe_to = "dNfBQiWGorDmHwWXR"


> init_string  = Con 1 1 
> login_string = Mtd (Login 2 rct_username rct_password SHA256)
> subscribe    = SubStreamNotifyUser 3 rct_subscribe_to
> unsubscribe  = Nosub 3
