module System.RCB.IAscii
where

import System.RCB.Auxiliary
import Data.RocketChat.Message.Datatype
import Data.RocketChat.Message.Method
import Data.RocketChat.Message.Algo

import Data.List (intercalate)

-- Interface Class
class Ascii a where
    ascii :: a -> String

-- Instances
-- Method instance
instance Ascii (Method) where
    ascii (Login id usr pwd SHA256) =
        "\"method\":\"login\",\"id\":\"" ++ show id ++
        "\",\"params\":[{\"user\":{\"username\":" ++ (show usr) ++
        "},\"password\":{\"digest\":" ++ (show . sha256hash $ pwd) ++
        ",\"algorithm\":\"" ++ (ascii SHA256) ++
        "\"}}]"
    ascii (SendMsg id msgs) =
        "\"method\":\"sendMessage\",\"id\":\"" ++ show id ++
        "\",\"params\":[" ++
        (intercalate "," . map fn $ msgs) ++ "]"
            where
              fn (i,r,m) = "{\"_id\":\"" ++ i ++ "\",\"rid\":\"" ++ r ++ "\",\"msg\":\"" ++ m ++ "\"}"

-- Algo instance
instance Ascii (Algo) where
    ascii SHA256 = "sha-256"
                           
-- Message instance
instance Ascii (Message) where
    ascii (Pong) =
        "{\"msg\": \"pong\"}"
    ascii (Con ver sup) =
        "{\"msg\": \"connect\", \"version\":\"" ++ (show ver) ++
        "\",\"support\": [\"" ++ (show sup) ++ "\"]}"
    ascii (Mtd mtd) =
        "{\"msg\": \"method\"," ++ (ascii mtd) ++ "}"
    ascii (Nosub id) =
        "{\"msg\":\"nosub\",\"id\":\"" ++ show id ++ "\"}"
    ascii (SubStreamNotifyUser id user) =
        "{\"msg\": \"sub\",\"id\": \"" ++ show id ++
        "\",\"name\": \"stream-notify-user\",\"params\":[\"" ++ user ++ "/notification\",false]}"
