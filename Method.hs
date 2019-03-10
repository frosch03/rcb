module Method
where

import Aux
import Algo
import Ascii  

data Method
    = Login String String Algo
    | Logout
    deriving (Eq)

instance Ascii (Method) where
    ascii (Login usr pwd SHA256) =
        "\"method\": \"login\",\"params\":[{\"user\": { \"username\": " ++ (show usr) ++
        " },\"password\": {\"digest\": " ++ (show . sha256hash $ pwd) ++
        ",\"algorithm\":\"" ++ (ascii SHA256) ++
        "\"}}]"

instance Show (Method) where
    show (Login usr pwd _) =
        "Login: " ++ usr ++ "(" ++ hidePw pwd ++ ")"

