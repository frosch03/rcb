module StreamFunctions where

import Message
import FRP.Yampa

getText :: Message -> Maybe String
getText (Changed _ _ (CF _ ((CFA title text (_, _, _, t, msg)):_)))
    = Just msg
getText _
    = Nothing
    
process :: SF Message (Maybe String)
process = arr $ getText
