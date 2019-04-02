module StreamFunctions where

import Message
import FRP.Yampa

getText :: Message -> Maybe (String, String)
getText (Changed _ _ (CF _ ((CFA title text (_, rid, _, t, msg)):_)))
    = Just (rid, msg)
getText _
    = Nothing
    
process :: SF Message (Maybe (String, String))
process = arr $ getText
