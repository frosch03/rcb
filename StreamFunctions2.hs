module StreamFunctions2 where

import Message
import FRP.Yampa
import FRP.Yampa.Loop

getText :: Message -> Maybe (String, String)
getText (Changed _ _ (CF _ ((CFA title text (_, rid, _, t, msg)):_)))
    = Just (rid, msg)
getText _
    = Nothing
    
process :: SF String (Maybe String)
-- process = arr $ Just
-- process = delay 1 "" >>> changed
process = changed

changed :: SF String (Maybe String)
changed =
    loopPre "" (arr isNew)

isNew :: Eq a => (a, a) -> (Maybe a, a)
isNew (x, y) = (x', x)
    where
      x' = if (x == y)
           then Nothing
           else Just x
