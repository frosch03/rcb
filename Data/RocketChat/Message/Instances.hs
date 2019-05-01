-------------------------------------------------------------------------------
-- | Module      :  Data.RocketChat.Message.Instances
--   Copyright   :  (c) Matthias Brettschneider 2019
--   License     :  as-is
--
--   Maintainer  :  frosch03@gmail.com
--   Stability   :  unstable
--   Portability :  unportable
--
-------------------------------------------------------------------------------

module Data.RocketChat.Message.Instances
where

import Data.RocketChat.Message.Datatype
import Data.RocketChat.Message.Parser

import Text.ParserCombinators.Parsec

instance Read (Message) where
    readsPrec p s = case parse pMessage "" s of
                      Left s  -> error $ "error while parsing Message / " ++ (show s)
                      Right x -> [(x, "")]


instance Show (Message) where
    show (Ping) =
        "PING"
    show (Pong) =
        "PONG"
    show (Con v s) =
        "Connect (version: " ++ show v ++ ", support: " ++ show s ++ ")"
    show (Mtd m) =
        "METHOD " ++ (show m)
    show (SubStreamNotifyUser id user) =
        "SUBSCRIBE USER NOTIFICATIONS (id: " ++ show id ++ ", userid: " ++ user ++")"
-- --------------
    show (Error reason) =
        "ERROR (" ++ reason ++ ")"
    show (Server id) =
        "SERVER (ID:" ++ show id ++ ")"
    show (Connected s) =
        "CONNECTED (Session:" ++ s ++ ")"
    show (Ready subs) =
        "READY (Subs:" ++ show subs ++ ")"
    show (Updated methods) =
        "UPDATED (Methods:" ++ show methods ++ ")"
    show (Nosub i) =
        "UNSUBSCRIBE (id: " ++ show i ++ ")"
    show (Added collection id fields) =
        "ADDED (Collection: " ++ collection ++ ", Id: " ++ id ++ ", " ++ show fields ++ ")"
    show (Result id result) =
        "RESULT (Id: " ++ (show id) ++ ", " ++ (show result) ++ ")"
    show (Changed collection id cf) =
        "CHANGED: " ++ (show collection) ++ " (ID: " ++ id ++ ", " ++ show cf ++")"
