module Scribble.Protocol.Arithmetic where

import Scribble.FSM (class Branch, class Initial, class Terminal, class Receive,
class Select, class Send, kind Role, class RoleName, kind Protocol, class
    ProtocolRoleNames, class ProtocolName) 
import Type.Row (Cons, Nil)
import Data.Void (Void)
import Scribble.Type.SList

foreign import data Arithmetic :: Protocol
instance protocolNameServer :: ProtocolName Arithmetic "Arithmetic"

instance protocolRoleNamesArithmetic :: ProtocolRoleNames Arithmetic ("Server" ::: "Client" ::: SNil)

foreign import data Server :: Role
foreign import data Client :: Role

instance roleNameServer :: RoleName Server "Server"
instance roleNameClient :: RoleName Client "Client"

foreign import data S1 :: Type
foreign import data S2 :: Type
foreign import data S3 :: Type
foreign import data S4 :: Type
foreign import data S5 :: Type
foreign import data S6 :: Type
foreign import data S7 :: Type
foreign import data S8 :: Type

foreign import data C1 :: Type
foreign import data C2 :: Type
foreign import data C3 :: Type
foreign import data C4 :: Type
foreign import data C5 :: Type
foreign import data C6 :: Type
foreign import data C7 :: Type
foreign import data C8 :: Type

instance initialServer :: Initial Server S1
instance terminalServer :: Terminal Server S8

instance initialClient :: Initial Client C1
instance terminalClient :: Terminal Client C8

-- Server states:
instance branchS1 :: Branch Server S1 (Cons "quit" S8 (Cons "add" S5 (Cons "multiply" S2 Nil)))
instance receiveS2 :: Receive Client S2 S3 Int
instance receiveS3 :: Receive Client S3 S4 Int
instance sendS4 :: Send Client S4 S1 Int
instance receiveS5 :: Receive Client S5 S6 Int
instance receiveS6 :: Receive Client S6 S7 Int
instance sendS7 :: Send Client S7 S1 Int

-- Client states:
instance selectC1 :: Select Server C1 (Cons "quit" C8 (Cons "add" C5 (Cons "multiply" C2 Nil)))
instance sendC2 :: Send Server C2 C3 Int
instance sendC3 :: Send Server C3 C4 Int
instance receiveC4 :: Receive Server C4 C1 Int
instance sendC5 :: Send Server C5 C6 Int
instance sendC6 :: Send Server C6 C7 Int
instance receiveC7 :: Receive Server C7 C1 Int
