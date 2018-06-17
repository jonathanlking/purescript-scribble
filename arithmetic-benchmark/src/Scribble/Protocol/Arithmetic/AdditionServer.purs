module Scribble.Protocol.Arithmetic.AdditionServer where

import Scribble.FSM
import Scribble.Type.SList (type (:::), SLProxy(..), SNil, symbols)
import Type.Row (Cons, Nil)
import Data.Void (Void)

-- From purescript-argonaut-codecs
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Core (Json) -- From purescript-argonaut-core
import Data.Generic.Rep (class Generic) -- From purescript-generics-rep
-- From purescript-argonaut-generic
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)


data Add = Add Int Int
derive instance genericAdd :: Generic Add _
instance encodeJsonAdd :: EncodeJson Add where
  encodeJson = genericEncodeJson
instance decodeJsonAdd :: DecodeJson Add where
  decodeJson = genericDecodeJson
data Res = Res Int
derive instance genericRes :: Generic Res _
instance encodeJsonRes :: EncodeJson Res where
  encodeJson = genericEncodeJson
instance decodeJsonRes :: DecodeJson Res where
  decodeJson = genericDecodeJson
data Bye = Bye
derive instance genericBye :: Generic Bye _
instance encodeJsonBye :: EncodeJson Bye where
  encodeJson = genericEncodeJson
instance decodeJsonBye :: DecodeJson Bye where
  decodeJson = genericDecodeJson

foreign import data AdditionServer :: Protocol

instance protocolNameAdditionServer :: ProtocolName AdditionServer "AdditionServer"

instance protocolRoleNamesAdditionServer :: ProtocolRoleNames AdditionServer ("Client" ::: "Server" ::: SNil)

foreign import data Client :: Role

instance roleNameClient :: RoleName Client "Client"

foreign import data S7 :: Type
foreign import data S7Add :: Type
foreign import data S7Bye :: Type
foreign import data S8 :: Type
foreign import data S9 :: Type

instance initialClient :: Initial Client S7
instance terminalClient :: Terminal Client S8
instance sendS7Add :: Send Server S7Add S9 Add
instance sendS7Bye :: Send Server S7Bye S8 Bye
instance selectS7 :: Select Server S7 (Cons "bye" S7Bye (Cons "add" S7Add Nil))
instance receiveS9 :: Receive Server S9 S7 Res

foreign import data Server :: Role

instance roleNameServer :: RoleName Server "Server"

foreign import data S15 :: Type
foreign import data S15Add :: Type
foreign import data S15Bye :: Type
foreign import data S16 :: Type
foreign import data S17 :: Type

instance initialServer :: Initial Server S15
instance terminalServer :: Terminal Server S16
instance receiveS15Add :: Receive Client S15Add S17 Add
instance receiveS15Bye :: Receive Client S15Bye S16 Bye
instance branchS15 :: Branch Server S15 (Cons "bye" S15Bye (Cons "add" S15Add Nil))
instance sendS17 :: Send Client S17 S15 Res

