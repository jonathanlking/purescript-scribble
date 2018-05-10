module Scribble.Protocol.Multiparty.TwoBuyer where

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


data Quote = Quote Int
derive instance genericQuote :: Generic Quote _
instance encodeJsonQuote :: EncodeJson Quote where
  encodeJson = genericEncodeJson
instance decodeJsonQuote :: DecodeJson Quote where
  decodeJson = genericDecodeJson
data Book = Book String
derive instance genericBook :: Generic Book _
instance encodeJsonBook :: EncodeJson Book where
  encodeJson = genericEncodeJson
instance decodeJsonBook :: DecodeJson Book where
  decodeJson = genericDecodeJson
data Agree = Agree
derive instance genericAgree :: Generic Agree _
instance encodeJsonAgree :: EncodeJson Agree where
  encodeJson = genericEncodeJson
instance decodeJsonAgree :: DecodeJson Agree where
  decodeJson = genericDecodeJson
data Transfer = Transfer Int
derive instance genericTransfer :: Generic Transfer _
instance encodeJsonTransfer :: EncodeJson Transfer where
  encodeJson = genericEncodeJson
instance decodeJsonTransfer :: DecodeJson Transfer where
  decodeJson = genericDecodeJson
data Quit = Quit
derive instance genericQuit :: Generic Quit _
instance encodeJsonQuit :: EncodeJson Quit where
  encodeJson = genericEncodeJson
instance decodeJsonQuit :: DecodeJson Quit where
  decodeJson = genericDecodeJson
data Contribution = Contribution Int
derive instance genericContribution :: Generic Contribution _
instance encodeJsonContribution :: EncodeJson Contribution where
  encodeJson = genericEncodeJson
instance decodeJsonContribution :: DecodeJson Contribution where
  decodeJson = genericDecodeJson

foreign import data TwoBuyer :: Protocol

instance protocolNameTwoBuyer :: ProtocolName TwoBuyer "TwoBuyer"

instance protocolRoleNamesTwoBuyer :: ProtocolRoleNames TwoBuyer ("Buyer1" ::: "Buyer2" ::: "Seller" ::: SNil)

foreign import data Buyer2 :: Role

instance roleNameBuyer2 :: RoleName Buyer2 "Buyer2"

foreign import data S23 :: Type
foreign import data S25 :: Type
foreign import data S26 :: Type
foreign import data S26Agree :: Type
foreign import data S26Quit :: Type
foreign import data S27 :: Type
foreign import data S29 :: Type
foreign import data S24 :: Type
foreign import data S28 :: Type

instance initialBuyer2 :: Initial Buyer2 S23
instance terminalBuyer2 :: Terminal Buyer2 S24
instance receiveS23 :: Receive Seller S23 S25 Quote
instance receiveS25 :: Receive Buyer1 S25 S26 Contribution
instance sendS26Agree :: Send Buyer1 S26Agree S27 Agree
instance sendS26Quit :: Send Buyer1 S26Quit S29 Quit
instance selectS26 :: Select Buyer1 S26 (Cons "quit" S26Quit (Cons "agree" S26Agree Nil))
instance sendS27 :: Send Seller S27 S28 Agree
instance sendS29 :: Send Seller S29 S24 Quit
instance sendS28 :: Send Seller S28 S24 Transfer

foreign import data Buyer1 :: Role

instance roleNameBuyer1 :: RoleName Buyer1 "Buyer1"

foreign import data S9 :: Type
foreign import data S11 :: Type
foreign import data S12 :: Type
foreign import data S13 :: Type
foreign import data S13Agree :: Type
foreign import data S13Quit :: Type
foreign import data S10 :: Type
foreign import data S14 :: Type

instance initialBuyer1 :: Initial Buyer1 S9
instance terminalBuyer1 :: Terminal Buyer1 S10
instance sendS9 :: Send Seller S9 S11 Book
instance receiveS11 :: Receive Seller S11 S12 Quote
instance sendS12 :: Send Buyer2 S12 S13 Contribution
instance receiveS13Agree :: Receive Buyer2 S13Agree S14 Agree
instance receiveS13Quit :: Receive Buyer2 S13Quit S10 Quit
instance branchS13 :: Branch Buyer1 S13 (Cons "agree" S13Agree (Cons "quit" S13Quit Nil))
instance sendS14 :: Send Seller S14 S10 Transfer

foreign import data Seller :: Role

instance roleNameSeller :: RoleName Seller "Seller"

foreign import data S38 :: Type
foreign import data S40 :: Type
foreign import data S41 :: Type
foreign import data S42 :: Type
foreign import data S42Agree :: Type
foreign import data S42Quit :: Type
foreign import data S39 :: Type
foreign import data S43 :: Type
foreign import data S44 :: Type

instance initialSeller :: Initial Seller S38
instance terminalSeller :: Terminal Seller S39
instance receiveS38 :: Receive Buyer1 S38 S40 Book
instance sendS40 :: Send Buyer1 S40 S41 Quote
instance sendS41 :: Send Buyer2 S41 S42 Quote
instance receiveS42Agree :: Receive Buyer2 S42Agree S43 Agree
instance receiveS42Quit :: Receive Buyer2 S42Quit S39 Quit
instance branchS42 :: Branch Seller S42 (Cons "agree" S42Agree (Cons "quit" S42Quit Nil))
instance receiveS43 :: Receive Buyer1 S43 S44 Transfer
instance receiveS44 :: Receive Buyer2 S44 S39 Transfer

