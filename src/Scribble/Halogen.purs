module Scribble.Halogen where

import Scribble.FSM (class Branch, class Initial, class ProtocolName, class ProtocolRoleNames, class Receive, class RoleName, class Select, class Send, class Terminal, Protocol, Role(..))
import Effect.Aff (Aff, delay, finally, attempt)
import Effect.Aff.AVar (AVar, new, empty, put, read, take)
import Data.Time.Duration (Milliseconds(..))
import Control.Monad.Error.Class (throwError)
import Control.Coroutine (Consumer)
import Data.Tuple (Tuple(..))
import Prelude --(class Show, Unit, bind, discard, pure, show, unit, ($), (<$>), (<*>), (<>), (>>=))
import Control.Apply ((*>))
import Type.Row (class ListToRow, Cons, Nil, kind RowList, RLProxy(RLProxy))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Record.Unsafe (unsafeGet, unsafeHas)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Core (Json, fromArray, fromObject, fromString, toObject, toString)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Effect.Class (liftEffect)
import Scribble.Type.SList as SList
import Data.List (List, (:))
import Data.Monoid (mempty)
import Foreign.Object (fromFoldable, lookup)
import Data.Array as Array
import Type.Proxy (Proxy)
import Data.String (toLower)
import Data.List.Types (List(..))
import Scribble.Core
import Halogen.Aff as HA
import Control.Monad.Free.Trans (FreeT)
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.Free.Trans (hoistFreeT)
import Effect.Class.Console (log)
import Data.Either (Either(..), either)
import Effect.Exception (Error, error)
import Control.Monad.Error.Class (throwError)

-- | Designed for a binary session (with direct communication)
halogenSession :: forall r n c ps s t q m eff.
     Transport c ps
  => Initial r s
  => Terminal r t
  => RoleName r n
  => IsSymbol n
  => Proxy c
  -> Role r
  -> ps
  -> (q ~> Aff) 
  -> (Error -> Aff Unit)
  -> (Channel c s -> Consumer m Aff (Channel c t))
  -> Consumer m Aff Unit
halogenSession _ r params query handler prog = do
  (c :: Channel c s) <- lift $ open r params
  c' <- hoistFreeT (paranoid handler) (prog c)
  lift $ close r c'

paranoid :: forall eff. (Error -> Aff Unit) -> Aff ~> Aff
paranoid h x = do
  r <- attempt x
  case r of
    Left  e -> do 
      h e
      throwError e
    Right a -> pure a

-- | Initialise a multiparty session using the proxy server
halogenMultiSession :: forall r rn p pn rns rns' list row s t c ps q m eff.
     RoleName r rn 
  => IsSymbol rn
  => ProtocolName p pn
  => IsSymbol pn
  => ProtocolRoleNames p rns
  => SList.Symbols rns
  => SList.RemoveSymbol rn rns rns'
  => SList.Symbols rns'
  => SList.ToHomoRowList rns' Identifier list
  => SList.RecordKV list row
  => ListToRow list row
  => Initial r s
  => Terminal r t
  => Transport c ps
  => Proxy c
  -> ps
  -> Protocol p
  -> Tuple (Role r) Identifier
  -> Record row
  -> (q ~> Aff)
  -> (Error -> Aff Unit)
  -> (Channel c s -> Consumer m Aff (Channel c t))
  -> Consumer m Aff Unit
halogenMultiSession _ params _ (Tuple r name) ass query handler prog = do
  c@(Channel ch _ _) <- hoistFreeT (paranoid handler) $ lift $ do
    c <- open r params
    let (Channel ch _ _) = c
    uSend ch (encodeReq proxyReq) -- Send our session request params to the proxy
    _ <- uReceive ch -- We receive the role/ident assignment back (safe to ignore for now)
    pure c
  c' <- hoistFreeT (paranoid handler) (prog c)
  lift $ uSend ch (fromString "close")
  _ <- lift $ uReceive ch -- We wait to receive confirmation back
  lift $ close r c'
    where
      role = reflectSymbol (SProxy :: SProxy rn)
      proxyReq = { protocol: Tuple (reflectSymbol (SProxy :: SProxy pn)) (SList.symbols (SList.SLProxy :: SList.SLProxy rns)) 
        , role: role
        , assignment: (Tuple role (show name)) : (SList.getKVs (RLProxy :: RLProxy list) ass)
        }
