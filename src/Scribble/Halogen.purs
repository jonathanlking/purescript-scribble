module Scribble.Halogen where

import Scribble.FSM (class Branch, class Initial, class ProtocolName, class ProtocolRoleNames, class Receive, class RoleName, class Select, class Send, class Terminal, Protocol, Role(..))
import Control.Monad.Aff (Aff, delay, finally, attempt)
import Control.Monad.Aff.AVar (AVAR, AVar, makeVar, putVar, tryTakeVar, takeVar)
import Data.Time.Duration (Milliseconds(..))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Control.Coroutine (Consumer)
import Data.Tuple (Tuple(..))
import Prelude --(class Show, Unit, bind, discard, pure, show, unit, ($), (<$>), (<*>), (<>), (>>=))
import Control.Apply ((*>))
import Control.Monad.Eff (kind Effect, Eff)
import Type.Row (class ListToRow, Cons, Nil, kind RowList, RLProxy(RLProxy))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Record.Unsafe (unsafeGet, unsafeHas)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Core (Json, fromArray, fromObject, fromString, toObject, toString)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Control.Monad.Eff.Class (liftEff)
import Scribble.Type.SList as SList
import Data.List (List, (:))
import Data.Monoid (mempty)
import Data.StrMap (fromFoldable, lookup)
import Data.Array as Array
import Type.Proxy (Proxy)
import Data.String (toLower)
import Data.List.Types (List(..))
import Scribble.Core
import Halogen.Aff as HA
import Control.Monad.Free.Trans (FreeT)
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.Free.Trans (hoistFreeT)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Eff.Console (log)
import Data.Either (Either(..), either)
import Control.Monad.Eff.Exception (EXCEPTION, Error, error)
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
  -> (q ~> Aff (TransportEffects (eff))) 
  -> (Error -> Aff (TransportEffects eff) Unit)
  -> (Channel c s -> Consumer m (Aff (TransportEffects (eff))) (Channel c t))
  -> Consumer m (Aff (TransportEffects (eff))) Unit
halogenSession _ r params query handler prog = do
  (c :: Channel c s) <- lift $ open r params
  c' <- hoistFreeT (paranoid handler) (prog c)
  lift $ close r c'

paranoid :: forall eff. (Error -> Aff (TransportEffects eff) Unit) -> Aff (TransportEffects eff) ~> Aff (TransportEffects eff)
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
  -> (q ~> Aff (TransportEffects eff))
  -> (Error -> Aff (TransportEffects eff) Unit)
  -> (Channel c s -> Consumer m (Aff (TransportEffects (eff))) (Channel c t))
  -> Consumer m (Aff (TransportEffects eff)) Unit
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
