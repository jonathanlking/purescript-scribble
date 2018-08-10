module Scribble.Halogen where

import Scribble.FSM (class Initial, class ProtocolName, class ProtocolRoleNames, class RoleName, class Terminal, Protocol, Role)
import Effect.Aff (Aff, attempt)
import Control.Monad.Error.Class (throwError)
import Control.Coroutine (Consumer)
import Data.Tuple (Tuple(..))
import Prelude --(class Show, Unit, bind, discard, pure, show, unit, ($), (<$>), (<*>), (<>), (>>=))
import Type.Row (class ListToRow, kind RowList, RLProxy(RLProxy))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Core (fromString)
import Scribble.Type.SList as SList
import Data.List ((:))
import Type.Proxy (Proxy)
import Scribble.Core (class Transport, Channel(..), Identifier, close, encodeReq, open, uReceive, uSend)
import Control.Monad.Free.Trans (hoistFreeT)
import Data.Either (Either(..))
import Effect.Exception (Error)

-- | Designed for a binary session (with direct communication)
halogenSession :: forall r n c ps s t q m.
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

paranoid :: (Error -> Aff Unit) -> Aff ~> Aff
paranoid h x = do
  r <- attempt x
  case r of
    Left  e -> do 
      h e
      throwError e
    Right a -> pure a

-- | Initialise a multiparty session using the proxy server
halogenMultiSession :: forall r rn p pn rns rns' list row s t c ps q m.
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
