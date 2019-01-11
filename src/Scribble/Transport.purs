module Scribble.Transport
  ( class Transport
  , send
  , receive
  , close
  , class TransportClient
  , connect
  , class TransportServer
  , serve
  ) where

import Prelude
import Data.Argonaut.Core (Json)
import Effect.Aff.Class (class MonadAff)

-- | An asynchronous untyped communication layer that sends JSON
-- | A new chanel can be created using parameters p
class Transport c p | c -> p where
  send    :: forall m. MonadAff m => c -> Json -> m Unit
  receive :: forall m. MonadAff m => c -> m Json
  close   :: forall m. MonadAff m => c -> m Unit

-- TODO: This isn't a perfect abstraction as `close` is specific
--       to whether your opened or awaited the connection.

class Transport c p <= TransportClient c p | c -> p where
  connect :: forall m. MonadAff m => p -> m c

class Transport c p <= TransportServer c p | c -> p where
  serve :: forall m. MonadAff m => p -> m c
