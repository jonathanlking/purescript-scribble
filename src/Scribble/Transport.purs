module Scribble.Transport
  ( class Transport
  , send
  , receive
  , open
  , close
  ) where

import Prelude
import Data.Argonaut.Core (Json)
import Effect.Aff.Class (class MonadAff)

-- | An asynchronous untyped communication layer that sends JSON
-- | A new chanel can be created using parameters p
class Transport c p | c -> p where
  send    :: forall m. MonadAff m => c -> Json -> m Unit
  receive :: forall m. MonadAff m => c -> m Json
  open    :: forall m. MonadAff m => p -> m c
  close   :: forall m. MonadAff m => c -> m Unit
