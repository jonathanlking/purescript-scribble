module Scribble.Transport.Memory
  ( Memory
  ) where

import Prelude
import Data.Argonaut.Core (Json)
import Effect.Aff.Class (liftAff)
import Effect.Aff.AVar (AVar, put, take)
import Scribble.Transport (class Transport, class TransportClient, class TransportServer)

newtype Memory = Memory (AVar Json)

instance memoryAvarTransport :: Transport Memory (AVar Json) where
  send (Memory v) x = liftAff $ put x v
  receive (Memory v) = liftAff $ take v
  close = const $ pure unit

instance memoryAvarTransportConnect :: TransportClient Memory (AVar Json) where
  connect = pure <<< Memory

instance memoryAvarTransportAwait :: TransportServer Memory (AVar Json) where
  serve = pure <<< Memory
