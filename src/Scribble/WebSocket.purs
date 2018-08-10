module Scribble.WebSocket where

import Scribble.Core

import Web.Event.EventTarget as EET
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.WebSocket as WS
import Effect.Class (liftEffect)

import Control.Coroutine as CR
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Foreign (F, Foreign, unsafeToForeign, readString)
import Effect.Exception (error)
import Control.Monad.Error.Class (throwError)

import Data.Maybe (Maybe(..))
import Prelude (Unit, const, pure, unit, ($), (<<<), bind, discard, void, (>>=), (>>>), flip)

import Data.Functor ((<$>))
import Unsafe.Coerce (unsafeCoerce)
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Parser (jsonParser)

import Effect.Aff (Aff, delay, launchAff, forkAff)
import Effect.Aff.AVar (AVar, new, empty, put, read, take)
import Data.Time.Duration (Milliseconds(..))

import Effect.Class.Console (log)
data Status = Open | Closed

data URL = URL String

data WebSocket = WebSocket (AVar Status) (AVar Json) WS.WebSocket

-- TODO: Fix!
-- This is an exception-unsafe implementation
modifyVar :: forall e a. (a -> a) -> AVar a -> Aff Unit
modifyVar f v = do
  x <- take v
  put (f x) v

open :: forall eff. URL -> Aff WebSocket
open (URL url) = do
  status <- empty
  ibuf <- empty
  socket <- liftEffect $ WS.create url []
  -- Add the listener for receiving messages
  liftEffect $ do
    el <- (receiveListener ibuf)
    EET.addEventListener
      WSET.onMessage
      el
      false
      (WS.toEventTarget socket)
  -- Add the listener for the connection opening
  liftEffect $ do
    el <- (EET.eventListener \_ -> void $ launchAff $ do
        liftEffect $ log "open"
        put Open status)
    EET.addEventListener
      WSET.onOpen
      el
      false
      (WS.toEventTarget socket)
  -- Add the listener for the connection closing
  liftEffect $ do
    el <- (EET.eventListener \_ -> void $ launchAff $ do
        liftEffect $ log "close"
        modifyVar (const Closed) status
        throwError $ error "Connection closed")
    EET.addEventListener
      WSET.onClose
      el
      false
      (WS.toEventTarget socket)
  -- Add the listener for a connection error
--  liftEffect $ EET.addEventListener
--    WSET.onClose
--    (EET.eventListener \_ -> void $ launchAff $ do
--      liftEffect $ log "error"
--      modifyVar (const Closed) status
--      throwError $ error "Connection closed")
--    false
--    (WS.socketToEventTarget socket)
  pure $ WebSocket status ibuf socket
    where
    receiveListener ibuf = EET.eventListener \ev -> do
      for_ (ME.fromEvent ev) \msgEvent ->
        for_ (readHelper readString (ME.data_ msgEvent)) \msg ->
          either (\e -> pure unit) (void <<< launchAff <<< (flip put) ibuf) (jsonParser msg)
    readHelper :: forall a b. (Foreign -> F a) -> b -> Maybe a
    readHelper read =
      either (const Nothing) Just <<< runExcept <<< read <<< unsafeToForeign

send :: forall eff. WebSocket -> Json -> Aff Unit
send c@(WebSocket sv _ ws) x = do
  status <- read sv
  case status of
    Open -> liftEffect $ WS.sendString ws $ stringify x
    Closed -> throwError $ error "Channel is closed"

receive :: forall eff. WebSocket -> Aff Json
receive c@(WebSocket sv ibuf _) = do
  status <- read sv
  case status of
    Open -> take ibuf 
    Closed -> throwError $ error "Channel is closed"

close :: forall eff. WebSocket -> Aff Unit
close (WebSocket sv _ ws) = do
  status <- read sv
  case status of
    Open -> do 
      modifyVar (const Closed) sv 
      liftEffect $ WS.close ws
    Closed -> pure unit

instance transportWebSocket :: Transport WebSocket URL where
  uSend = send
  uReceive = receive
  uOpen = open
  uClose = close
