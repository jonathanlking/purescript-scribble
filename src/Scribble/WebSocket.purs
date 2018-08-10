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
import Foreign (F, Foreign, toForeign, readString)
import Effect.Exception (error)
import Control.Monad.Error.Class (throwError)

import Data.Maybe (Maybe(..))
import Prelude (Unit, const, pure, unit, ($), (<<<), bind, discard, void, (>>=), (>>>), flip)

import Data.Functor ((<$>))
import Unsafe.Coerce (unsafeCoerce)
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Parser (jsonParser)

import Effect.Aff (Aff, delay, launchAff, forkAff)
import Effect.AVar (AVar, makeVar, makeEmptyVar, putVar, readVar, takeVar)
import Data.Time.Duration (Milliseconds(..))

import Effect.Class.Console (log)
data Status = Open | Closed

data URL = URL String

data WebSocket = WebSocket (AVar Status) (AVar Json) WS.WebSocket

-- TODO: Fix!
-- This is an exception-unsafe implementation
type AffAVar e a = Aff (avar :: AVAR | e) a
modifyVar :: forall e a. (a -> a) -> AVar a -> AffAVar e Unit
modifyVar f v = do
  x <- takeVar v
  putVar (f x) v

open :: forall eff. URL -> Aff (dom :: DOM, avar :: AVAR, exception :: EXCEPTION | eff) WebSocket
open url = do
  status <- makeEmptyVar
  ibuf <- makeEmptyVar
  socket <- liftEffect $ WS.create url []
  -- Add the listener for receiving messages
  liftEffect $ EET.addEventListener
    WSET.onMessage
    (receiveListener ibuf)
    false
    (WS.socketToEventTarget socket)
  -- Add the listener for the connection opening
  liftEffect $ EET.addEventListener
    WSET.onOpen
    (EET.eventListener \_ -> void $ launchAff $ do
      liftEffect $ log "open"
      putVar Open status)
    false
    (WS.socketToEventTarget socket)
  -- Add the listener for the connection closing
  liftEffect $ EET.addEventListener
    WSET.onClose
    (EET.eventListener \_ -> void $ launchAff $ do
      liftEffect $ log "close"
      modifyVar (const Closed) status
      throwError $ error "Connection closed")
    false
    (WS.socketToEventTarget socket)
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
      for_ (readHelper WS.readMessageEvent ev) \msgEvent ->
        for_ (readHelper readString (ME.data_ msgEvent)) \msg ->
          either (\e -> pure unit) (void <<< launchAff <<< (flip putVar) ibuf) (jsonParser msg)
    readHelper :: forall a b. (Foreign -> F a) -> b -> Maybe a
    readHelper read =
      either (const Nothing) Just <<< runExcept <<< read <<< toForeign

send :: forall eff. WebSocket -> Json -> Aff (dom :: DOM, avar :: AVAR, exception :: EXCEPTION | eff) Unit
send c@(WebSocket sv _ ws) x = do
  status <- readVar sv
  case status of
    Open -> liftEffect $ WS.sendString ws $ stringify x
    Closed -> throwError $ error "Channel is closed"

receive :: forall eff. WebSocket -> Aff (dom :: DOM, avar :: AVAR, exception :: EXCEPTION | eff) Json
receive c@(WebSocket sv ibuf _) = do
  status <- readVar sv
  case status of
    Open -> takeVar ibuf 
    Closed -> throwError $ error "Channel is closed"

close :: forall eff. WebSocket -> Aff (dom :: DOM, avar :: AVAR, exception :: EXCEPTION | eff) Unit
close (WebSocket sv _ ws) = do
  status <- readVar sv
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
