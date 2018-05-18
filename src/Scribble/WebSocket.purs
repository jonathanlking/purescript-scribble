module Scribble.WebSocket where

import Scribble.Core

import DOM (DOM)
import DOM.Event.EventTarget as EET
import DOM.Websocket.Event.EventTypes as WSET
import DOM.Websocket.Event.MessageEvent as ME
import DOM.Websocket.WebSocket as WS
import DOM.Websocket.Types (URL)
import Control.Monad.Eff.Class (liftEff)

import Control.Coroutine as CR
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Foreign (F, Foreign, toForeign, readString)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Control.Monad.Error.Class (throwError)

import Data.Maybe (Maybe(..))
import Prelude (Unit, const, pure, unit, ($), (<<<), bind, discard, void, (>>=), (>>>), flip)

import Data.Functor ((<$>))
import Control.Monad.Aff (Aff)
import Unsafe.Coerce (unsafeCoerce)
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Parser (jsonParser)

import Control.Monad.Aff (Aff, delay, launchAff, forkAff)
import Control.Monad.Aff.AVar (AVAR, AVar, makeVar, makeEmptyVar, putVar, readVar, takeVar)
import Data.Time.Duration (Milliseconds(..))

import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Eff.Console (log)
data Status = Open | Closed

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
  socket <- liftEff $ WS.create url []
  -- Add the listener for receiving messages
  liftEff $ EET.addEventListener
    WSET.onMessage
    (receiveListener ibuf)
    false
    (WS.socketToEventTarget socket)
  -- Add the listener for the connection opening
  liftEff $ EET.addEventListener
    WSET.onOpen
    (EET.eventListener \_ -> void $ launchAff $ do
      liftEff $ unsafeCoerceEff $ log "open"
      putVar Open status)
    false
    (WS.socketToEventTarget socket)
  -- Add the listener for the connection closing
  liftEff $ EET.addEventListener
    WSET.onClose
    (EET.eventListener \_ -> void $ launchAff $ do
      liftEff $ unsafeCoerceEff $ log "close"
      modifyVar (const Closed) status
      throwError $ error "Connection closed")
    false
    (WS.socketToEventTarget socket)
  -- Add the listener for a connection error
--  liftEff $ EET.addEventListener
--    WSET.onClose
--    (EET.eventListener \_ -> void $ launchAff $ do
--      liftEff $ unsafeCoerceEff $ log "error"
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
    Open -> liftEff $ WS.sendString ws $ stringify x
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
      liftEff $ WS.close ws
    Closed -> pure unit

instance transportWebSocket :: Transport WebSocket URL where
  uSend = send
  uReceive = receive
  uOpen = open
  uClose = close
