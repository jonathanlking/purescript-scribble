module Scribble.Transport.WebSocket where

import Scribble.Coroutine

import DOM (DOM)
import DOM.Event.EventTarget as EET
import DOM.Websocket.Event.EventTypes as WSET
import DOM.Websocket.Event.MessageEvent as ME
import DOM.Websocket.WebSocket as WS
import DOM.Websocket.Types (URL)
import Control.Monad.Eff.Class (liftEff)

import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Foreign (F, Foreign, toForeign, readString)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)

import Data.Maybe (Maybe(..))
import Prelude (Unit, const, pure, unit, ($), (<<<))

import Data.Functor ((<$>))
import Control.Monad.Aff (Aff)
import Unsafe.Coerce (unsafeCoerce)
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Parser (jsonParser)

newtype WebSocket = WebSocket WS.WebSocket

instance transportWebSocket :: Transport WebSocket DOM URL where
  uSend (WebSocket ws) x = liftEff $ WS.sendString ws $ stringify x
  uProducer (WebSocket ws) = wsProducer ws
--   uSend (WebSocket ws) x = liftEff $ rename $ WS.sendString ws $ stringify x
--   uProducer (WebSocket ws) = renameT $ wsProducer ws
  uOpen url = unsafeCoerceEff $ WebSocket <$> WS.create url []
  uClose (WebSocket ws) = unsafeCoerceEff $ WS.close ws

-- Slightly safer than unsafeCoerce
rename :: forall m a eff. m (dom :: DOM | eff) a -> m (comm :: DOM | eff) a
rename = unsafeCoerce

renameT :: forall t m a eff. t (m (dom :: DOM | eff)) a -> t (m (comm :: DOM | eff)) a
renameT = unsafeCoerce

-- data EProxy (s :: Symbol) (e :: Effect) = EProxy
-- 
-- coerceName :: forall m eff a b e r. EProxy a e -> EProxy b e -> m (a :: e | eff) r -> m (b :: e | eff) r
-- coerceName = unsafeCoerce

-- | A producer coroutine that emits messages that arrive from the websocket.
-- | From https://github.com/slamdata/purescript-halogen/blob/master/examples/driver-websockets/src/Main.purs
wsProducer
  :: forall eff
   . WS.WebSocket
  -> CR.Producer Json (Aff (avar :: AVAR, dom :: DOM | eff)) Unit
wsProducer socket = CRA.produce \emit ->
  EET.addEventListener
    WSET.onMessage
    (listener emit)
    false
    (WS.socketToEventTarget socket)
  where
  listener emit = EET.eventListener \ev -> do
    for_ (readHelper WS.readMessageEvent ev) \msgEvent ->
      for_ (readHelper readString (ME.data_ msgEvent)) \msg ->
        -- Silently discard all messages which fail to parse to JSON
        either (\e -> pure unit) (emit <<< Left) (jsonParser msg) 
  readHelper :: forall a b. (Foreign -> F a) -> b -> Maybe a
  readHelper read =
    either (const Nothing) Just <<< runExcept <<< read <<< toForeign
