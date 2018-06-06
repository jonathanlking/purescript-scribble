module Main where

import Control.Parallel
import Control.Apply ((<*>))
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.Websocket.WebSocket as WS
import Data.Argonaut.Core (Json)
import Data.Symbol (SProxy(SProxy))
import Data.Tuple (Tuple(..))
import Prelude (Unit, bind, discard, otherwise, pure, show, ($), (*), (+), (-), (<<<), (<=), (>), min, unit, (>>=))
import Data.Functor ((<$>))
import Scribble.Core as SC
import Scribble.FSM (Protocol(..), Role(..))
import Scribble.Protocol.Large.ManyBranches as MB
import Scribble.WebSocket (WebSocket)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

main = pure unit

mbServer :: forall eff. Aff (SC.TransportEffects eff) Unit
mbServer
  = SC.multiSession
        (Proxy :: Proxy WebSocket)
        (WS.URL $ "ws://127.0.0.1:9160") 
        (Protocol :: Protocol MB.ManyBranches) 
        (Tuple (Role :: Role MB.Client)
        (SC.Identifier "Client"))
        {"Server": SC.Identifier "Server"} $ \c -> do
