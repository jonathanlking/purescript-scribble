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
import Prelude (Unit, bind, discard, otherwise, pure, show, ($), (*), (+), (-), (<<<), (<=), (>), min, unit, (<>))
import Data.Functor ((<$>))
import Scribble.Core as SC
import Scribble.Indexed as SI
import Scribble.FSM (Protocol(..), Role(..))
import Scribble.Protocol.Arithmetic.MathServer as MS
import Scribble.WebSocket (WebSocket)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.Aff.AVar as AV

foreign import time :: forall eff. Eff (eff) Number

main = launchAff $ do
   prog 20

-- prog :: forall eff. Int -> Aff (SC.TransportEffects (console :: CONSOLE | eff)) Unit
-- prog n 
--   = SC.session
--         (Proxy :: Proxy WebSocket)
--         (Role :: Role MS.Client)
--         (WS.URL $ "ws://localhost:9161") 
--         (\c -> do
--             t1 <- liftEff $ time
--             (Tuple x c) <- fib n c
--             t2 <- liftEff $ time
--             liftEff $ log $ show $ t2 - t1
--             liftEff $ log $ show x
--             c <- SC.select c (SProxy :: SProxy "quit")
--             SC.send c MS.Quit)
-- 
-- fib :: forall eff. Int -> SC.Channel WebSocket MS.S9 -> Aff (SC.TransportEffects eff) (Tuple Int (SC.Channel WebSocket MS.S9))
-- fib n c
--   | n <= 1    = pure (Tuple 1 c)
--   | otherwise = do
--      (Tuple x c) <- fib (n - 1) c
--      (Tuple y c) <- fib (n - 2) c
--      c <- SC.select c (SProxy :: SProxy "add")
--      c <- SC.send c (MS.Add x y)
--      (Tuple (MS.Sum s) c) <- SC.receive c
--      pure (Tuple s c)

prog :: forall eff. Int -> Aff (SC.TransportEffects (console :: CONSOLE | eff)) Unit
prog n 
  = SI.session
        (Proxy :: Proxy WebSocket)
        (Role :: Role MS.Client)
        (WS.URL $ "ws://localhost:9161") 
        $ do
            t1 <- SI.aff $ liftEff $ time
            x <- fib n
            t2 <- SI.aff $ liftEff $ time
            SI.aff $ liftEff $ log $ show $ t2 - t1
            SI.aff $ liftEff $ log $ show x
            SI.select (SProxy :: SProxy "quit")
            SI.send MS.Quit
  where
    bind = SI.(:>>=)
    pure = (SI.ipure)
    discard = bind

fib :: forall eff. Int -> SI.Session WebSocket (SC.TransportEffects eff) MS.S9 MS.S9 Int
fib n
  | n <= 1    = pure 1
  | otherwise = do
     x <- fib (n - 1)
     y <- fib (n - 2)
     SI.select (SProxy :: SProxy "add")
     SI.send (MS.Add x y)
     MS.Sum s <- SI.receive
     pure s
  where
    bind = SI.(:>>=)
    pure = (SI.ipure)
    discard = bind
