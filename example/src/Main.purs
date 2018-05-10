module Main where

import Control.Parallel

import Control.Apply ((<*>))
import Control.Coroutine as CR
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import DOM.Websocket.WebSocket as WS
import Data.Argonaut.Core (Json)
import Data.Symbol (SProxy(SProxy))
import Data.Tuple (Tuple(..))
import Prelude (Unit, bind, discard, otherwise, pure, show, ($), (*), (+), (-), (<<<), (<=))
import Data.Functor ((<$>))
import Scribble.Coroutine as SC
import Scribble.FSM (Protocol(..), Role(..))
import Scribble.Protocol.Arithmetic (Arithmetic, C1, Client, S1, S8, Server)
import Scribble.Transport.WebSocket (WebSocket)

main :: _ 
main = launchAff $ do
  sequential $
    Tuple <$> parallel (prog 15)
          <*> parallel runServer 

prog :: forall eff. Int -> Aff (dom :: DOM, avar :: AVAR, console :: CONSOLE | eff) Unit
prog n 
  = SC.multiSession (WS.URL $ "ws://127.0.0.1:9160") 
        (Protocol :: Protocol Arithmetic) 
        (Tuple (Role :: Role Client)
        (SC.Identifier "Jonathan")) 
        {"Server": SC.Identifier "Nick"} 
        (\c -> do
            (Tuple x c) <- fib n c
            lift $ liftEff $ log $ show x
            lift $ SC.select c (SProxy :: SProxy "quit"))

runServer :: forall eff. Aff (dom :: DOM, avar :: AVAR | eff) Unit
runServer
  = SC.multiSession
        (WS.URL $ "ws://127.0.0.1:9160") 
        (Protocol :: Protocol Arithmetic) 
        (Tuple (Role :: Role Server)
        (SC.Identifier "Nick"))
        {"Client": SC.Identifier "Jonathan"}
        server 

fib :: forall eff. Int -> SC.Channel WebSocket C1 -> CR.Consumer Json (Aff (dom
    :: DOM, avar :: AVAR | eff)) (Tuple Int (SC.Channel WebSocket C1))
fib n c
  | n <= 1    = pure (Tuple 1 c)
  | otherwise = do
     (Tuple x c) <- fib (n - 1) c
     (Tuple y c) <- fib (n - 2) c
     c <- lift $ SC.select c (SProxy :: SProxy "add")
     c <- lift $ SC.send c x
     c <- lift $ SC.send c y
     SC.receive c

server :: forall eff. SC.Channel WebSocket S1 -> CR.Consumer Json (Aff (dom ::
    DOM, avar :: AVAR | eff)) (SC.Channel WebSocket S8)
server c
  = SC.choice c { quit: lift <<< pure
                , add: (\c -> do
                    (Tuple x c) <- SC.receive c
                    (Tuple y c) <- SC.receive c
                    c <- lift $ SC.send c (x + y)
                    server c)
                , multiply: (\c -> do
                    (Tuple x c) <- SC.receive c
                    (Tuple y c) <- SC.receive c
                    c <- lift $ SC.send c (x * y)
                    server c)
                }
