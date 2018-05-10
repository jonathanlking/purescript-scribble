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
import Prelude (Unit, bind, discard, otherwise, pure, show, ($), (*), (+), (-), (<<<), (<=), (>), min, unit)
import Data.Functor ((<$>))
import Scribble.Coroutine as SC
import Scribble.FSM (Protocol(..), Role(..))
import Scribble.Protocol.Arithmetic (Arithmetic, C1, Client, S1, S8, Server)
import Scribble.Protocol.Multiparty.TwoBuyer
import Scribble.Transport.WebSocket (WebSocket)
import Type.Proxy (Proxy(..))

main = launchAff $ do
  -- Run the Arithmetic example
  sequential $
    (\ _ _ -> unit)
      <$> parallel (prog 15)
      <*> parallel runServer 
  -- Run the Two-Buyer example
  sequential $
    (\ _ _ _ -> unit) 
      <$> parallel buyer1
      <*> parallel buyer2
      <*> parallel seller

-- Examples for the Two-Buyer protocol

buyer1 :: forall eff. Aff (dom :: DOM, avar :: AVAR | eff) Unit
buyer1 = SC.multiSession
  (Proxy :: Proxy WebSocket)
  (WS.URL $ "ws://127.0.0.1:9160") 
  (Protocol :: Protocol TwoBuyer)
  (Tuple (Role :: Role Buyer1) (SC.Identifier "Jonathan"))
  {"Buyer2": SC.Identifier "Nick", "Seller": SC.Identifier "Nobuko"}
  (\c -> do
    c <- lift $ SC.send c (Book "War and Peace")
    (Tuple (Quote price) c) <- SC.receive c
    -- Buyer1 can contribute a maximum of £10
    let cont = min 10 price
    c <- lift $ SC.send c (Contribution cont)
    SC.choice c { agree: \c -> do
      (Tuple Agree c) <- SC.receive c
      lift $ SC.send c (Transfer cont) 
                , quit: \c -> do
      (Tuple Quit c) <- SC.receive c
      pure c
                }
  )

buyer2 :: forall eff. Aff (dom :: DOM, avar :: AVAR | eff) Unit
buyer2 = SC.multiSession
  (Proxy :: Proxy WebSocket)
  (WS.URL $ "ws://127.0.0.1:9160") 
  (Protocol :: Protocol TwoBuyer)
  (Tuple (Role :: Role Buyer2) (SC.Identifier "Nick"))
  {"Buyer1": SC.Identifier "Jonathan", "Seller": SC.Identifier "Nobuko"}
  (\c -> do 
    (Tuple (Quote price) c) <- SC.receive c
    (Tuple (Contribution amount) c) <- SC.receive c
    let cont = price - amount -- The amount Buyer2 has to contribute
    if cont > 20 -- Buyer 2 will contribute up to £20
      then do
        c <- lift $ SC.select c (SProxy :: SProxy "quit")
        c <- lift $ SC.send c Quit
        lift $ SC.send c Quit
      else do
        c <- lift $ SC.select c (SProxy :: SProxy "agree")
        c <- lift $ SC.send c Agree
        c <- lift $ SC.send c Agree
        lift $ SC.send c (Transfer cont)
  )

seller :: forall eff. Aff (dom :: DOM, avar :: AVAR | eff) Unit
seller = SC.multiSession
  (Proxy :: Proxy WebSocket)
  (WS.URL $ "ws://127.0.0.1:9160") 
  (Protocol :: Protocol TwoBuyer)
  (Tuple (Role :: Role Seller) (SC.Identifier "Nobuko"))
  {"Buyer1": SC.Identifier "Jonathan", "Buyer2": SC.Identifier "Nick"}
  (\c -> do
    (Tuple (Book name) c) <- SC.receive c
    let quote = 30
    c <- lift $ SC.send c (Quote quote)
    c <- lift $ SC.send c (Quote quote)
    SC.choice c { agree: \c -> do
      (Tuple Agree c) <- SC.receive c
      (Tuple (Transfer t1) c) <- SC.receive c
      (Tuple (Transfer t1) c) <- SC.receive c
      pure c
                , quit: \c -> do
      (Tuple Quit c) <- SC.receive c
      pure c
                }
  )

-- Examples for the Arithmetic protocol

prog :: forall eff. Int -> Aff (dom :: DOM, avar :: AVAR, console :: CONSOLE | eff) Unit
prog n 
  = SC.multiSession
        (Proxy :: Proxy WebSocket)
        (WS.URL $ "ws://127.0.0.1:9160") 
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
        (Proxy :: Proxy WebSocket)
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
