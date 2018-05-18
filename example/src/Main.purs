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
import Scribble.FSM (Protocol(..), Role(..))
import Scribble.Protocol.Arithmetic (Arithmetic, C1, Client, S1, S8, Server)
import Scribble.Protocol.Multiparty.TwoBuyer
import Scribble.WebSocket (WebSocket)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

main = launchAff $ do
  -- Run the Arithmetic example
--  sequential $
--    (\ _ _ -> unit)
--      <$> parallel (prog 15)
--      <*> parallel runServer 
  -- Run the Two-Buyer example
  sequential $
    (\ _ _ _ -> unit)
      <$> parallel buyer1
      <*> parallel buyer2
      <*> parallel seller

-- Examples for the Two-Buyer protocol

buyer1 :: forall eff. Aff (SC.TransportEffects (console :: CONSOLE | eff)) Unit
buyer1 = SC.multiSession
  (Proxy :: Proxy WebSocket)
  (WS.URL $ "ws://127.0.0.1:9160")
  (Protocol :: Protocol TwoBuyer)
  (Tuple (Role :: Role Buyer1) (SC.Identifier "Jonathan"))
  {"Buyer2": SC.Identifier "Nick", "Seller": SC.Identifier "Nobuko"}
  (\c -> do
    c <- SC.send c (Book "War and Peace")
    (Tuple (Quote price) c) <- SC.receive c
    liftEff $ log $ "Buyer1: Quoted £" <> show price
    -- Buyer1 can contribute a maximum of £10
    let cont = min 10 price
    c <- SC.send c (Contribution cont)
    SC.choice c { agree: \c -> do
      (Tuple Agree c) <- SC.receive c
      liftEff $ log $ "Buyer1: Buyer2 agreed!"
      SC.send c (Transfer cont) 
                , quit: \c -> do
      (Tuple Quit c) <- SC.receive c
      liftEff $ log $ "Buyer1: Buyer2 quit!"
      pure c
                }
  )

buyer2 :: forall eff. Aff (SC.TransportEffects (console :: CONSOLE | eff)) Unit
buyer2 = SC.multiSession
  (Proxy :: Proxy WebSocket)
  (WS.URL $ "ws://127.0.0.1:9160")
  (Protocol :: Protocol TwoBuyer)
  (Tuple (Role :: Role Buyer2) (SC.Identifier "Nick"))
  {"Buyer1": SC.Identifier "Jonathan", "Seller": SC.Identifier "Nobuko"}
  (\c -> do
    (Tuple (Quote price) c) <- SC.receive c
    (Tuple (Contribution amount) c) <- SC.receive c
    liftEff $ log $ "Buyer2: Quoted £" <> show price <> ", with Buyer1 offering £" <> show amount
    let cont = price - amount -- The amount Buyer2 has to contribute
    if cont > 20 -- Buyer 2 will contribute up to £20
      then do
        c <- SC.select c (SProxy :: SProxy "quit")
        liftEff $ log $ "Buyer2: Quitting!"
        c <- SC.send c Quit
        SC.send c Quit
      else do
        c <- SC.select c (SProxy :: SProxy "agree")
        liftEff $ log $ "Buyer2: Agreeing!"
        c <- SC.send c Agree
        c <- SC.send c Agree
        SC.send c (Transfer cont)
  )

seller :: forall eff. Aff (SC.TransportEffects (console :: CONSOLE | eff)) Unit
seller = SC.multiSession
  (Proxy :: Proxy WebSocket)
  (WS.URL $ "ws://127.0.0.1:9160")
  (Protocol :: Protocol TwoBuyer)
  (Tuple (Role :: Role Seller) (SC.Identifier "Nobuko"))
  {"Buyer1": SC.Identifier "Jonathan", "Buyer2": SC.Identifier "Nick"}
  (\c -> do
    (Tuple (Book name) c) <- SC.receive c
    liftEff $ log $ "Seller: \"" <> name <> "\" requested"
    let quote = 30
    c <- SC.send c (Quote quote)
    c <- SC.send c (Quote quote)
    SC.choice c { agree: \c -> do
      liftEff $ log $ "Seller: The decided to buy the book!"
      (Tuple Agree c) <- SC.receive c
      (Tuple (Transfer t1) c) <- SC.receive c
      liftEff $ log $ "Seller: Received £" <> show t1 <> " from Buyer 1"
      (Tuple (Transfer t2) c) <- SC.receive c
      liftEff $ log $ "Seller: Received £" <> show t2 <> " from Buyer 2"
      pure c
                , quit: \c -> do
      liftEff $ log $ "Seller: The decided not to buy the book!"
      (Tuple Quit c) <- SC.receive c
      pure c
                }
  )

-- Examples for the Arithmetic protocol

-- prog :: forall eff. Int -> Aff (SC.TransportEffects (console :: CONSOLE | eff)) Unit
-- prog n 
--   = SC.multiSession
--         (Proxy :: Proxy WebSocket)
--         (WS.URL $ "ws://127.0.0.1:9160") 
--         (Protocol :: Protocol Arithmetic) 
--         (Tuple (Role :: Role Client)
--         (SC.Identifier "Jonathan")) 
--         {"Server": SC.Identifier "Nick"} 
--         (\c -> do
--             (Tuple x c) <- fib n c
--             liftEff $ log $ show x
--             SC.select c (SProxy :: SProxy "quit"))
-- 
-- runServer :: forall eff. Aff (SC.TransportEffects eff) Unit
-- runServer
--   = SC.multiSession
--         (Proxy :: Proxy WebSocket)
--         (WS.URL $ "ws://127.0.0.1:9160") 
--         (Protocol :: Protocol Arithmetic) 
--         (Tuple (Role :: Role Server)
--         (SC.Identifier "Nick"))
--         {"Client": SC.Identifier "Jonathan"}
--         server
-- 
-- fib :: forall eff. Int -> SC.Channel WebSocket C1 -> Aff (SC.TransportEffects eff) (Tuple Int (SC.Channel WebSocket C1))
-- fib n c
--   | n <= 1    = pure (Tuple 1 c)
--   | otherwise = do
--      (Tuple x c) <- fib (n - 1) c
--      (Tuple y c) <- fib (n - 2) c
--      c <- SC.select c (SProxy :: SProxy "add")
--      c <- SC.send c x
--      c <- SC.send c y
--      SC.receive c
-- 
-- server :: forall eff. SC.Channel WebSocket S1 -> Aff (SC.TransportEffects eff) (SC.Channel WebSocket S8)
-- server c
--     = SC.choice c { quit: (pure :: _ -> Aff _ _)
--                 , add: (\c -> do
--                     (Tuple x c) <- SC.receive c
--                     (Tuple y c) <- SC.receive c
--                     c <- SC.send c (x + y)
--                     server c)
--                 , multiply: (\c -> do
--                     (Tuple x c) <- SC.receive c
--                     (Tuple y c) <- SC.receive c
--                     c <- SC.send c (x * y)
--                     server c)
--                 }
