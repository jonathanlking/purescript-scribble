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
import Scribble.Protocol.Arithmetic.MathServer as MS
import Scribble.Protocol.Multiparty.TwoBuyer as TB
import Scribble.WebSocket (WebSocket)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

main = launchAff $ do
  -- Run the Arithmetic example
--  sequential $
--    (\ _ _ -> unit)
--      <$> parallel (prog 5)
--      <*> parallel runServer 
--  -- Run the Two-Buyer example
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
  (Protocol :: Protocol TB.TwoBuyer)
  (Tuple (Role :: Role TB.Buyer1) (SC.Identifier "Jonathan"))
  {"Buyer2": SC.Identifier "Nick", "Seller": SC.Identifier "Nobuko"}
  (\c -> do
    c <- SC.send c (TB.Book "War and Peace")
    (Tuple (TB.Quote price) c) <- SC.receive c
    liftEff $ log $ "Buyer1: Quoted £" <> show price
    -- Buyer1 can contribute a maximum of £10
    let cont = min 10 price
    c <- SC.send c (TB.Contribution cont)
    SC.choice c { agree: \c -> do
      (Tuple TB.Agree c) <- SC.receive c
      liftEff $ log $ "Buyer1: Buyer2 agreed!"
      SC.send c (TB.Transfer cont) 
                , quit: \c -> do
      (Tuple TB.Quit c) <- SC.receive c
      liftEff $ log $ "Buyer1: Buyer2 quit!"
      pure c
                }
  )

buyer2 :: forall eff. Aff (SC.TransportEffects (console :: CONSOLE | eff)) Unit
buyer2 = SC.multiSession
  (Proxy :: Proxy WebSocket)
  (WS.URL $ "ws://127.0.0.1:9160")
  (Protocol :: Protocol TB.TwoBuyer)
  (Tuple (Role :: Role TB.Buyer2) (SC.Identifier "Nick"))
  {"Buyer1": SC.Identifier "Jonathan", "Seller": SC.Identifier "Nobuko"}
  (\c -> do
    (Tuple (TB.Quote price) c) <- SC.receive c
    (Tuple (TB.Contribution amount) c) <- SC.receive c
    liftEff $ log $ "Buyer2: Quoted £" <> show price <> ", with Buyer1 offering £" <> show amount
    let cont = price - amount -- The amount Buyer2 has to contribute
    if cont > 20 -- Buyer 2 will contribute up to £20
      then do
        c <- SC.select c (SProxy :: SProxy "quit")
        liftEff $ log $ "Buyer2: TB.Quit!"
        c <- SC.send c TB.Quit
        SC.send c TB.Quit
      else do
        c <- SC.select c (SProxy :: SProxy "agree")
        liftEff $ log $ "Buyer2: Agreeing!"
        c <- SC.send c TB.Agree
        c <- SC.send c TB.Agree
        SC.send c (TB.Transfer cont)
  )

seller :: forall eff. Aff (SC.TransportEffects (console :: CONSOLE | eff)) Unit
seller = SC.multiSession
  (Proxy :: Proxy WebSocket)
  (WS.URL $ "ws://127.0.0.1:9160")
  (Protocol :: Protocol TB.TwoBuyer)
  (Tuple (Role :: Role TB.Seller) (SC.Identifier "Nobuko"))
  {"Buyer1": SC.Identifier "Jonathan", "Buyer2": SC.Identifier "Nick"}
  (\c -> do
    (Tuple (TB.Book name) c) <- SC.receive c
    liftEff $ log $ "Seller: \"" <> name <> "\" requested"
    let quote = 30
    c <- SC.send c (TB.Quote quote)
    c <- SC.send c (TB.Quote quote)
    SC.choice c { agree: \c -> do
      liftEff $ log $ "Seller: The decided to buy the book!"
      (Tuple TB.Agree c) <- SC.receive c
      (Tuple (TB.Transfer t1) c) <- SC.receive c
      liftEff $ log $ "Seller: Received £" <> show t1 <> " from Buyer 1"
      (Tuple (TB.Transfer t2) c) <- SC.receive c
      liftEff $ log $ "Seller: Received £" <> show t2 <> " from Buyer 2"
      pure c
                , quit: \c -> do
      liftEff $ log $ "Seller: The decided not to buy the book!"
      (Tuple TB.Quit c) <- SC.receive c
      pure c
                }
  )

-- Examples for the Arithmetic protocol

prog :: forall eff. Int -> Aff (SC.TransportEffects (console :: CONSOLE | eff)) Unit
prog n 
  = SC.multiSession
        (Proxy :: Proxy WebSocket)
        (WS.URL $ "ws://127.0.0.1:9160") 
        (Protocol :: Protocol MS.MathServer) 
        (Tuple (Role :: Role MS.Client)
        (SC.Identifier "Jonathan")) 
        {"Server": SC.Identifier "Nick"} 
        (\c -> do
            (Tuple x c) <- fib n c
            liftEff $ log $ show x
            c <- SC.select c (SProxy :: SProxy "quit")
            SC.send c MS.Quit)

runServer :: forall eff. Aff (SC.TransportEffects eff) Unit
runServer
  = SC.multiSession
        (Proxy :: Proxy WebSocket)
        (WS.URL $ "ws://127.0.0.1:9160") 
        (Protocol :: Protocol MS.MathServer) 
        (Tuple (Role :: Role MS.Server)
        (SC.Identifier "Nick"))
        {"Client": SC.Identifier "Jonathan"}
        server

fib :: forall eff. Int -> SC.Channel WebSocket MS.S9 -> Aff (SC.TransportEffects eff) (Tuple Int (SC.Channel WebSocket MS.S9))
fib n c
  | n <= 1    = pure (Tuple 1 c)
  | otherwise = do
     (Tuple x c) <- fib (n - 1) c
     (Tuple y c) <- fib (n - 2) c
     c <- SC.select c (SProxy :: SProxy "add")
     c <- SC.send c (MS.Add x y)
     (Tuple (MS.Sum s) c) <- SC.receive c
     pure (Tuple s c)

server :: forall eff. SC.Channel WebSocket MS.S20 -> Aff (SC.TransportEffects eff) (SC.Channel WebSocket MS.S21)
server c
    = SC.choice c {
	quit: (\c -> do
            (Tuple _ c) <- SC.receive c
            pure c)
        , add: (\c -> do
            (Tuple (MS.Add x y) c) <- SC.receive c
            c <- SC.send c (MS.Sum (x + y))
            server c)
        , multiply: (\c -> do
            (Tuple (MS.Multiply x y) c) <- SC.receive c
            c <- SC.send c (MS.Product (x * y))
            server c)
        }
