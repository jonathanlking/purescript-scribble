module Scribble.Coroutine where

import Scribble.FSM (class Branch, class Initial, class ProtocolName, class ProtocolRoleNames, class Receive, class RoleName, class Select, class Send, class Terminal, Protocol, Role(..))
import Control.Monad.Aff (Aff, delay)
import Control.Monad.Aff.AVar (AVAR, AVar, makeVar', tryTakeVar)
import Data.Time.Duration (Milliseconds(..))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Exception (error)
import Control.Coroutine as CR
import Data.Tuple (Tuple(..))
import Prelude (class Show, Unit, bind, discard, pure, show, unit, ($), (<$>), (<>))
import Control.Monad.Eff (kind Effect, Eff)
import Type.Row (class ListToRow, Cons, Nil, kind RowList, RLProxy(RLProxy))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Record.Unsafe (unsafeGet, unsafeHas)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Core (Json, fromArray, fromObject, fromString)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Control.Monad.Eff.Class (liftEff)
import Scribble.Type.SList as SList
import Data.List (List, (:))
import Data.Monoid (mempty)
import Data.StrMap (fromFoldable)
import Data.Array as Array

-- | An asynchronous untyped communication layer
-- | Only values of 'primative' type a can be communicated
-- | A new chanel can be created using parameters p
class Transport c (e :: Effect) p | c -> e p where
  uSend     :: forall eff. c -> Json -> Aff (dom :: e | eff) Unit
  uProducer :: forall eff. c -> CR.Producer Json (Aff (dom :: e, avar :: AVAR | eff)) Unit
  uOpen     :: forall eff. p -> Eff (dom :: e | eff) c
  uClose    :: forall eff. c -> Eff (dom :: e | eff) Unit

data Channel c s = Channel c (AVar Unit)

-- | Runtime linearity check - will throw an error if a chanel is used multiple
-- | times. Returns a new chanel with the usage reset.
checkLinearity :: forall c s t eff. Channel c s -> Aff (avar :: AVAR | eff) (Channel c t)
checkLinearity (Channel c v) = do
  r <- tryTakeVar v
  case r of
    (Just _) -> (Channel c) <$> makeVar' unit
    _ -> throwError $ error "Linearity exception"

-- | Open a new chanel and receive the initial state
open :: forall r n c s e eff p.
     Initial r s
  => Transport c e p 
  => RoleName r n
  => IsSymbol n
  => Role r -> p -> Aff (dom :: e, avar :: AVAR | eff) (Channel c s)
open _ p = do
  b <- makeVar' unit
  c <- liftEff $ uOpen p
  pure $ Channel c b

-- | We don't need to check linearity here, as in order to construct a terminal
-- | state, the previous state must have been consumed.
close :: forall r c s e eff p.
     Terminal r s
  => Transport c e p
  => Role r -> Channel c s -> Eff (dom :: e | eff) Unit
close _ (Channel c _) = uClose c

send :: forall r rn c a s t e eff p. 
     Send r s t a
  => RoleName r rn
  => IsSymbol rn
  => Transport c e p
  => EncodeJson a
  => Channel c s -> a -> Aff (dom :: e, avar :: AVAR | eff) (Channel c t)
send c@(Channel t _) x = do 
  c' <- checkLinearity c 
  uSend t $ encodeMessage (Role :: Role r) (encodeJson x)
  pure c'

receive :: forall r c a s t e eff p. 
     Receive r s t a
  => Transport c e p
  => DecodeJson a
  => Channel c s -> CR.Consumer Json (Aff (dom :: e, avar :: AVAR | eff)) (Tuple a (Channel c t))
receive c@(Channel t _) = do
  c' <- lift $ checkLinearity c
  x <- CR.await
  case decodeJson x of
    Left e  -> lift $ throwError $ error e
    Right a -> pure $ Tuple a c' 

-- Encode a message with role information for the proxy
encodeMessage :: forall r rn.
     RoleName r rn
  => IsSymbol rn
  => Role r
  -> Json
  -> Json
encodeMessage _ m = fromObject $ fromFoldable $ (Tuple "to" $ fromString
    (reflectSymbol (SProxy :: SProxy rn))) : (Tuple "body" m) : mempty 

newtype Identifier = Identifier String
instance identifierShow :: Show Identifier where
  show (Identifier i) = i

type SessionReq = { protocol :: Tuple String (List String), assignment :: List (Tuple String String), role :: String }

-- TODO: Rewrite this using pureST
encodeReq :: SessionReq -> Json
encodeReq req = fromObject $ fromFoldable $ (Tuple "protocol" protocol) : (Tuple "role" (fromString req.role)) : (Tuple "assignment" ass) : mempty
  where
    (Tuple name roles) = req.protocol
    protocol = fromObject $ fromFoldable $ (Tuple "name" (fromString name)) : (Tuple "roles" roles') : mempty
    roles' = fromArray $ fromString <$> Array.fromFoldable roles
    ass = fromObject $ fromString <$> fromFoldable req.assignment

multiSession :: forall r rn p pn rns rns' list row s t c e ps eff.
     RoleName r rn 
  => IsSymbol rn
  => ProtocolName p pn
  => IsSymbol pn
  => ProtocolRoleNames p rns
  => SList.Symbols rns
  => SList.RemoveSymbol rn rns rns'
  => SList.Symbols rns'
  => SList.ToHomoRowList rns' Identifier list
  => SList.RecordKV list row
  => ListToRow list row
  => Initial r s
  => Terminal r t
  => Transport c e ps
  => ps
  -> Protocol p
  -> Tuple (Role r) Identifier
  -> Record row
  -> (Channel c s -> CR.Consumer Json (Aff (dom :: e, avar :: AVAR | eff)) (Channel c t))
  -> Aff (dom :: e, avar :: AVAR | eff) Unit
multiSession params _ (Tuple r name) ass prog = do 
  (c :: Channel c s) <- open r params
  let (Channel ch _) = c
  CR.runProcess (cons c `CR.pullFrom` uProducer ch)
  where
    cons c@(Channel ch _) = do
      lift $ delay (Milliseconds 200.0) -- Hacky - wait for connection
      lift $ uSend ch (encodeReq proxyReq) -- Send our session request params to the proxy
      _ <- CR.await -- We receive the role/ident assignment back (safe to ignore for now)
      c' <- prog c
      lift $ liftEff $ close r c'
    role = reflectSymbol (SProxy :: SProxy rn)
    proxyReq = { protocol: Tuple (reflectSymbol (SProxy :: SProxy pn)) (SList.symbols (SList.SLProxy :: SList.SLProxy rns)) 
      , role: role
      , assignment: (Tuple role (show name)) : (SList.getKVs (RLProxy :: RLProxy list) ass)
      }

session :: forall r n c e p s t eff.
     Transport c e p
  => Initial r s
  => Terminal r t
  => RoleName r n
  => IsSymbol n
  => Role r
  -> p
  -> (Channel c s -> CR.Consumer Json (Aff (dom :: e, avar :: AVAR | eff)) (Channel c t))
  -> Aff (dom :: e, avar :: AVAR | eff) Unit
session r p prog = do
  (c :: Channel c s) <- open r p
  let (Channel ch _) = c
  CR.runProcess (cons c `CR.pullFrom` uProducer ch)
  where
  cons c = do
    lift $ delay (Milliseconds 200.0) -- Hacky - wait for connection
    c' <- prog c
    lift $ liftEff $ close r c'

-- | Label used for banching/selecting
newtype Label = Label String

instance encodeJsonLabel :: EncodeJson Label where
  encodeJson (Label l) = encodeJson l
instance decodeJsonLabel :: DecodeJson Label where
  decodeJson l = Label <$> decodeJson l

-- | `Functions` maps the 'dictionary' `ts` to a dictionary of continuations
-- | to common state `u` running in monad `m`
class Functions (m :: Type -> Type) (ts :: RowList) (c :: Type -> Type) u (funcs :: RowList) | m ts c u -> funcs
instance functionNil  :: Functions m Nil c u Nil
instance functionCons :: Functions m tail c u tail'
  => Functions m (Cons label t tail) c u (Cons label (c t -> m u) tail')

-- | Constraint to assert element membership in RowList
class Elem (list :: RowList) (l :: Symbol) e | l -> e
instance elemHead :: Elem (Cons l e tail) l e
instance elemTail :: Elem tail l e => Elem (Cons l' e' tail) l e

choice :: forall r c s ts u funcs row e eff p.
     Branch r s ts
  => Terminal r u
  => Transport c e p 
  => Functions (CR.Consumer Json (Aff (dom :: e, avar :: AVAR | eff))) ts (Channel c) (Channel c u) funcs
  => ListToRow funcs row
  => Channel c s -> Record row -> CR.Consumer Json (Aff (dom :: e, avar :: AVAR | eff)) (Channel c u)
choice c row = do 
  c' <- lift $ checkLinearity c
  x <- CR.await
  case decodeJson x of
    Left e  -> lift $ throwError $ error e
    Right (Label label) -> if (unsafeHas label row)
                             then (unsafeGet label row) c'
                             else lift $ throwError (error $ "Branch chosen `"
                                                    <> label  <> "`  is not supported")

select :: forall r rn c s ts t label e eff p.
     Select r s ts
  => RoleName r rn
  => IsSymbol rn
  => Transport c e p
  => Elem ts label t
  => IsSymbol label
  => Channel c s -> SProxy label -> Aff (dom :: e, avar :: AVAR | eff) (Channel c t)
select c@(Channel t _) l = do
  c' <- checkLinearity c
  uSend t $ encodeMessage (Role :: Role r) (encodeJson $ Label $ reflectSymbol l)
  pure c'
