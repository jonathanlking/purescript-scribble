module Scribble.Core where

import Scribble.FSM (class Branch, class Initial, class ProtocolName, class ProtocolRoleNames, class Receive, class RoleName, class Select, class Send, class Terminal, Protocol, Role(..))
import Control.Monad.Aff (Aff, delay)
import Control.Monad.Aff.AVar (AVAR, AVar, makeVar, makeVar', putVar, tryTakeVar, takeVar)
import Data.Time.Duration (Milliseconds(..))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Control.Coroutine as CR
import Data.Tuple (Tuple(..))
import Prelude (class Show, Unit, bind, discard, pure, show, unit, ($), (<$>), (<*>), (<>), (>>=))
import Control.Apply ((*>))
import Control.Monad.Eff (kind Effect, Eff)
import Type.Row (class ListToRow, Cons, Nil, kind RowList, RLProxy(RLProxy))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Record.Unsafe (unsafeGet, unsafeHas)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Core (Json, fromArray, fromObject, fromString, toObject, toString)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Control.Monad.Eff.Class (liftEff)
import Scribble.Type.SList as SList
import Data.List (List, (:))
import Data.Monoid (mempty)
import Data.StrMap (fromFoldable, lookup)
import Data.Array as Array
import Type.Proxy (Proxy)
import Data.String (toLower)
import Data.List.Types (List(..))
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Eff.Console (log)

-- TODO: Really shouldn't be here... (fix row/set effect issue)
import DOM (DOM)

type TransportEffects eff = (dom :: DOM, avar :: AVAR, exception :: EXCEPTION | eff)

-- | An asynchronous untyped communication layer
-- | Only values of 'primative' type a can be communicated
-- | A new chanel can be created using parameters p
class Transport c p | c -> p where
  uSend     :: forall eff. c -> Json -> Aff (TransportEffects eff) Unit
  uReceive  :: forall eff. c -> Aff (TransportEffects eff) Json
  uOpen     :: forall eff. p -> Aff (TransportEffects eff) c
  uClose    :: forall eff. c -> Aff (TransportEffects eff) Unit

data Channel c s = Channel c (AVar (List Json)) (AVar Unit)

-- | Runtime linearity check - will throw an error if a chanel is used multiple
-- | times. Returns a new chanel with the usage reset.
checkLinearity :: forall c s t eff. Channel c s -> Aff (TransportEffects eff) (Channel c t)
checkLinearity (Channel c bv v) = do
  r <- tryTakeVar v
  case r of
    (Just _) -> (Channel c bv) <$> makeVar' unit
    _ -> throwError $ error "Linearity exception"

-- | Open a new chanel and receive the initial state
open :: forall r n c s eff p.
     Initial r s
  => Transport c p 
  => RoleName r n
  => IsSymbol n
  => Role r -> p -> Aff (TransportEffects eff) (Channel c s)
open _ p = do
  lin <- makeVar' unit
  bstack <- makeVar' Nil
  c <- uOpen p
  pure $ Channel c bstack lin

-- | We don't need to check linearity here, as in order to construct a terminal
-- | state, the previous state must have been consumed.
close :: forall r c s eff p.
     Terminal r s
  => Transport c p
  => Role r -> Channel c s -> Aff (TransportEffects eff) Unit
close _ (Channel c _ _) = uClose c

send :: forall r rn c a s t eff p. 
     Send r s t a
  => RoleName r rn
  => IsSymbol rn
  => Transport c p
  => EncodeJson a
  => Channel c s -> a -> Aff (TransportEffects eff) (Channel c t)
send c@(Channel t _ _) x = do
  c' <- checkLinearity c 
  uSend t $ encodeMessage (Role :: Role r) (encodeJson x)
  pure c'

receive :: forall r c a s t eff p. 
     Receive r s t a
  => Transport c p
  => DecodeJson a
  => Channel c s -> Aff (TransportEffects eff) (Tuple Json (Channel c t))
receive ch@(Channel c bv _) = do
  ch' <- checkLinearity ch
  b <- takeVar bv
  x <- case b of
    Nil -> do
      putVar bv Nil
      uReceive c
    (Cons v vs) -> (putVar bv vs) *> pure v
  case decodeJson x of
    Left e  -> throwError $ error e
    Right a -> pure $ Tuple a ch' 

-- | Encode a message with role information for the proxy
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

-- | Initialise a multiparty session using the proxy server
multiSession :: forall r rn p pn rns rns' list row s t c ps eff.
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
  => Transport c ps
  => Proxy c
  -> ps
  -> Protocol p
  -> Tuple (Role r) Identifier
  -> Record row
  -> (Channel c s -> Aff (TransportEffects eff) (Channel c t))
  -> Aff (TransportEffects eff) Unit
multiSession _ params _ (Tuple r name) ass prog = do 
  (c :: Channel c s) <- open r params
  let (Channel ch _ _) = c
  uSend ch (encodeReq proxyReq) -- Send our session request params to the proxy
  _ <- uReceive ch -- We receive the role/ident assignment back (safe to ignore for now)
  c' <- prog c
  uSend ch (fromString "close")
  _ <- uReceive ch -- We wait to receive confirmation back
  close r c'
    where
      role = reflectSymbol (SProxy :: SProxy rn)
      proxyReq = { protocol: Tuple (reflectSymbol (SProxy :: SProxy pn)) (SList.symbols (SList.SLProxy :: SList.SLProxy rns)) 
        , role: role
        , assignment: (Tuple role (show name)) : (SList.getKVs (RLProxy :: RLProxy list) ass)
        }

-- | Designed for a binary session (with direct communication)
session :: forall r n c p s t eff.
     Transport c p
  => Initial r s
  => Terminal r t
  => RoleName r n
  => IsSymbol n
  => Proxy c
  -> Role r
  -> p
  -> (Channel c s -> Aff (TransportEffects eff) (Channel c t))
  -> Aff (TransportEffects eff) Unit
session _ r p prog = do
  (c :: Channel c s) <- open r p
  c' <- prog c
  close r c'

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

choice :: forall r c s ts u funcs row eff p.
     Branch r s ts
  => Terminal r u
  => Transport c p 
  => Functions (Aff (TransportEffects eff)) ts (Channel c) (Channel c u) funcs
  => ListToRow funcs row
  => Channel c s -> Record row -> Aff (TransportEffects eff) (Channel c u)
choice c@(Channel ch bv _) row = do
  c' <- checkLinearity c
  x <- uReceive ch
  let lab = (toObject x >>= lookup "tag" >>= toString)
  let lab' = toLower <$> lab
  case lab' of
    Nothing -> throwError $ error "Unable to parse tag of message in branch"
    (Just label) -> if (unsafeHas label row)
                      then do
                         takeVar bv >>= \vs -> putVar bv (Cons x vs)
                         (unsafeGet label row) c'
                      else throwError (error $ "Branch chosen `"
                                             <> label  <> "`  is not supported")

select :: forall r rn c s ts t label eff p.
     Select r s ts
  => RoleName r rn
  => IsSymbol rn
  => Transport c p
  => Elem ts label t
  => IsSymbol label
  => Channel c s -> SProxy label -> Aff (TransportEffects eff) (Channel c t)
select c@(Channel t _ _) l = do
  c' <- checkLinearity c
  pure c'
