module Scribble.Indexed where

import Scribble.FSM (class Branch, class Initial, class ProtocolName, class ProtocolRoleNames, class Receive, class RoleName, class Select, class Send, class Terminal, Protocol, Role(..))
import Control.Monad.Aff (Aff, delay)
import Control.Monad.Aff.AVar (AVAR, AVar, makeVar, makeEmptyVar, putVar, tryTakeVar, takeVar)
import Data.Time.Duration (Milliseconds(..))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Control.Coroutine as CR
import Data.Tuple (Tuple(..))
import Prelude (class Show, Unit, bind, discard, pure, show, unit, ($), (<$>), (<*>), (<>), (>>=), map)
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

import Control.Monad.Eff.Class
import Data.Functor
import Control.Apply
import Data.Tuple (snd)
import Control.Bind
import Control.Monad
import Control.Applicative
import Unsafe.Coerce (unsafeCoerce)
import Scribble.Core (class Transport, uOpen, uClose, uSend, uReceive)

-- TODO: Really shouldn't be here... (fix row/set effect issue)
import DOM (DOM)

type TransportEffects eff = (dom :: DOM, avar :: AVAR, exception :: EXCEPTION | eff)

newtype Session c eff i t a = Session ((Channel c i) -> Aff eff (Tuple (Channel c t) a))

-- From purescript-indexed-monad
class IxMonad m where
  ipure ∷ ∀ a x. a → m x x a
  ibind ∷ ∀ a b x y z. m x y a → (a → m y z b) → m x z b

infixr 1 ibind as :>>=

instance sessionIxMonad :: IxMonad (Session c eff) where
  ipure x = Session (\c -> pure (Tuple c x))
  ibind (Session s) f = Session (\c -> (s c) >>= (\(Tuple c' x) -> case f x of (Session s') -> s' c'))

instance sessionFunctor :: Functor (Session c eff i i) where
  map f (Session s) = Session (\c -> map (\(Tuple _ x) -> Tuple c $ f x) $ s c)

instance sessionApply :: Apply (Session c eff i i) where
  apply (Session f) (Session s) = Session (\c -> apply (h $ f c) (s c))
    where
    h :: forall f a b c.
         Functor f
      => f (Tuple c (a -> b))
      -> f (Tuple c a -> Tuple c b)
    h = map (\(Tuple _ f) -> \(Tuple c x)  -> Tuple c (f x))

instance sessionBind :: Bind (Session c eff i i) where
  bind (Session s) f = Session (\c -> (s c) >>= (\(Tuple _ x) -> case f x of (Session s') -> s' c))

instance sessionApplicative :: Applicative (Session c eff i i) where
  pure x = Session (\c -> pure (Tuple c x))

instance sessionMonad :: Monad (Session c eff i i)

instance sessionMonadEff :: MonadEff eff (Session c eff i i) where
  liftEff eff = Session (\c -> map (Tuple c) (liftEff eff))

aff :: forall c eff i a. Aff eff a -> Session c eff i i a
aff a = Session (\c -> map (Tuple c) a)

data Channel c s = Channel c (AVar (List Json))

-- | Open a new chanel and receive the initial state
open :: forall r n c s eff p.
     Initial r s
  => Transport c p 
  => RoleName r n
  => IsSymbol n
  => Role r -> p -> Aff (TransportEffects eff) (Channel c s)
open _ p = do
  bstack <- makeVar Nil
  c <- uOpen p
  pure $ Channel c bstack

-- | We don't need to check linearity here, as in order to construct a terminal
-- | state, the previous state must have been consumed.
close :: forall r c s eff p.
     Terminal r s
  => Transport c p
  => Role r -> Channel c s -> Aff (TransportEffects eff) Unit
close _ (Channel c _) = uClose c

send :: forall r rn c a s t eff p. 
     Send r s t a
  => RoleName r rn
  => IsSymbol rn
  => Transport c p
  => EncodeJson a
  => a -> Session c (TransportEffects eff) s t Unit
send x = Session \c@(Channel t _) -> 
  map (Tuple (unsafeCoerce c)) 
    (uSend t $ encodeMessage (Role :: Role r) (encodeJson x))

receive :: forall r c a s t eff p. 
     Receive r s t a
  => Transport c p
  => DecodeJson a
  => Session c (TransportEffects eff) s t a
receive = Session \c@(Channel t bv) ->
  map (Tuple (unsafeCoerce c)) $ do
    b <- takeVar bv
    x <- case b of
      Nil -> do
        putVar Nil bv
        uReceive t
      (Cons v vs) -> (putVar vs bv) *> pure v
    case decodeJson x of
      Left e  -> throwError $ error e
      Right a -> pure a

-- | Label used for banching/selecting
newtype Label = Label String

instance encodeJsonLabel :: EncodeJson Label where
  encodeJson (Label l) = encodeJson l
instance decodeJsonLabel :: DecodeJson Label where
  decodeJson l = Label <$> decodeJson l

-- | `Functions` maps the 'dictionary' `ts` to a dictionary of continuations
-- | to common state `u` running in monad `m`
class Functions (im :: Type -> Type -> Type -> Type) (ts :: RowList) u (funcs :: RowList) | im ts u -> funcs
instance functionNil  :: Functions im Nil u Nil
instance functionCons :: Functions im tail u tail'
  => Functions im (Cons label t tail) u (Cons label (im t u Unit) tail')

-- | Constraint to assert element membership in RowList
class Elem (list :: RowList) (l :: Symbol) e | l -> e
instance elemHead :: Elem (Cons l e tail) l e
instance elemTail :: Elem tail l e => Elem (Cons l' e' tail) l e
instance elemEmpty :: 
     Fail (TypeConcat (TypeConcat "'" l) "' is not a supported choice") 
  => Elem Nil l e 

choice :: forall r c s ts u funcs row eff p.
     Branch r s ts
  => Terminal r u
  => Transport c p 
  => Functions (Session c (TransportEffects eff)) ts u funcs
  => ListToRow funcs row
  => Record row -> Session c (TransportEffects eff) s u Unit
choice row = Session \c@(Channel ch bv) ->
  map (Tuple (unsafeCoerce c)) $ do
   x <- uReceive ch
   let lab = (toObject x >>= lookup "tag" >>= toString)
   let lab' = toLower <$> lab
   case lab' of
     Nothing -> throwError $ error "Unable to parse tag of message in branch"
     (Just label) -> if (unsafeHas label row)
                       then do
                          takeVar bv >>= \vs -> putVar (Cons x vs) bv
                          (unsafeGet label row) c
                       else throwError (error $ "Branch chosen `"
                                              <> label  <> "`  is not supported")

select :: forall r rn c s ts t label eff p.
     Select r s ts
  => RoleName r rn
  => IsSymbol rn
  => Transport c p
  => Elem ts label t
  => IsSymbol label
  => SProxy label -> Session c (TransportEffects eff) s t Unit
select _ = unsafeCoerce (pure unit :: Session _ _ s s _)

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
  -> Session c (TransportEffects eff) s t Unit
  -> Aff (TransportEffects eff) Unit
multiSession _ params _ (Tuple r name) ass (Session prog) = do 
  (c :: Channel c s) <- open r params
  let (Channel ch _) = c
  uSend ch (encodeReq proxyReq) -- Send our session request params to the proxy
  _ <- uReceive ch -- We receive the role/ident assignment back (safe to ignore for now)
  (Tuple c' _) <- prog c
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
  -> Session c (TransportEffects eff) s t Unit
  -> Aff (TransportEffects eff) Unit
session _ r p (Session prog) = do
  (c :: Channel c s) <- open r p
  (Tuple c' _) <- prog c
  close r c'
