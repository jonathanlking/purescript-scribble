module Scribble.Session
  ( Session
  , Channels
  , class Continuations
  , class Elem
  , lift
  , whileWaiting
  , session
  , send
  , receive
  , choice
  , connect
  , disconnect
  , select
  ) where

import Prelude

import Scribble.Transport (class Transport, class TransportClient, class TransportServer)
import Scribble.Transport as T
import Scribble.FSM (class Branch, class Initial, class Receive, class RoleName, class Select, class Send,
    class Connect, class Accept, class Disconnect, class Terminal, Role)

import Control.Monad.Error.Class (throwError)

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Map as M
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))

import Effect.Exception (error)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff.AVar (AVar, new, put, take)
import Effect.Aff.Class (class MonadAff, liftAff)

import Type.Row (class ListToRow, Cons, Nil, kind RowList)
import Type.Proxy (Proxy)

import Record.Unsafe (unsafeGet, unsafeHas)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Core (Json, toObject, toString)
import Foreign.Object (lookup)
import Data.String (toLower)
import Data.List.Types (List(..))
import Prim.TypeError as TE

import Unsafe.Coerce (unsafeCoerce)

import Control.Monad.Indexed (class IxMonad, iap)
import Control.Bind.Indexed (class IxBind, ibind)
import Control.Apply.Indexed (class IxApply)
import Control.Applicative.Indexed (class IxApplicative, ipure)
import Data.Functor.Indexed (class IxFunctor, imap)

-- The session now needs to keep a list of open connections (with each role)
-- TODO: Use a map indexed by the roles? (take the roles as an additonal type paramter)
data Channel c = Channel c (AVar (List Json))
newtype Channels c s = Channels (M.Map String (Channel c))

newtype Session m c i t a = Session ((Channels c i) -> m (Tuple (Channels c t) a))

instance sessionIxMonad :: Monad m => IxMonad (Session m c)

instance sessionIxApplicative :: Monad m => IxApplicative (Session m c) where
  ipure x = Session (\c -> pure (Tuple c x))

instance sessionIxBind :: Monad m => IxBind (Session m c) where
  ibind (Session s) f
    = Session (\c -> (s c) 
        >>= (\(Tuple c' x) -> case f x of
          (Session s') -> s' c'))

-- TODO: Check if the Monad constraint can be relaxed to Applicative
-- iapply ∷ ∀ a b i j k. Session m c i j (a → b) → Session m c j k a → Session m c i k b
-- 
-- c :: Channel c i 
-- f c :: m (Tuple (Channel c j) (a -> b))
-- 
-- s :: ((Channel c j) -> m (Tuple (Channel c k) a))
-- 
-- need to produce :: m (Tuple (Channel c k) b)
-- 
-- I'm almost certain `m` must be a Monad (as you need m (m a) -> m a)

instance sessionIxApply :: Monad m => IxApply (Session m c) where
  iapply = iap

instance sessionIxFunctor :: Functor m => IxFunctor (Session m c) where
  imap f (Session s) = Session \c -> map (\(Tuple c' x) -> Tuple c' $ f x) $ s c

instance sessionFunctor :: Functor m => Functor (Session m c i j) where
  map = imap

instance sessionApply :: Monad m => Apply (Session m c i i) where
  apply = iap

instance sessionBind :: Monad m => Bind (Session m c i i) where
  bind = ibind

instance sessionApplicative :: Monad m => Applicative (Session m c i i) where
  pure = ipure

instance sessionMonad :: Monad m => Monad (Session m c i i)

instance sessionMonadEffect :: MonadEffect m => MonadEffect (Session m c i i) where
  liftEffect eff = Session \c -> map (Tuple c) (liftEffect eff)

instance sessionMonadAff :: MonadAff m => MonadAff (Session m c i i) where
  liftAff aff = Session \c -> map (Tuple c) (liftAff aff)

lift :: forall c i f a. Functor f => f a -> Session f c i i a
lift x = Session \c -> map (Tuple c) x

whileWaiting :: forall m c i j a.
     -- I really want: forall t. Semigroup m t
     -- But PS doesn't have quantified constraints
     Semigroup (m (Tuple (Channels c j) a))
  => Session m c i j a
  -> (forall t. m t)
  -> Session m c i j a
whileWaiting (Session s) action
  = Session \c -> let x = s c in x <> (action :: m (Tuple (Channels c j) a))

connect :: forall r r' rn m s t c p.
     Connect r r' s t
  => TransportClient c p
  => RoleName r' rn
  => IsSymbol rn
  => MonadAff m
  => Role r' -> p -> Session m c s t Unit
connect _ p = Session $ \(Channels cs) ->
  do
    bstack <- liftAff $ new Nil
    conn <- T.connect p
    let chan = Channel conn bstack
    let key = reflectSymbol (SProxy :: SProxy rn)
    let cs' = M.insert key chan cs
    pure $ Tuple (Channels cs') unit

request :: forall r r' rn m s t c p.
     Accept r r' s t
  => TransportServer c p
  => RoleName r' rn
  => IsSymbol rn
  => MonadAff m
  => Role r' -> p -> Session m c s t Unit
request _ p = Session $ \(Channels cs) ->
  do
    bstack <- liftAff $ new Nil
    conn <- T.serve p
    let chan = Channel conn bstack
    let key = reflectSymbol (SProxy :: SProxy rn)
    let cs' = M.insert key chan cs
    pure $ Tuple (Channels cs') unit

disconnect :: forall r r' rn m s t c p.
     Disconnect r r' s t
  => Transport c p
  => RoleName r' rn
  => IsSymbol rn
  => MonadAff m
  => Role r' -> Session m c s t Unit
disconnect _ = Session $ \(Channels cs) ->
  do
    let key = reflectSymbol (SProxy :: SProxy rn)
    let chan = M.lookup key cs
    -- TODO: We should never be in a case where there isn't an entry
    maybe (pure unit) (\(Channel c _) -> T.close c) chan
    pure $ Tuple (Channels $ M.delete key cs) unit

-- | Designed for a session with explicit connections
session :: forall r c p s t m a.
     Transport c p
  => Initial r s
  => Terminal r t
  => MonadAff m
  => Proxy c
  -> Role r
  -> Session m c s t a
  -> m a
session _ _ (Session prog) = do
  (Tuple _ x) <- prog (Channels M.empty)
  pure x

send :: forall r rn c a s t m p. 
     Send r s t a
  => RoleName r rn
  => IsSymbol rn
  => Transport c p
  => EncodeJson a
  => MonadAff m
  => a -> Session m c s t Unit
send x = Session \(Channels cs) -> 
  do
    let key = reflectSymbol (SProxy :: SProxy rn)
    let chan = M.lookup key cs
    -- TODO: We should never be in a case where there isn't an entry
    maybe (pure unit) (\(Channel c _) -> T.send c (encodeJson x)) chan
    pure $ Tuple (Channels cs) unit

receive :: forall r rn c a s t m p. 
     Receive r s t a
  => RoleName r rn
  => IsSymbol rn
  => Transport c p
  => DecodeJson a
  => MonadAff m
  => Session m c s t a
receive = Session \(Channels cs) ->
  do
    let key = reflectSymbol (SProxy :: SProxy rn)
    let chan = M.lookup key cs
    case chan of
      Nothing -> liftAff $ throwError $ error $ "Channel with " <> key <> " is closed"
      Just (Channel c bv) -> do
         b <- liftAff $ take bv
         x <- case b of
           Nil -> do
             liftAff $ put Nil bv
             T.receive c
           (Cons v vs) -> liftAff $ (put vs bv) *> pure v
         case decodeJson x of
           Left e  -> liftAff $ throwError $ error e
           Right a -> pure (Tuple (Channels cs) a)

-- | Label used for banching/selecting
newtype Label = Label String

instance encodeJsonLabel :: EncodeJson Label where
  encodeJson (Label l) = encodeJson l
instance decodeJsonLabel :: DecodeJson Label where
  decodeJson l = Label <$> decodeJson l

-- | `Continuations` maps the 'dictionary' `ts` to a dictionary of continuations
-- | to common state `u` running in monad `m`
class Continuations (im :: Type -> Type -> Type -> Type) (ts :: RowList) u a (funcs :: RowList) | im ts u a -> funcs
instance functionNil  :: Continuations im Nil u a Nil
else
instance functionCons :: Continuations im tail u a tail'
  => Continuations im (Cons label t tail) u a (Cons label (im t u a) tail')

-- | Constraint to assert element membership in RowList
class Elem (list :: RowList) (l :: Symbol) e | list l -> e
instance elemHead :: Elem (Cons l e tail) l e
else
instance elemTail :: Elem tail l e => Elem (Cons l' e' tail) l e 
else
instance elemEmpty :: 
     TE.Fail (TE.Beside (TE.Text l) (TE.Text " is not a supported choice"))
  => Elem Nil l e 

choice :: forall r r' rn c s ts u funcs row m p a.
     Branch r r' s ts
  => RoleName r' rn
  => IsSymbol rn
  => Terminal r u
  => Transport c p 
  => Continuations (Session m c) ts u a funcs
  => ListToRow funcs row
  => MonadAff m
  => Record row -> Session m c s u a
choice row = Session \(Channels cs) ->
  do
    let key = reflectSymbol (SProxy :: SProxy rn)
    let chan = M.lookup key cs
    case chan of
      Nothing -> liftAff $ throwError $ error $ "Channel with " <> key <> " is closed"
      Just c@(Channel ch bv) -> do
        x <- T.receive ch
        let lab = (toObject x >>= lookup "tag" >>= toString)
        let lab' = toLower <$> lab
        case lab' of
          Nothing -> liftAff $ throwError $ error "Unable to parse tag of message in branch"
          (Just label) -> if (unsafeHas label row)
                            then do
                               liftAff $ take bv >>= \vs -> put (Cons x vs) bv
                               (unsafeGet label row) c
                            else liftAff $ throwError (error $ "Branch chosen `"
                                                   <> label  <> "`  is not supported")

select :: forall r rn c s ts t label m p.
     Select r s ts
  => RoleName r rn
  => IsSymbol rn
  => Transport c p
  => Elem ts label t
  => IsSymbol label
  => MonadAff m
  => SProxy label -> Session m c s t Unit
select _ = unsafeCoerce (pure unit :: Session m c s s Unit)
