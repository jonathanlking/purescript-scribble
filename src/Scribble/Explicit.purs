module Scribble.Explicit where

import Scribble.FSM (class Connect, class Disconnect, class Branch, class Initial, class ProtocolName, class ProtocolRoleNames, class Receive, class RoleName, class Select, class Send, class Terminal, Protocol, Role(..))
import Effect.Aff (Aff, delay)
import Effect.Aff.AVar (AVar, new, empty, put, read, take)
import Data.Time.Duration (Milliseconds(..))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Indexed
import Effect.Exception (error)
import Control.Coroutine as CR
import Data.Tuple (Tuple(..))
import Prelude (class Show, Unit, bind, discard, pure, show, unit, ($), (<$>), (<*>), (<>), (>>=), map)
import Control.Apply ((*>))
import Type.Row (class ListToRow, Cons, Nil, kind RowList, RLProxy(RLProxy))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Record.Unsafe (unsafeGet, unsafeHas)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Core (Json, fromArray, fromObject, fromString, toObject, toString)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Effect.Class (liftEffect)
import Scribble.Type.SList as SList
import Data.List (List, (:))
import Data.Monoid (mempty)
import Foreign.Object (fromFoldable, lookup)
import Data.Array as Array
import Type.Proxy (Proxy)
import Data.String (toLower)
import Data.List.Types (List(..))
import Effect.Class.Console (log)
import Prim.TypeError
import Data.Map as M

import Effect.Class
import Data.Functor
import Control.Apply
import Data.Tuple (snd)
import Control.Bind
import Control.Monad
import Control.Applicative
import Unsafe.Coerce (unsafeCoerce)
import Scribble.Core (class Transport, uOpen, uClose, uSend, uReceive)

-- The session now needs to keep a list of open connections (with each role)
-- TODO: Use a map indexed by the roles? (take the roles as an additonal type paramter)
data Channel c = Channel c (AVar Unit)
newtype Channels c s = Channels (M.Map String (Channel c))

newtype Session c i t a = Session ((Channels c i) -> Aff (Tuple (Channels c t) a))

-- From purescript-indexed-monad
-- instance sessionIxMonad :: IxMonad (Session c) where
--   ipure x = Session (\c -> pure (Tuple c x))
--   ibind (Session s) f
--     = Session (\c -> (s c) 
--         >>= (\(Tuple c' x) -> case f x of
--           (Session s') -> s' c'))
-- 
-- instance sessionFunctor :: Functor (Session c i i) where
--   map f (Session s) = Session (\c -> map (\(Tuple _ x) -> Tuple c $ f x) $ s c)
-- 
-- instance sessionApply :: Apply (Session c i i) where
--   apply (Session f) (Session s) = Session (\c -> apply (h $ f c) (s c))
--     where
--     h :: forall f a b c.
--          Functor f
--       => f (Tuple c (a -> b))
--       -> f (Tuple c a -> Tuple c b)
--     h = map (\(Tuple _ f) -> \(Tuple c x)  -> Tuple c (f x))
-- 
-- instance sessionBind :: Bind (Session c i i) where
--   bind (Session s) f = Session (\c -> (s c) >>= (\(Tuple _ x) -> case f x of (Session s') -> s' c))
-- 
-- instance sessionApplicative :: Applicative (Session c i i) where
--   pure x = Session (\c -> pure (Tuple c x))
-- 
-- instance sessionMonad :: Monad (Session c i i)
-- 
-- instance sessionMonadEffect :: MonadEffect (Session c i i) where
--   liftEffect eff = Session (\c -> map (Tuple c) (liftEffect eff))
-- 
-- aff :: forall c eff i a. Aff a -> Session c i i a
-- aff a = Session (\c -> map (Tuple c) a)

-- -- | Open a new chanel and receive the initial state
-- open :: forall r n c s eff p.
--      Initial r s
--   => Transport c p 
--   => RoleName r n
--   => IsSymbol n
--   => Role r -> p -> Aff (Channel c s)
-- open _ p = do
--   bstack <- new Nil
--   c <- uOpen p
--   pure $ Channel c bstack
-- 
-- -- | We don't need to check linearity here, as in order to construct a terminal
-- -- | state, the previous state must have been consumed.
-- close :: forall r c s eff p.
--      Terminal r s
--   => Transport c p
--   => Role r -> Channel c s -> Aff Unit
-- close _ (Channel c _) = uClose c
-- 
connect :: forall r r' rn s t c p.
     Connect r r' s t
  => Transport c p
  => RoleName r' rn
  => IsSymbol rn
  => Role r' -> p -> Session c s t Unit
connect _ p = Session $ \(Channels cs) ->
  do
    lock <- empty
    conn <- uOpen p
    let chan = Channel conn lock
    let key = reflectSymbol (SProxy :: SProxy rn)
    pure $ Tuple (Channels $ M.insert key chan cs) unit

disconnect :: forall r r' rn s t c p.
     Disconnect r r' s t
  => Transport c p
  => RoleName r' rn
  => IsSymbol rn
  => Role r' -> Session c s t Unit
disconnect _ = Session $ \(Channels cs) ->
  do
    let key = reflectSymbol (SProxy :: SProxy rn)
    let chan = M.lookup key cs
    -- TODO: We should never be in a case where there isn't an entry
    maybe (pure unit) (\(Channel c _) -> uClose c) chan
    pure $ Tuple (Channels $ M.delete key cs) unit

send :: forall r rn c a s t eff p. 
     Send r s t a
  => RoleName r rn
  => IsSymbol rn
  => Transport c p
  => EncodeJson a
  => a -> Session c s t Unit
send x = Session \(Channels cs) -> 
  do
    let key = reflectSymbol (SProxy :: SProxy rn)
    let chan = M.lookup key cs
    -- TODO: We should never be in a case where there isn't an entry
    maybe (pure unit) (\(Channel c _) -> uSend c (encodeJson x)) chan
    pure $ Tuple (Channels cs) unit

receive :: forall r rn c a s t eff p. 
     Receive r s t a
  => RoleName r rn
  => IsSymbol rn
  => Transport c p
  => DecodeJson a
  => Session c s t a
receive = Session \(Channels cs) -> 
  do
    let key = reflectSymbol (SProxy :: SProxy rn)
    let chan = M.lookup key cs
    val <- case chan of
      Nothing -> throwError $ error "Channel closed"
      Just (Channel c lock) -> do
        take lock
        x <- uReceive c
        case decodeJson x of
          Left e  -> throwError $ error e
          Right a -> pure a
    pure $ Tuple (Channels cs) val

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
class Elem (list :: RowList) (l :: Symbol) e | list l -> e
instance elemHead :: Elem (Cons l e tail) l e else
instance elemTail :: Elem tail l e => Elem (Cons l' e' tail) l e else
instance elemEmpty :: 
     Fail (Beside (Text l) (Text "is not a supported choice"))
  => Elem Nil l e 

-- choice :: forall r rn c s ts u funcs row eff p.
--      Branch r s ts
--   => RoleName r rn
--   => Terminal r u
--   => Transport c p 
--   => Functions (Session c) ts u funcs
--   => ListToRow funcs row
--   => Record row -> Session c s u Unit
-- choice row = Session \(Channels cs)
--   do
--     let key = reflectSymbol (SProxy :: SProxy rn)
--     let chan = M.lookup key cs
--     val <- case chan of
--       Nothing -> throwError $ error "Channel closed"
--       Just (Channel c lock) -> do
--         take lock
--         x <- uReceive c
--         let lab = (toObject x >>= lookup "tag" >>= toString)
--         let lab' = toLower <$> lab
--         case lab' of
--           Nothing -> throwError $ error "Unable to parse tag of message in branch"
--           (Just label) -> if (unsafeHas label row)
--                             then do
--                                take bv >>= \vs -> put (Cons x vs) bv
--                                (unsafeGet label row) c
--                             else throwError (error $ "Branch chosen `"
--                                                    <> label  <> "`  is not supported")
--     pure $ Tuple (Channels cs) val
-- 
-- 
-- choice row = Session \c@(Channel ch bv) ->
--   map (Tuple (unsafeCoerce c)) $ do
--    x <- uReceive ch
--    let lab = (toObject x >>= lookup "tag" >>= toString)
--    let lab' = toLower <$> lab
--    case lab' of
--      Nothing -> throwError $ error "Unable to parse tag of message in branch"
--      (Just label) -> if (unsafeHas label row)
--                        then do
--                           take bv >>= \vs -> put (Cons x vs) bv
--                           (unsafeGet label row) c
--                        else throwError (error $ "Branch chosen `"
--                                               <> label  <> "`  is not supported")
-- 
-- select :: forall r rn c s ts t label eff p.
--      Select r s ts
--   => RoleName r rn
--   => IsSymbol rn
--   => Transport c p
--   => Elem ts label t
--   => IsSymbol label
--   => SProxy label -> Session c s t Unit
-- select _ = unsafeCoerce (pure unit :: Session _ s s _)
-- 
-- -- | Encode a message with role information for the proxy
-- encodeMessage :: forall r rn.
--      RoleName r rn
--   => IsSymbol rn
--   => Role r
--   -> Json
--   -> Json
-- encodeMessage _ m = fromObject $ fromFoldable $ (Tuple "to" $ fromString
--     (reflectSymbol (SProxy :: SProxy rn))) : (Tuple "body" m) : mempty 
-- 
-- newtype Identifier = Identifier String
-- instance identifierShow :: Show Identifier where
--   show (Identifier i) = i
-- 
-- type SessionReq = { protocol :: Tuple String (List String), assignment :: List (Tuple String String), role :: String }
-- 
-- -- TODO: Rewrite this using pureST
-- encodeReq :: SessionReq -> Json
-- encodeReq req = fromObject $ fromFoldable $ (Tuple "protocol" protocol) : (Tuple "role" (fromString req.role)) : (Tuple "assignment" ass) : mempty
--   where
--     (Tuple name roles) = req.protocol
--     protocol = fromObject $ fromFoldable $ (Tuple "name" (fromString name)) : (Tuple "roles" roles') : mempty
--     roles' = fromArray $ fromString <$> Array.fromFoldable roles
--     ass = fromObject $ fromString <$> fromFoldable req.assignment
-- 
-- -- | Initialise a multiparty session using the proxy server
-- multiSession :: forall r rn p pn rns rns' list row s t c ps eff.
--      RoleName r rn 
--   => IsSymbol rn
--   => ProtocolName p pn
--   => IsSymbol pn
--   => ProtocolRoleNames p rns
--   => SList.Symbols rns
--   => SList.RemoveSymbol rn rns rns'
--   => SList.Symbols rns'
--   => SList.ToHomoRowList rns' Identifier list
--   => SList.RecordKV list row
--   => ListToRow list row
--   => Initial r s
--   => Terminal r t
--   => Transport c ps
--   => Proxy c
--   -> ps
--   -> Protocol p
--   -> Tuple (Role r) Identifier
--   -> Record row
--   -> Session c s t Unit
--   -> Aff Unit
-- multiSession _ params _ (Tuple r name) ass (Session prog) = do 
--   (c :: Channel c s) <- open r params
--   let (Channel ch _) = c
--   uSend ch (encodeReq proxyReq) -- Send our session request params to the proxy
--   _ <- uReceive ch -- We receive the role/ident assignment back (safe to ignore for now)
--   (Tuple c' _) <- prog c
--   uSend ch (fromString "close")
--   _ <- uReceive ch -- We wait to receive confirmation back
--   close r c'
--     where
--       role = reflectSymbol (SProxy :: SProxy rn)
--       proxyReq = { protocol: Tuple (reflectSymbol (SProxy :: SProxy pn)) (SList.symbols (SList.SLProxy :: SList.SLProxy rns)) 
--         , role: role
--         , assignment: (Tuple role (show name)) : (SList.getKVs (RLProxy :: RLProxy list) ass)
--         }
-- 
-- -- | Designed for a binary session (with direct communication)
-- session :: forall r n c p s t eff.
--      Transport c p
--   => Initial r s
--   => Terminal r t
--   => RoleName r n
--   => IsSymbol n
--   => Proxy c
--   -> Role r
--   -> p
--   -> Session c s t Unit
--   -> Aff Unit
-- session _ r p (Session prog) = do
--   (c :: Channel c s) <- open r p
--   (Tuple c' _) <- prog c
--   close r c'
