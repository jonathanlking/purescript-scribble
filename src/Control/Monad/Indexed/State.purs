module Control.Monad.Indexed.State
  ( IxStateT
  ) where

import Data.Tuple (Tuple(..))
import Control.Monad (class Monad, pure, (>>=))
import Control.Monad.Indexed (class IxMonad)
import Data.Lens.Lens (Lens, lens)

-- TODO: Include IxMonadState class?
-- http://hackage.haskell.org/package/indexed-extras-0.2/docs/Control-Monad-Indexed-State.html

newtype IxStateT m i j a = IxStateT (i -> m (Tuple a j))

-- instance indexedStateTIxMonad :: Monad m => IxMonad (IxStateT m) where
--   ipure a = IxStateT (\i -> pure (Tuple a i))
--   ibind (IxStateT s) f
--     = IxStateT (\i -> (s i) 
--         >>= (\(Tuple a j) -> case f a of
--           (IxStateT s') -> s' j))

-- I'm not sure if the following are lawful/morally correct, but they're useful
-- What I'm trying to do is 'lift' values into an indexed 
-- 
-- class HoistIx (ixT :: (Type -> Type) -> Type -> Type -> Type -> Type) where
--   hoist :: forall f g t a b. (f a -> g b) -> ixT f t t a -> ixT g t t b
-- 
-- instance ixStateTTageed :: Tagged (IxStateT m i i a) where
--  hoist (IxStateT cont) = IxStateT \i -> case cont i of 

-- instance indexedLiftFunctor :: Functor f 
-- 
-- instance indexedStateTFunctor :: Functor f => Functor (IxStateT c i i) where
--   map f (IxStateT s) = IxStateT (\c -> map (\(Tuple _ x) -> Tuple c $ f x) $ s c)

-- instance indexedStateTApply :: Apply (IxStateT c i i) where
--   apply (IxStateT f) (IxStateT s) = IxStateT (\c -> apply (h $ f c) (s c))
--     where
--     h :: forall f a b c.
--          Functor f
--       => f (Tuple c (a -> b))
--       -> f (Tuple c a -> Tuple c b)
--     h = map (\(Tuple _ f) -> \(Tuple c x)  -> Tuple c (f x))
-- 
-- instance indexedStateTBind :: Bind (IxStateT c i i) where
--   bind (IxStateT s) f = IxStateT (\c -> (s c) >>= (\(Tuple _ x) -> case f x of (IxStateT s') -> s' c))
-- 
-- instance indexedStateTApplicative :: Applicative (IxStateT c i i) where
--   pure x = IxStateT (\c -> pure (Tuple c x))
-- 
-- instance indexedStateTMonad :: Monad (IxStateT c i i)
-- 
-- instance indexedStateTMonadEffect :: MonadEffect (IxStateT c i i) where
--   liftEffect eff = IxStateT (\c -> map (Tuple c) (liftEffect eff))
