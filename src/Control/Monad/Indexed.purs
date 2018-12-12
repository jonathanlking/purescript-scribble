module Control.Monad.Indexed where

class IxMonad m where
  ipure ∷ ∀ a i. a → m i i a
  ibind ∷ ∀ a b i j k. m i j a → (a → m j k b) → m i k b

infixl 1 ibind as :>>=
