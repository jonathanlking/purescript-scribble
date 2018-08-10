module Scribble.Type.SList where

-- Very little original contribution here, mainly cherry picking SList parts
-- from purescript-record-extra (by Justin Woo)

import Prelude (class Show, show)
import Record (delete, get)
import Data.Tuple (Tuple(..))
import Data.List (List, (:))
import Data.Monoid (mempty)
import Type.Prelude (class IsSymbol, class RowLacks, RLProxy(RLProxy), SProxy(SProxy), reflectSymbol)
import Type.Row (Cons, Nil, kind RowList)

foreign import kind SList
foreign import data SCons :: Symbol -> SList -> SList
foreign import data SNil :: SList

data SLProxy (ss :: SList) = SLProxy

infixr 6 type SCons as :::

-- | Create value level list of strings
class Symbols (ss :: SList) where
  symbols :: SLProxy ss -> List String

instance nilSymbols :: Symbols SNil where
  symbols _ = mempty

instance consSymbols ::
  ( IsSymbol name
  , Symbols tail
  ) => Symbols (SCons name tail) where
  symbols _ = first : rest
    where
      first = reflectSymbol (SProxy :: SProxy name)
      rest = symbols (SLProxy :: SLProxy tail)

-- | Create homogenous RowList using symbols as labels
class ToHomoRowList (symbols :: SList)
                (a :: Type)
                (row :: RowList)
                | symbols a -> row

instance toHomoRowNil
  :: ToHomoRowList SNil a Nil
instance toHomoRowCons
  :: ToHomoRowList ss a rs
  =>  ToHomoRowList (SCons s ss) a (Cons s a rs)

-- | Remove (the first occurance of) a given symbol from an SList.
-- | The list *must* contain the symbol for the constraint to be satisfied.
class RemoveSymbol (s :: Symbol) (containing :: SList) (lacking :: SList) | s containing -> lacking
instance removeHead :: RemoveSymbol s (SCons s tail) tail
instance removeTail :: RemoveSymbol s ss ss' => RemoveSymbol s (SCons s' ss) (SCons s' ss') 


class RecordKV rl row | rl -> row where
  getKVs :: RLProxy rl -> Record row -> List (Tuple String String)

instance recordKVNil :: RecordKV Nil () where
  getKVs _ _ = mempty

instance recordKVCons ::
  ( IsSymbol key
  , Show a
  , RecordKV listRest rowRest
  , RowLacks key rowRest
  , RowCons  key a rowRest rowFull
  ) => RecordKV (Cons key a listRest) rowFull where
  getKVs _ rec = Tuple (reflectSymbol key) (show val) : rest
    where
    key = SProxy :: SProxy key
    val = get key rec
    rest = getKVs (RLProxy :: RLProxy listRest) (delete key rec)
