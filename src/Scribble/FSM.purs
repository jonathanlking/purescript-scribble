module Scribble.FSM where

import Type.Row (kind RowList)
import Scribble.Type.SList (kind SList)

foreign import kind Protocol
data Protocol (p :: Protocol) = Protocol

foreign import kind Role
data Role (r :: Role) = Role

-- | The symbol representation (name) of a protocol
class ProtocolName (r :: Protocol) (name :: Symbol) | r -> name

-- | The symbol representation (name) of a role
class RoleName (r :: Role) (name :: Symbol) | r -> name

-- | The list of roles in a protocol
class ProtocolRoleNames (p :: Protocol) (rs :: SList) | p -> rs

-- | A way of supplying the terminal/initial state for a given role
-- | TODO: Work out why the constraint solver can't solve them
class Initial (r :: Role) a | r -> a
class Terminal (r :: Role) a | r -> a

-- | Transition from state `s` to `t` through sending a value of type `a`
class Send (r :: Role) s t a | a -> t, s -> r a

-- | Transition from state `s` to `t` through receiving a value of type `a`
class Receive (r :: Role) s t a | a -> t, s -> r a

-- | Branching (external choice) allows the other party to select from one or
-- | more next states to transition to. Therefore you must provide continuations 
-- | to handle all of the possibilities.
-- | Valid Scribble protocols guarentee that there is at most one terminal
-- | state, so all branches must either reach it or loop infinitely.
-- | In this case 'r' is the role of the party offering the choice.
class Branch (r :: Role) s (ts :: RowList) | s -> ts r

-- | Selection (internal choice) is the dual of branching and allows you to
-- | select your next state. You distinguish your choice with a proxy containing
-- | the symbol of the next state.
-- | `s` and `ts` have the same meaning.
class Select (r :: Role) s (ts :: RowList) | s -> ts r

class Connect (r :: Role) (r' :: Role) s t | s -> r r' t
class Disconnect (r :: Role) (r' :: Role) s t | s -> r r' t
