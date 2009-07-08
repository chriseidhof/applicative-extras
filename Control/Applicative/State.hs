module Control.Applicative.State (module Control.Monad.State) where

import Control.Applicative
import Control.Monad.State

{- A state idiom. -}
-- the freedom in reordering the effectful part of the computation comes
-- in that we can pass the state first to f and then to v or first to v and 
-- then to s.  With monads we must first pass the state to f because
-- of the type of >>=.

instance Applicative (State a) where
   pure  = return
   (<*>) = ap
