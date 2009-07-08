{-# LANGUAGE TypeOperators #-}
module Control.Applicative.Compose where

import Control.Applicative

-- | Type-level composition
newtype (f :+: g) a = Compose { decompose :: (f (g a)) }

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Compose x) = Compose $ (fmap . fmap) f x

{- The composition of any two Applicatives is an idiom -}
instance (Applicative f, Applicative g) => Applicative (f :+: g) where
   pure x = Compose$ (pure pure) <*> (pure x)
   Compose fs <*> Compose xs = Compose $ pure (<*>) <*> fs <*> xs
