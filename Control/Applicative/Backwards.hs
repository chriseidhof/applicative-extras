module Control.Applicative.Backwards where

import Prelude hiding (foldr, foldr1, foldl, foldl1)
import Control.Applicative
import Data.Foldable
import Data.Traversable
import Data.Monoid

newtype Backwards t a = Backwards { forwards :: t a }

instance (Functor t) => Functor (Backwards t) where
 fmap f (Backwards a) = Backwards (fmap f a)

instance (Applicative t) => Applicative (Backwards t) where
 pure a = Backwards (pure a)
 (Backwards f) <*> (Backwards a) = Backwards (a <**> f)

instance (Foldable t) => Foldable (Backwards t) where
 foldMap f (Backwards t) = getDual (foldMap (Dual . f) t)
 foldr f z (Backwards t) = foldl (flip f) z t
 foldl f z (Backwards t) = foldr (flip f) z t
 foldr1 f (Backwards t) = foldl1 (flip f) t
 foldl1 f (Backwards t) = foldr1 (flip f) t

instance (Traversable t) => Traversable (Backwards t) where
 traverse f (Backwards t) =
   fmap Backwards . forwards $ traverse (Backwards . f) t
 sequenceA (Backwards t) =
   fmap Backwards . forwards $ sequenceA (fmap Backwards t)
