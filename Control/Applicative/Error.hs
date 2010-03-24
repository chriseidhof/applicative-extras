{-# LANGUAGE PatternGuards #-}
module Control.Applicative.Error where

import Control.Applicative

-- | An error idiom.  Rather like the error monad, but collect all
-- | errors together 
data Failing a = Success a | Failure [ErrorMsg]
 deriving Show
type ErrorMsg = String

instance Functor Failing where
  fmap f (Failure fs) = Failure fs
  fmap f (Success a) = Success (f a)

instance Applicative Failing where
   pure = Success
   Failure msgs <*> Failure msgs' = Failure (msgs ++ msgs')
   Success _ <*> Failure msgs' = Failure msgs'
   Failure msgs' <*> Success _ = Failure msgs'
   Success f <*> Success x = Success (f x)

instance Alternative Failing where
  empty                       = Failure []
  (Success x) <|> _           = Success x
  _           <|> (Success y) = Success y
  (Failure x) <|> (Failure y) = Failure (x ++ y)

maybeRead :: Read a => String -> Maybe a
maybeRead s | [(i, "")] <- readsPrec 0 s = Just i
            | otherwise = Nothing

-- | Tries to read a value. Shows an error message when reading fails.
maybeRead' :: Read a => String -> String -> Failing a
maybeRead' s msg | Just x <- maybeRead s = Success x
                 | otherwise = Failure [msg]

-- | Tries to read an Integer
asInteger :: String -> Failing Integer
asInteger s = maybeRead' s (s ++ " is not a valid integer")

-- | Tries conversion to an enum
tryToEnum :: Enum a => Int -> Failing a
tryToEnum x | value <- toEnum x = Success value
            | otherwise         = Failure ["Conversion error"]
