{-# LANGUAGE ExistentialQuantification #-}

module Util where

import GHC.Stack (HasCallStack)

data Day = forall a. (Show a) => Day Int a

unsafeListToTuple :: (HasCallStack) => [a] -> (a, a)
unsafeListToTuple [a, b] = (a, b)
unsafeListToTuple _ = error "Supplied list does not have the right structure"

listToTuple :: [a] -> Maybe (a, a)
listToTuple [a, b] = Just (a, b)
listToTuple _ = Nothing

evalDay :: Day -> String
evalDay (Day _ r) = show r

runDay :: Day -> (Int, String)
runDay (Day d r) = (d, show r)

diff :: (Num a) => a -> a -> a
diff x y = abs $ x - y