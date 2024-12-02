{-# LANGUAGE ExistentialQuantification #-}

module Util where

data Day = forall a. (Show a) => Day Int a

evalDay :: Day -> String
evalDay (Day _ r) = show r

diff :: (Num a) => a -> a -> a
diff x y = abs $ x - y