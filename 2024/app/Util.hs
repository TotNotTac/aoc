{-# LANGUAGE ExistentialQuantification #-}

module Util where

data Day = forall a. (Show a) => Day Int a

evalDay :: Day -> String
evalDay (Day _ r) = show r

runDay :: Day -> (Int, String)
runDay (Day d r) = (d, show r)

diff :: (Num a) => a -> a -> a
diff x y = abs $ x - y