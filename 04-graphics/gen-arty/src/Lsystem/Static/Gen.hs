module Lsystem.Static.Gen where

type Rules a = a -> [a]

gen :: Eq a => Rules a -> [a] -> Int -> [a]
gen rules seed step
  | step == 0 = seed
  | otherwise = gen rules (rules =<< seed) (step - 1)

