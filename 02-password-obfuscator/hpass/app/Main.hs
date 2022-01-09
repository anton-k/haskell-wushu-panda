module Main where

import Crypto.Hpass.App
import Crypto.Hpass.Args

main :: IO ()
main = runHpass =<< readHpass
