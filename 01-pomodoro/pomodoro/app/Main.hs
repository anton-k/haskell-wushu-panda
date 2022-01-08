module Main where

import System.Pomodoro.App
import System.Pomodoro.Args

main :: IO ()
main = runPomodoro =<< readPomodoro
