module Main where

import Exam.App
import Exam.Args

main :: IO ()
main = uncurry runExam =<< readExam
