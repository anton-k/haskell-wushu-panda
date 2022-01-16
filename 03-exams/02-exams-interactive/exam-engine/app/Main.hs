module Main where

import Exam.App
import Exam.Args

main :: IO ()
main = runExam =<< readExam
