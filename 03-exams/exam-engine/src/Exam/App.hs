module Exam.App(
  runExam,
  module X
) where

import Control.Monad.Writer.Strict
import Exam.Types as X

evalExam :: Exam -> IO ExamResult
evalExam = undefined

saveExam :: Exam -> ExamResult -> IO ()
saveExam = undefined

runExam :: Spec -> Exam -> IO ()
runExam Spec{..} Exam{..} = do
 spec'init $ desc'note exam'questions
 res <- execWriterT $ mapM_ query $ desc'data exam'questions
 spec'exit res
 where
  query qn = do
    n <- liftIO $ spec'ask qn
    step qn n


