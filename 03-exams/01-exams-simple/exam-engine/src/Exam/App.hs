module Exam.App(
  runExam,
  module X
) where

import Control.Monad.Writer.Strict
import Exam.Types as X
import Safe (atMay)

runExam :: Act -> Spec -> IO ()
runExam Act{..} (Spec person Exam{..}) = do
 act'init $ exam'greeting
 res <- execWriterT $ mapM_ query exam'questions
 act'exit (ExamResult person exam'name res)
 where
  query qn = do
    n <- liftIO $ act'ask qn
    step qn n

-----------------------------------------------------------------

type Run a = WriterT Score IO a

step :: Question -> Int -> Run ()
step qn ans = mapM_ tell $ toAnswer qn ans

toAnswer :: Question -> Int -> Maybe Score
toAnswer (Question (Desc _ opts)) n =
  fmap desc'data $ opts `atMay` n

