module Exam.Args(
  readExam,
  defaultSpec,
) where

import Exam.Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as M
import Text.Read
import qualified Data.List as L
import System.IO

readExam :: IO Exam
readExam = pure $
  Exam (Desc "Welcome to elementary math exam:"
    [ qn 2 2
    , qn 3 (-5)
    , qn 100 12
    ])
  where
    qn a b =
      Question
        (Desc (mconcat [text a, " + ", text b, " equals:"])
          [ Desc (text a) (score (Bin "Wrong") 1)
          , Desc (text b) (score (Bin "Wrong") 1)
          , Desc (text (a + b)) (score (Bin "Right") 1)
          ]
        )

defaultSpec :: Spec
defaultSpec = Spec{..}
  where
    spec'init str = do
      line
      next
      T.putStrLn str
      next

    spec'ask (Question (Desc title qs)) = do
      line
      next
      T.putStrLn title
      next
      mapM_ T.putStrLn $ zipWith showQuestion [1..] qs
      next
      res <- getAnswer (length qs)
      next
      pure res

    getAnswer maxSize = do
      prompt maxSize
      mn <- readMaybe @Int <$> getLine
      case mn of
        Just n | n > 0 && n <= maxSize -> pure $ pred n
        _                              -> getAnswer maxSize

    showQuestion n (Desc title _) = mconcat ["  ", T.pack (show n), ". ", title]

    prompt maxSize = do
      T.putStr $ mconcat ["Type answer from 1 to ", text maxSize, ": "]
      hFlush stdout

    spec'exit (ExamResult m) = do
      line
      next
      T.putStrLn "Results:"
      next
      mapM_ (\(Bin title, score) -> T.putStrLn $ mconcat ["  ", title, ": ", T.pack (show score)]) $ L.sortOn (negate . snd)  $ M.toList m
      next

text :: Show a => a -> Text
text = T.pack . show

next :: IO ()
next = putStrLn ""

line :: IO ()
line = putStrLn "---------------------------------"

