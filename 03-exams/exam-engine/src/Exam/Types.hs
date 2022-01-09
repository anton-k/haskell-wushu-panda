module Exam.Types(
  Exam(..),
  ExamResult(..),
  Desc(..),
  Question(..),
  Score(..),
  score,
  Bin(..),
  Spec(..),
  Run,
  step,
) where

import Control.Monad.Writer.Strict

import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Map.Strict as M
import Safe

newtype Bin = Bin
  { bin'name :: Text
  }
  deriving (Show, Eq, Ord)

data Spec = Spec
  { spec'ask  :: Question -> IO Int
  , spec'init :: Text -> IO ()
  , spec'exit :: ExamResult -> IO ()
  }

data Exam = Exam
  { exam'questions :: Desc [Question]
  }

data Desc a = Desc
  { desc'note :: Text
  , desc'data :: a
  }

newtype Question = Question (Desc [Desc Score])

newtype Score = Score ExamResult

score :: Bin -> Int -> Score
score b n = Score $ ExamResult $ M.singleton b n

newtype ExamResult = ExamResult
  { examResult'scores :: Map Bin Int
  }

instance Semigroup ExamResult where
  (<>) (ExamResult a) (ExamResult b) = ExamResult $ M.unionWith (+) a b

instance Monoid ExamResult where
  mempty = ExamResult mempty

----------------------------------------------------------

type Run a = WriterT ExamResult IO a

step :: Question -> Int -> Run ()
step qn ans = mapM_ tell $ toAnswer qn ans

toAnswer :: Question -> Int -> Maybe ExamResult
toAnswer (Question (Desc _ opts)) n =
  fmap ((\(Score res) -> res) . desc'data) $ opts `atMay` n

