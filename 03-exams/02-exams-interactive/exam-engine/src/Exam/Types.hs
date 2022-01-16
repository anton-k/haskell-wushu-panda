module Exam.Types(
  Exam(..),
  Desc(..),
  Question(..),
  Score(..),
  score,
  Bin(..),
  Spec(..),
  ExamResult(..),
) where

import Data.Aeson
import Data.Aeson.TH (deriveJSON)

import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics
import qualified Data.Map.Strict as M
import Exam.Utils.Aeson

data Desc a = Desc
  { desc'title :: Text
  , desc'data  :: a
  }
  deriving stock (Show)
$(deriveJSON dropPrefixOptions ''Desc)

newtype Bin = Bin
  { bin'name :: Text
  }
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype Question = Question (Desc [Desc Score])
  deriving newtype (Show, ToJSON, FromJSON)

newtype Score = Score (Map Bin Int)
  deriving newtype (Show, ToJSON, FromJSON)

score :: Bin -> Int -> Score
score b n = Score $ M.singleton b n

data Spec = Spec
  { spec'name   :: Text
  , spec'file   :: FilePath
  , spec'exam   :: Exam
  }

instance Semigroup Score where
  (<>) (Score a) (Score b) = Score $ M.unionWith (+) a b

instance Monoid Score where
  mempty = Score mempty

data Exam = Exam
  { exam'name      :: Text
  , exam'questions :: [Question]
  , exam'greeting  :: Text
  }
  deriving stock (Show)
$(deriveJSON dropPrefixOptions ''Exam)

data ExamResult = ExamResult
  { examResult'name    :: Text
  , examResult'exam    :: Text
  , examResult'scores  :: Score
  }
$(deriveJSON dropPrefixOptions ''ExamResult)
