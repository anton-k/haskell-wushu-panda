module Exam.Args(
  readExam,
  defaultAct,
  exam1,
) where

import Exam.Types
import Data.Text (Text)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as M
import Text.Read
import qualified Data.List as L
import System.IO
import Options.Applicative
import Data.Yaml (decodeFileEither)
import Data.Yaml.Pretty (encodePretty, defConfig)

data Args = Args
  { args'input  :: FilePath
  , args'output :: FilePath
  , args'name   :: String
  }

fromArgs :: Args -> IO (Act, Spec)
fromArgs Args{..} = do
  eExam <- decodeFileEither args'input
  case eExam of
    Right exam -> do
      let act = saveTo args'output defaultAct
      pure (act, Spec (T.pack args'name) exam)
    Left err -> error $ show err

readExam :: IO (Act, Spec)
readExam = fromArgs =<< readArgs

readArgs :: IO Args
readArgs = execParser opts
  where
    opts = info (examArgs <**> helper)
      ( fullDesc
     <> progDesc "Exam engine"
     <> header "Exam engine executes examine and saves results" )

    examArgs = Args <$> getInput <*> getOutput <*> getName

    getInput = strOption
          ( long "input"
         <> short 'i'
         <> metavar "FILE"
         <> help "Input for exam config")

    getOutput = strOption
          ( long "output"
         <> short 'o'
         <> metavar "FILE"
         <> help "Save results of the exam")

    getName = strOption
          ( long "name"
         <> short 'n'
         <> metavar "NAME"
         <> help "Person name")

exam1 :: Exam
exam1 =
  Exam
    { exam'name = "Math exam (first grade)"
    , exam'questions =
      [ qn 2 2
      , qn 3 (-5)
      , qn 100 12
      ]
    , exam'greeting = "Welcome to math exam:"
    }
  where
    qn a b =
      Question
        (Desc (mconcat [text a, " + ", text b, " equals:"])
          [ Desc (text a) (score (Bin "Wrong") 1)
          , Desc (text b) (score (Bin "Wrong") 1)
          , Desc (text (a + b)) (score (Bin "Right") 1)
          ]
        )

saveTo :: FilePath -> Act -> Act
saveTo file x = x
  { act'exit = \res -> act'exit x res >> B.writeFile file (encodePretty defConfig res)
  }

defaultAct :: Act
defaultAct = Act{..}
  where
    act'init str = do
      line
      next
      T.putStrLn str
      next

    act'ask (Question (Desc title qs)) = do
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

    act'exit (ExamResult _ _ (Score m)) = do
      line
      next
      T.putStrLn "Results:"
      next
      mapM_ (\(Bin title, score) -> T.putStrLn $ mconcat ["  ", title, ": ", text score]) $ L.sortOn (negate . snd)  $ M.toList m
      next

text :: Show a => a -> Text
text = T.pack . show

next :: IO ()
next = putStrLn ""

line :: IO ()
line = putStrLn $ replicate 30 '-'

