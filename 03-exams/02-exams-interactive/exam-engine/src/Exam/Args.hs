module Exam.Args(
  readExam,
) where

import Exam.Types
import qualified Data.Text as T
import Options.Applicative
import Data.Yaml (decodeFileEither)

data Args = Args
  { args'input  :: FilePath
  , args'output :: FilePath
  , args'name   :: String
  }

fromArgs :: Args -> IO Spec
fromArgs Args{..} = do
  eExam <- decodeFileEither args'input
  case eExam of
    Right exam -> pure (Spec (T.pack args'name) args'output exam)
    Left err -> error $ show err

readExam :: IO Spec
readExam = fromArgs =<< execParser opts
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
