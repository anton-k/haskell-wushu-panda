-- | Parse command line arguments for pomodoro
module System.Pomodoro.Args(
  readPomodoro,
) where

import Options.Applicative
import System.Pomodoro.App (Pomodoro(..))

-- | Read pomodoro command line arguments
readPomodoro :: IO Pomodoro
readPomodoro = execParser opts
  where
    opts = info (pomodoroArgs <**> helper)
      ( fullDesc
     <> progDesc "Pomodoro timer"
     <> header "pomodoro timer executes command after so many minutes" )

    pomodoroArgs = (\mins cmd -> Pomodoro (fromMinutes mins) cmd (Just $ toGreeting mins))
      <$> getMinutes
      <*> getCmd

    getMinutes =
      option auto
          ( long "minutes"
         <> short 'm'
         <> help "How many minutes to wait"
         <> showDefault
         <> value 25
         <> metavar "INT")

    fromMinutes n = fromInteger $ 60 * n

    getCmd = strOption
          ( long "cmd"
         <> short 'c'
         <> metavar "SHELL-COMMAND"
         <> help "Command to execute on exit"
         <> showDefault
         <> value "echo \"done\"")

    toGreeting n = unwords ["Pomodoro timer:", show n, (if n == 1 then id else ( <> "s")) "minute"]

