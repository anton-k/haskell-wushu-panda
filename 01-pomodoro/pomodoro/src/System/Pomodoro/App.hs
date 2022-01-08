module System.Pomodoro.App
    ( Pomodoro(..)
    , runPomodoro
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad (void, replicateM_)
import Data.Time
import System.ProgressBar
import System.Process (system)

-- | Pomodoro timer
data Pomodoro = Pomodoro
  { pomodoro'totalTime  :: NominalDiffTime   -- ^ how many seconds to work until the end
  , pomodoro'onExit     :: String            -- ^ command to run on exit
  , pomodoro'greet      :: Maybe String      -- ^ greeting line
  }

-- | Runs pomodoro timer
runPomodoro :: Pomodoro -> IO ()
runPomodoro Pomodoro{..} = do
  mapM_ putStrLn pomodoro'greet
  let seconds = floor pomodoro'totalTime
  pb <- newProgressBar defStyle 10 (Progress 0 seconds ())
  replicateM_ seconds (step pb)
  void $ system pomodoro'onExit
  where
    step bar = do
      wait
      incProgress bar 1

wait :: IO ()
wait = threadDelay 1000000 -- 1 second

