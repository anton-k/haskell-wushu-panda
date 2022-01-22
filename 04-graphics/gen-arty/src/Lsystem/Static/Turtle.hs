module Lsystem.Static.Turtle where

import Graphics.Proc
import Control.Monad

data Cmd = Stroke Float | Move Float | Turn Float
  deriving (Show, Eq)

data TurtleCfg = TurtleCfg
  { turtle'start     :: P2
  , turtle'orient    :: Float
  , turtle'color     :: Col
  , turtle'weight    :: Float
  }

drawTutrtle :: TurtleCfg -> [Cmd] -> Draw
drawTutrtle TurtleCfg{..} commands = do
  strokeFill turtle'color
  strokeWeight turtle'weight
  foldM_ go (turtle'start, turtle'orient) commands
  where
    go (!start, !orient) = \case
      Stroke len  -> do
                      let next = start + len *^ e orient
                      line start next
                      pure (next, orient)
      Move len    -> pure (start + len *^ e orient, orient)
      Turn angle -> pure (start, angle + orient)

    rot a (P2 x y) = P2 (x * cos a - y * sin a) (x * sin a + y * cos a)

runTurtle :: P2 -> P2 -> Float -> [Cmd] -> IO ()
runTurtle winSize shift sc cmds = runProc $ def
  { procSetup = size winSize
  , procDraw  = const $ do
      background white
      translate (negate shift)
      scale (P2 sc sc)
      drawTutrtle cfg cmds
  }
  where
    cfg = defaultCfg center
    center = 0.5 *^ winSize

defaultCfg :: P2 -> TurtleCfg
defaultCfg center = TurtleCfg center 0.25 black 1




