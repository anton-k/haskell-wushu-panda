module Lsystem.Static.Example where

import Lsystem.Static.Gen
import Lsystem.Static.Turtle

toCmd :: Char -> Cmd
toCmd = \case
  'F' -> Stroke 5
  'f' -> Move 5
  '-' -> Turn (-1/4)
  '+' -> Turn (1/4)

kochIsland :: Int -> String
kochIsland = gen rules seed
  where
    rules = \case
      'F' -> "F-F+F+FF-F-F+F"
      ch  -> pure ch

    seed = "F-F-F-F"


quadroKochIsland :: Int -> String
quadroKochIsland = gen rules seed
  where
    rules = \case
      'F' -> "F+f-FF+F+FF+Ff+FF-f+FF-F-FF-Ff-FFF"
      'f' -> "ffffff"
      ch  -> pure ch

    seed = "F+F+F+F"


data Tok = Fl | Fr | Plus | Minus
  deriving (Show, Eq)

fromTok :: Float -> Tok -> Cmd
fromTok angle = \case
  Fl -> Stroke 5
  Fr -> Stroke 5
  Minus -> Turn (negate angle)
  Plus -> Turn angle

dragon :: Int -> [Tok]
dragon = gen rules seed
  where
    rules = \case
      Fl -> [Fl, Plus, Fr, Plus]
      Fr -> [Minus, Fl, Minus, Fr]
      ch -> [ch]

    seed = [Fl]

hexaGospel :: Int -> [Tok]
hexaGospel = gen rules seed
  where
    seed = [Fl]

    rules = \case
      Fl -> [Fl, Plus, Fr, Plus, Plus, Fr, Minus, Fl, Minus, Minus, Fl, Fl, Minus, Fr, Plus]
      Fr -> [Minus, Fl, Plus, Fr, Fr, Plus, Plus, Fr, Plus, Fl, Minus, Minus, Fl, Minus, Fr]
      ch -> [ch]



