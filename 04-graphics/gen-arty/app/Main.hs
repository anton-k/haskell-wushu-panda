module Main where

import Graphics.Proc
import Lsystem.Static.Turtle
import Lsystem.Static.Example
import SolarSystem

main :: IO ()
main = runApp


drawHexaGospel = run (P2 30 180) 1.75 (fromTok (1/6) <$> hexaGospel 4)
drawDragon = run (P2 (-70) (-220)) 0.7 (fromTok (1/4) <$> dragon 13)
drawQuadroKochIsland = run (P2 (-30) 180) 1.3  (toCmd <$> quadroKochIsland 2)
drawKochIsland = run (P2 150 150) 1 (toCmd <$> kochIsland 3)


run = runTurtle (P2 700 700)


