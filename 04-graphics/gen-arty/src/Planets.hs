module Planets
    ( runApp
    ) where

import Graphics.Proc

runApp = runProc $ def
  { procSetup  = setup
  , procDraw   = draw
  , procUpdate = update
  }

-- constants

rad = 55
sizes  = (P2 300 300)
center = 0.5 *^ sizes

-- standard functions

setup :: Pio Float
setup = do
  size sizes
  return 0

draw :: Float -> Draw
draw t = do
  background (grey 255)
  drawSun
  drawPlanet t

update :: Float -> Pio Float
update t = return (t + 0.01)

-- drawing

drawSun = do
  fill yellow
  ellipse center 30

drawPlanet t = do
  fill blue
  ellipse p 12
  where
    p = center + rad *^ P2 (cos t) (sin t)

