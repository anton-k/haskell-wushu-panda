-- | Draw a solar system in motion
module SolarSystem(
  runApp
) where

import Graphics.Proc

data Body = Body
  { body'size       :: Float
  , body'color      :: Col
  , body'satelites  :: [Satelite]
  }

data Satelite = Satelite
  { satelite'radius  :: Float
  , satelite'speed   :: Float
  , satelite'start   :: Float
  , satelite'body    :: Body
  }

--------------------------------------------------------------------------------
-- draw body

type Time = Float

drawBody :: Time -> P2 -> Body -> Draw
drawBody t center Body{..} = do
  strokeFill body'color
  circle body'size center
  mapM_ (drawSatelite t center) body'satelites

drawSatelite :: Time -> P2 -> Satelite -> Draw
drawSatelite t center (Satelite rad speed start body) =
  drawBody t (center + rad *^ e angle) body
  where
    angle = t * speed + start

--------------------------------------------------------------------------------

runApp :: IO ()
runApp = runProc $ def
  { procSetup  = setup
  , procDraw   = draw
  , procUpdate = update
  }

windowSizes = P2 600 600
windowCenter = 0.5 *^ windowSizes

-- standard functions

setup :: Pio Float
setup = do
	size windowSizes
	return 0

draw :: Time -> Draw
draw t = do
  background white
  drawBody t windowCenter solarSystem

update :: Time -> Pio Time
update t = return (t + 0.001)

--------------------------------------------------------------------------------

solarSystem :: Body
solarSystem = sun
  where
    sun = Body
      { body'size      = 40
      , body'color     = yellow
      , body'satelites = [mercury, venus, earth, mars]
          -- TODO: , jupiter, saturn, uranus, neptune]
      }

    mercury = Satelite
      { satelite'radius = 100
      , satelite'speed  = 2
      , satelite'start  = 0
      , satelite'body   =
          Body
            { body'size      = 7
            , body'color     = olive
            , body'satelites = []
            }
      }

    venus = Satelite
      { satelite'radius  = 135
      , satelite'speed   = 1
      , satelite'start  = 0.3
      , satelite'body    =
          Body
            { body'size      = 10
            , body'color     = orange
            , body'satelites = []
            }
      }

    earth = Satelite
      { satelite'radius  = 190
      , satelite'speed   = 1.6
      , satelite'start   = 1.3
      , satelite'body    =
          Body
            { body'size = 12
            , body'color = blue
            , body'satelites = [moon]
            }
      }
      where
        moon = Satelite
          { satelite'radius = 20
          , satelite'speed  = 7
          , satelite'start  = 0
          , satelite'body   =
              Body
                { body'size  = 3
                , body'color = gray
                , body'satelites = []
                }
          }

    mars = Satelite
      { satelite'radius   = 250
      , satelite'speed    = 1.3
      , satelite'start    = 1.9
      , satelite'body     =
          Body
            { body'size      = 12
            , body'color     = purple
            , body'satelites = [deimos, phobos]
            }
      }
      where
        deimos = Satelite
          { satelite'radius = 18
          , satelite'speed  = 4
          , satelite'start    = 0.9
          , satelite'body   =
              Body
                { body'size  = 3
                , body'color = green
                , body'satelites = []
                }
          }

        phobos = Satelite
          { satelite'radius = 27
          , satelite'speed  = 5
          , satelite'start  = 0.1
          , satelite'body   =
              Body
                { body'size  = 2
                , body'color = purple
                , body'satelites = []
                }
          }
