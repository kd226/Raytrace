module Predefined where

import Types
import Data.Vect.Double

-- Predefined colors
red, green, blue, white, black, grey :: Color
red     = Vec3 1.0 0.3 0.3
green   = Vec3 0.3 1.0 0.3
blue    = Vec3 0.3 0.3 1.0
white   = Vec3 1.0 1.0 1.0
black   = Vec3 0.0 0.0 0.0
grey    = Vec3 0.02 0.02 0.02

--------------------------------------------------------------------------------
-- Procedural textures that take point return its material

flatRed, shinyred, greenshinyred, glass, flatWhite, flatBlack, flatGreen :: Point3 -> Material
flatRed _ =  Material [(0.9, Lambert red)]
shinyred _ = Material [(0.1, Lambert red),(0.8, Reflection 1.1 1.0 white)]
greenshinyred _ = Material [(0.1, Lambert green)
                           ,(0.8, Reflection 1.5 1.0 (Vec3 1.0 0.5 0.5))]
glass _ = Material [(1.0,Refraction 1.5 1.0 white)
                   ,(1.0, Reflection 1.5 1.0 white)]
flatBlack _ = Material [(0.8, Lambert black)]
flatWhite _ = Material [(1.0, Lambert white)]
flatBlue _ = Material [(0.9, Lambert blue)]
flatGreen _ = Material [(0.9, Lambert green)]
metal _ = Material [(0.7, Reflection 0.0 1.0 white)]

lightBulb :: Color -> Point3 -> Material
lightBulb c _ = Material [(1.0, Emmisive c)]

xor :: Bool -> Bool -> Bool
xor True b  = not b
xor False b = b

-- alternate 20x20x20 black and white cubes
checkedMatt :: Point3 -> Material
checkedMatt p@(Vec3 x y z) =
  let xeven = even (truncate (x / 20.0))
      zeven = even (truncate (z / 20.0))
      yeven = even (truncate (y / 20.0))
  in if xeven `xor` yeven `xor` zeven
      then flatWhite p
      else flatBlack p
