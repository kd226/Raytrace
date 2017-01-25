{-|
Module      : Predefined
Description : Predefined types
Copyright   : (c) Konrad Dobroś, 2017
License     : GPL-3
Maintainer  : dobros.konrad@gmail.com
Stability   : experimental
Portability : POSIX

This module contains simple predefined colors and procedural texture functions.
-}

module Predefined ( -- * Predefined colors
      red, green, blue, white, black, grey
      -- * Procedural textures that take point return its material
      -- ** Flat materials are 90% diffusive, shiny have 10% diffusion and 80% reflectance
      , flatRed, shinyRed
      , greenShinyRed, glass, flatWhite, flatBlack
      , lightBulb, checkedMatt, flatBlue, flatGreen, mirror) where

import Types
import Data.Vect.Double

red, green, blue, white, black, grey :: Color
red     = Vec3 1.0 0.3 0.3
green   = Vec3 0.3 1.0 0.3
blue    = Vec3 0.3 0.3 1.0
white   = Vec3 1.0 1.0 1.0
black   = Vec3 0.0 0.0 0.0
grey    = Vec3 0.02 0.02 0.02

--------------------------------------------------------------------------------
--  Procedural textures that take point return its material

flatRed, shinyRed, greenShinyRed, glass, flatWhite, flatBlack :: Point3 -> Material
flatBlue, flatGreen, mirror :: Point3 -> Material
flatRed _ =  Material [(0.9, Lambert red)]
shinyRed _ = Material [(0.1, Lambert red),(0.8, Reflection 1.1 1.0 white)]
greenShinyRed _ = Material [(0.1, Lambert green)
                           ,(0.8, Reflection 1.5 1.0 (Vec3 1.0 0.5 0.5))]
-- | Glass is fully reflective and refractive
-- (conservation of energy is calculated from Fresnels law).
glass _ = Material [(1.0, Refraction 1.5 1.0 white)
                   ,(1.0, Reflection 1.5 1.0 white)]
flatBlack _ = Material [(0.8, Lambert black)]
flatWhite _ = Material [(1.0, Lambert white)]
flatBlue _ = Material [(0.9, Lambert blue)]
flatGreen _ = Material [(0.9, Lambert green)]

mirror _ = Material [(0.8, Reflection 0.0 1.0 (Vec3 1.0 1.0 1.0))]

-- | Creates a light material with specified color and 100% of emmision.
lightBulb :: Color -> Point3 -> Material
lightBulb c _ = Material [(1.0, Emmisive c)]

xor :: Bool -> Bool -> Bool
xor True b  = not b
xor False b = b

-- | Alternate 20x20x20 black and white cubes.
checkedMatt :: Point3 -> Material
checkedMatt p@(Vec3 x y z) =
  let xeven = even (truncate (x / 20.0))
      zeven = even (truncate (z / 20.0))
      yeven = even (truncate (y / 20.0))
  in if xeven `xor` yeven `xor` zeven
      then flatWhite p
      else flatBlack p
