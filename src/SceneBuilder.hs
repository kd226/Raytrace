{-|
Module      : SceneBuilder
Description : Building "World" to render
Copyright   : (c) Konrad Dobroś, 2017
License     : GPL-3
Maintainer  : dobros.konrad@gmail.com
Stability   : experimental
Portability : POSIX

This module contains functions used to build and manipulate everything that
is to be rendered. They should be used in state monad as with modify function,
however in case of diffrent need they are defined as pure modifiers.
-}

module SceneBuilder where

import Types
import Predefined
import Control.Monad.State.Strict
import Data.Vect.Double
import Debug.Trace
import Control.Monad.Random

-- | Just an empty scene without any objects, with 'grey' ambient light and 'black' background.
emptyScene :: Scene
emptyScene = Scene [] [] grey black

-- | Adds shape to a scene
addShape :: Shape -- ^ Shape that has to be added
         -> Scene -> Scene
addShape s sc = Scene (s : shapes sc) (lights sc) (ambientLight sc)
                      (backgroundColor sc)

-- | Adds spherical light to a scene
addLight :: Point3 -- ^ Center of a light source
         -> Double -- ^ Radius of a light sphere
         -> Color -- ^ Color of a light
         -> Scene -> Scene
addLight p r c s = Scene (light : shapes s) (p : lights s) (ambientLight s)
           (backgroundColor s)
  where
    light = Sphere p r (lightBulb c)

-- | Changes ambient light in a scene
setAmbientLight :: Color -- ^ New ambient light
                -> Scene -> Scene
setAmbientLight c s = Scene (shapes s) (lights s) c (backgroundColor s)

-- | Changes background color of a scene
setBackgroundColor :: Color -- ^ New background color
                   -> Scene -> Scene
setBackgroundColor c s = Scene (shapes s) (lights s) (ambientLight s) c

-- | Sample view with camera at (0,0,0), looking in (0,0,1) direction,
--   oriented along y and with field of view equal 110
sampleView = View (Vec3 0.0 0.0 0.0) 0.01 (mkNormal (Vec3 0.0 (-0.0) 0.5))
                  (mkNormal (Vec3 0.0 1.0 0.0)) (11.0/9.0 * pi/2)

-- | Yaws camera forward, that is rotates around vertical/right vector
yawCamera :: Double -- ^ Number of radians to rotate by
          -> View -> View
yawCamera phi v = View (camera v) (clipPlane v) newFor newUp (fov v)
  where
    oldUp = up v
    oldForward = forward v
    right = oldForward `crossprod` oldUp
    newUp = mkNormal$rotate3' phi right (fromNormal oldUp)
    newFor = mkNormal$rotate3' phi right (fromNormal oldForward)
-- | Moves camera to a new position
positionCamera :: Vec3 -- ^ New camera position
               -> View -> View
positionCamera p v = View p (clipPlane v) (forward v) (up v) (fov v)

-- | Sample 'World' with 'emptyScene' and 'sampleView' with ray depth of 10,
-- 1 frame to render and resolution of 500x500
sampleWorld :: World
sampleWorld = World emptyScene sampleView 10 1 500 500
