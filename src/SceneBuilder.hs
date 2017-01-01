module SceneBuilder where

import Raytracer
import Types
import Predefined
import Control.Monad.State.Strict
import Data.Vect.Double
import Debug.Trace
import Control.Monad.Random

emptyScene :: Scene
emptyScene = Scene [] [] grey black

addShape :: Shape -> Scene -> Scene
addShape s sc = Scene (s : shapes sc) (lights sc) (ambientLight sc)
                      (backgroundColor sc)

addLight :: Point3 -> Double -> Color -> Scene -> Scene
addLight p r c s = Scene (light : shapes s) (p : lights s) (ambientLight s)
           (backgroundColor s)
  where
    light = Sphere p r (lightBulb c)

setAmbientLight :: Color -> Scene -> Scene
setAmbientLight c s = Scene (shapes s) (lights s) c (backgroundColor s)

setBackgroundColor :: Color -> Scene -> Scene
setBackgroundColor c s = Scene (shapes s) (lights s) (ambientLight s) c

sampleView = View (Vec3 0.0 0.0 0.0) 0.01 (mkNormal (Vec3 0.0 (-0.0) 0.5)) (mkNormal (Vec3 0.0 1.0 0.0)) (11.0/9.0 * pi/2)

yawCamera :: Double -> View -> View
yawCamera phi v = View (camera v) (clipPlane v) newFor newUp (fov v)
  where
    oldUp = up v
    oldForward = forward v
    right = oldForward `crossprod` oldUp
    newUp = mkNormal$rotate3' phi right (fromNormal oldUp)
    newFor = mkNormal$rotate3' phi right (fromNormal oldForward)

positionCamera :: Vec3 -> View -> View
positionCamera p v = View p (clipPlane v) (forward v) (up v) (fov v)


sampleWorld :: World
sampleWorld = World emptyScene sampleView 10 1 500 500
