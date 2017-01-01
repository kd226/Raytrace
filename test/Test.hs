module Main where

import Raytracer
import Rendering
import Data.Word
import Data.Vect.Double
import SceneBuilder
import Control.Monad.State

testConsole :: State World (Bitmap Color)
testConsole = do
  w <- get
  let s = execState buildScene emptyScene
  let v = sampleView
  let rd = 2
  -- s <- get
  put (World s v rd)
  render 100 100

buildScene :: State Scene ()
buildScene = do
  addLight (Spotlight (Vec3 0.0 6.0 4.0) white)
  addShape (Sphere (Vec3 0.0 0.0 6.0) 1.0 greenshinyred)

main :: IO ()
main = do
  savePPM "test.ppm" (evalState testConsole sampleWorld)
