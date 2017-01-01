{-# LANGUAGE BangPatterns #-}
module Main where

import Raytracer
import Rendering
import Predefined
import Data.Word
import Data.Vect.Double
import SceneBuilder
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Random
import Data.List (foldl')
import Types


buildScene :: State Scene ()
buildScene = do
  -- addLight (Spotlight (Vec3 0.0 3.0 3.0) lightColor)
  modify $ addLight (Vec3 0.0 1.2 1.7) 0.11 white

  -- addShape (Plane (mkNormal (Vec3 0.0 1.0 0.0)) (-0.0) (lightBulb white))
  modify $ addShape (Plane (mkNormal (Vec3 0.0 1.0 0.0)) (0.0) flatWhite)
  modify $ addShape (Plane (mkNormal (Vec3 0.0 (1.0) 0.0)) (1.7) flatWhite)
  modify $ addShape (Plane (mkNormal (Vec3 1.0 0.0 0.0)) (1.0) flatBlue)
  modify $ addShape (Plane (mkNormal (Vec3 1.0 0.0 0.0)) (-1.0) flatRed)
  modify $ addShape (Plane (mkNormal (Vec3 0.0 0.0 (1.0))) (2.5) flatWhite)
  modify $ addShape (Plane (mkNormal (Vec3 0.0 0.0 (1.0))) (-0.1) flatGreen)

  modify $ addShape (Sphere (Vec3 0.5 (0.3) 1.7) 0.3 glass)
  -- modify $ addShape (Sphere (Vec3 (-0.5) (1.2) 1.7) 0.2 glass)
  modify $ addShape (Sphere (Vec3 (-0.7) (0.3) 2.2) 0.3 greenshinyred)
  modify $ addShape (Sphere (Vec3 (-0.9) (0.1) 1.8) 0.1 flatRed)
  modify $ addShape (Sphere (Vec3 (-0.1) (0.2) 2.3) 0.2 metal)
  -- addShape (Sphere (Vec3 0.2 (0.4) 0.6) 0.4 shinyred)
  modify $ setAmbientLight black

buildView :: State View ()
buildView = do
  -- modify $ yawCamera (pi/64.0)
  modify $ positionCamera (Vec3 0.0 0.8 0.0)

buildWorld :: State World ()
buildWorld = do
  let s = execState buildScene emptyScene
  let v = execState buildView sampleView
  let rd = 5

  -- s <- get
  put (World s v rd 2 2732 1536)

main :: IO ()
main = do
  let world = execState buildWorld sampleWorld
  {--world `seq` --}
  -- let bitmap = runReader renderFrames world
  -- savePPM "test.ppm" bitmap

  runReaderT (renderAll "test") world

-- main :: IO ()
-- main = do
--   let ios = evalState (buildWorld >> sampleEvery 500 500) sampleWorld
--   let ii = foldl' (>>) (putStrLn "") ios
--   ii
--   putStrLn "Succes"
