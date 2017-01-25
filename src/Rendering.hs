{-|
Module      : Rendering
Description : Rendering functions
Copyright   : (c) Konrad Dobroś, 2017
License     : GPL-3
Maintainer  : dobros.konrad@gmail.com
Stability   : experimental
Portability : POSIX

This module contains functions used to render result image and save it.
-}


{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Rendering (Bitmap, savePPM, renderAll, renderAllNotified) where

import Raytracer
import SceneBuilder
-- import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Random
import Data.Vect.Double
import Data.Word
import Debug.Trace
import Data.List.Split (chunksOf)
import Data.List (foldl')
import Types
import Predefined
import Control.Parallel.Strategies
import Control.DeepSeq

import Control.Monad.Par

type RGBA = Vec4
-- | Generic Bitmap type with fast acces to it's size
data Bitmap a = Bitmap { pixels::[a] -- ^ List of pixels of type a
                       , widthB::Int -- ^ Width of bitmap
                       , heightB::Int -- ^ Height of bitmap
                       }

-- Instances of NFData in order to implement parallelism
instance (NFData a) => NFData (Bitmap a) where
  rnf (Bitmap pix w h) = rnf pix `seq` rnf w `seq` rnf h

instance NFData Vec3 where
  rnf (Vec3 x y z) = rnf x `seq` rnf y `seq` rnf z

instance NFData Ray where
  rnf (Ray base dir) = rnf base `seq` rnf dir

instance NFData Normal3 where
  rnf a = rnf (fromNormal a)

createRays :: Int -> Int -> Reader World [Ray]
createRays w h = do
  v <- asks view
  let fovw = fov v
  let foh = fovw / fromIntegral w * fromIntegral h
  let grid = [ Vec3 (fromIntegral x) (fromIntegral y) (clipPlane v)
               | y <- [1..h], x <- [1..w]]
  let scaleX = -2* clipPlane v  * sin (fovw/2.0) / fromIntegral w
  let scaleY = -2* clipPlane v * sin (foh/2.0) / fromIntegral h
  let scaledGrid = map (pointwise (Vec3 scaleX scaleY 1.0).(&+) (Vec3 (fromIntegral w/(-2.0)) (fromIntegral h/(-2.0)) 0.0)) grid
  let left = forward v `crossprod` up v
  let projection = Mat3 (fromNormal left) (fromNormal (up v)) (fromNormal (forward v))
  let projectedGrid = map (lmul projection) scaledGrid
  return (map (Ray (camera v).mkNormal) projectedGrid)

renderPart :: [Ray] -> ReaderT World (Rand StdGen) [Color]
renderPart r = do
  dip <- asks rayDepth
  mapM (raytrace 1.0 dip) r

renderFrame :: [[Ray]] -> Int -> StdGen  -> Reader World [Color]
renderFrame chunked !size stdg = do
  let list = evalRand (replicateM size getSplit) stdg
  w <- ask
  let tmp x = runReaderT (renderPart x) w
  let c1 = map (evalRand.tmp) chunked
  let c2 = zipWith ($) c1 list
  let cf = concat c2
  return $ runPar $ do
    cls <- spawnP cf
    evcolors <- mapM spawnP c2
    colors <- mapM get evcolors
    return $ concat colors

-- | Renders all frames specified by "World" and returns them as average in color "Bitmap"
renderFrames :: Reader World (Bitmap Color)
renderFrames = do
  w <- asks width
  h <- asks height
  world <- ask
  let chunks = 32 -- Experimentally chosen number of divisions for fastest parallel performance
  rays <- createRays w h
  let chunked = chunksOf (w*h `div` chunks) rays
  frames <- asks samples
  let stdg = mkStdGen 0
  let generators = evalRand (replicateM frames getSplit) stdg
  let calcPixs = chunked `seq` map (renderFrame chunked chunks) generators
  let results = runReader (sequence calcPixs) world
  let pixsf = map (&* (1.0/fromIntegral frames))
             .foldl' addFrame (replicate (w*h) black) $ results
  return $ Bitmap pixsf w h

addFrame :: [Color] -> [Color] -> [Color]
addFrame soFar add = soFar `deepseq` zipWith (&+) soFar add

-- | Renders all frames specified by "World" and writes line to IO each time a
-- frame is rendered.
renderFramesNotified :: ReaderT World IO (Bitmap Color)
renderFramesNotified = do
  w <- asks width
  h <- asks height
  world <- ask
  let chunks = 32
  let rays = runReader (createRays w h) world
  let chunked = chunksOf (w*h `div` chunks)  rays
  frames <- asks samples
  let stdg = mkStdGen 0
  let generators = evalRand (replicateM frames getSplit) stdg
  let calcPixs = chunked `seq` map (renderFrame chunked chunks) generators
  let results = runReader (sequence calcPixs) world
  pixsf <- liftIO $ fmap (map (&* (1.0/fromIntegral frames)).snd).foldM addFrameN (0,replicate (w*h) black) $(results)
  return $ Bitmap pixsf w h

addFrameN :: (Int, [Color]) -> [Color] -> IO (Int, [Color])
addFrameN (!number,soFar) add = do
  soFar `deepseq` putStrLn $ "Frame number " ++ show (number+1) ++ " started."
  return  (number+1,zipWith (&+) soFar add)

-- | Render all frames specified in World and save them to file, notifing each
-- time a frame is rendered.
renderAllNotified :: String -- ^ Name of the file to save to
                  -> ReaderT World IO () -- ^ Returned IO in ReaderT monad
renderAllNotified name = do
  liftIO $ putStrLn "Start"
  bitmap <- renderFramesNotified
  bitmap `deepseq` liftIO $ savePPM name bitmap
  liftIO $ putStrLn "Succes"

-- | Renders all frames specified in "World" and save them to a file
renderAll :: String  -- ^ Name of the file to save to
          -> ReaderT World IO () -- ^ Returned IO in ReaderT monad
renderAll name = do
  world <- ask
  let bitmap = runReader renderFrames world
  bitmap `deepseq` liftIO $ savePPM name bitmap -- force full bitmap evaluation before saving it
  -- could be propably written as savePPM name $!! bitmap

toWords :: Bitmap Color -> Bitmap Word8
toWords b = Bitmap words8 (heightB b) (widthB b)
  where
    words8 = concatMap reword (pixels b)
    reword v = map calculateD [_1 v, _2 v, _3 v]
    calculateD d = truncate (d * fromIntegral mb)
    mb = maxBound :: Word8

-- | Save color "Bitmap" to .pmm file.
savePPM :: FilePath -- ^ Path to a file
        -> Bitmap Color -- ^ Bitmap to save
        -> IO () -- ^ Returned IO monad
savePPM f bm = writeFile fpmm stringifyPPM
  where
    fpmm = f ++ ".ppm"
    stringifyPPM = "P3\n" ++ (show.widthB$bm) ++ " " ++ (show.heightB$bm)
      ++ "\n255\n" ++ makeASCII
    makeASCII = concatMap ((++ " ").show) wordmap
    wordmap = pixels.toWords$bm
