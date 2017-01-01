{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Rendering (renderFrames,savePPM, renderAll) where

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

data Bitmap a = Bitmap [a] Int Int -- deriving (NFData)

instance (NFData a) => NFData (Bitmap a) where
  rnf (Bitmap pix w h) = (rnf pix) `seq` rnf w `seq` rnf h

instance NFData Vec3 where
  rnf (Vec3 x y z) = rnf x `seq` rnf y `seq` rnf z

instance NFData Ray where
  rnf (Ray base dir) = rnf base `seq` rnf dir

instance NFData Normal3 where
  rnf a = rnf (fromNormal a)

pixels :: Bitmap a -> [a]
pixels (Bitmap a _ _ ) = a

widthB :: Bitmap a -> Int
widthB (Bitmap _ w _) = w

heightB :: Bitmap a -> Int
heightB (Bitmap _ _ h) = h


createRays :: Int -> Int -> Reader World [Ray]
createRays w h = do
  v <- asks view
  let fovw = fov v
  let foh = fovw / fromIntegral w * fromIntegral h
  let grid = [ Vec3 (fromIntegral x) (fromIntegral y) (clipPlane v)
               | y <- [1..h], x <- [1..w]]
  let scaleX = -2*(clipPlane v) * sin (fovw/2.0) / fromIntegral w
  let scaleY = -2*(clipPlane v) * sin (foh/2.0) / fromIntegral h
  let scaledGrid = map (pointwise (Vec3 scaleX scaleY 1.0).(&+) (Vec3 (fromIntegral w/(-2.0)) (fromIntegral h/(-2.0)) 0.0)) grid
  let left = (forward v) `crossprod` (up v)
  let projection = Mat3 (fromNormal left) (fromNormal (up v)) (fromNormal (forward v))
  let projectedGrid = map (lmul projection) scaledGrid
  return (map (Ray (camera v).mkNormal) projectedGrid)

renderPart :: [Ray] -> ReaderT World (Rand StdGen) [Color]
renderPart r = do
  dip <- asks rayDepth
  mapM (raytrace 1.0 dip) r

renderFrame :: [[Ray]] -> Int -> StdGen  -> Reader World [Color]
renderFrame chunked !size stdg = do
-- chunks = 4
-- chunked = chunksOf (size `div` chunks) rs
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

renderFrames :: Reader World (Bitmap Color)
renderFrames = do
  w <- asks width
  h <- asks height
  world <- ask
  let chunks = 24
  rays <- createRays w h
  let chunked = chunksOf (w*h `div` chunks) $ rays
  frames <- asks samples
  let stdg = mkStdGen 0
  let generators = evalRand (replicateM frames getSplit) stdg
  let calcPixs = chunked `seq` map (renderFrame chunked chunks) generators
  let results = runReader (sequence calcPixs) world -- `using` evalList rpar
  let pixsf = (map (&* (1.0/fromIntegral frames))
             .foldl' addFrame (replicate (w*h) black) $(results {--`using` evalList rdeepseq --}))
  return $ Bitmap (pixsf) w h

addFrame :: [Color] -> [Color] -> [Color]
addFrame soFar add = soFar `deepseq` zipWith (&+) soFar add

renderAll :: String -> ReaderT World IO ()
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

savePPM :: FilePath -> Bitmap Color -> IO ()
savePPM f bm = writeFile fpmm stringifyPPM
  where
    fpmm = f ++ ".ppm"
    stringifyPPM = "P3\n" ++ (show.widthB$bm) ++ " " ++ (show.heightB$bm)
      ++ "\n255\n" ++ makeASCII
    makeASCII = concatMap ((++ " ").show) wordmap
    wordmap = pixels.toWords$bm
