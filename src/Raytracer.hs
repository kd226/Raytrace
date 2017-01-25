{-|
Module      : Raytracer
Description : Core
Copyright   : (c) Konrad Dobroś, 2017
License     : GPL-3
Maintainer  : dobros.konrad@gmail.com
Stability   : experimental
Portability : POSIX

This is an entry point to internals of Raytrace.
-}

{-# LANGUAGE BangPatterns #-}

module Raytracer (raytrace, roots) where

import Data.Vect.Double
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Random
import Debug.Trace
import Data.List (foldl1')
import Types
import Predefined

--------------------------------------------------------------------------------
-- Intersection calculations

type Time = Double

positionFromT :: Ray -> Time -> Vec3
positionFromT Ray{base = b, direction = dir} t = b &+ (&*) (fromNormal dir) t



-- Collision precision to prevent any rays that start inside shape
-- (due to rounding errors) from colliding
epsilon :: Double
epsilon = 0.0001

lightEpsilon :: Double
lightEpsilon = 0.0001

-- Calculate the roots of the equation a * x^2 + b * x + c = 0
roots :: Double -> Double -> Double -> [Double]
roots a b c =
  let discriminant = b*b - 4*a*c
  in if discriminant < 0.0
    then []
    else [ 0.5 * (-b + sqrt discriminant), 0.5 * (-b - sqrt discriminant)]

intersect :: Ray -> Shape -> [(Time, Intersection)]
intersect ray@Ray{base = base, direction = dir} (Sphere center rad materialfn) =
  let a = lensqr dir
      b = 2 * ( fromNormal dir &. (base &- center))
      c = lensqr (base &- center) - rad^2
      times = filter (> epsilon) (roots a b c)
      normal_at_time t = mkNormal (positionFromT ray t &- center)
      intersection_at_time t = Intersection (normal_at_time t)
        (positionFromT ray t) ray (materialfn (positionFromT ray t))
  in map (\t -> (t,intersection_at_time t)) times

intersect ray@Ray{base = base, direction = dir} (Plane normal d materialfn) =
  let vd = normal &. dir
      v0 = (d - (fromNormal normal &. base))
  in if vd == 0 then []
     else let t = v0 / vd
              hitpoint = positionFromT ray t
          in [ (t,
          Intersection (if vd > 0 then mkNormal.neg.fromNormal$normal else normal) hitpoint ray (materialfn hitpoint)) | t > epsilon]

-- Extract the closest intersection from a list
closest :: [ (Time,Intersection) ] -> Intersection
closest xs = let select_nearest (t1,i1) (t2,i2) = if t1<t2 then (t1,i1) else (t2,i2)
           in snd (foldl1' select_nearest xs)



--------------------------------------------------------------------------------
-- Local lighting model

lightsource :: BRDF -> Bool
lightsource (Emmisive c) = True
lightsource _ = False

-- Is the light directly visible from point?
pointIsLit :: Point3 -> Light -> Reader World Bool
pointIsLit point lightpos =
  do
    s <- asks scene
    let path = lightpos &- point
    let time_at_light = len path
    let ray = Ray point (mkNormal path)
    let hits = concatMap (intersect ray) (shapes s)
    let first = closest hits
    return (any lightsource.snd.unzip.brdfs.material$first)

-- --------------------------------------------------------------------------------
-- -- Main path tracing functions
--
overallLighting :: Double -> Int -> Intersection -> ReaderT World (Rand StdGen) Color
overallLighting importance depth hit =
    do
      s <- asks scene
      let brs = brdfs (material hit)
      let calcColor (x,y) = fmap (&* x) (calculateBRDF y (importance*x) depth hit) --
      colors <- mapM calcColor brs
      let color = foldr addMix black colors
      return $! clamp color

-- | Send a ray and return its color.
raytrace :: Double -- ^ Importance of ray
         -- (when importance is sufficently low raytracing stops and returns black).
         -> Int -- ^ Depth to trace, that is raytracing stops when depth reaches 0.
         -> Ray -- ^ Ray to send.
         -> ReaderT World (Rand StdGen) Color -- ^ Color of this ray.
raytrace _ 0 _ = return black
raytrace importance depth ray =
  if importance < lightEpsilon
    then return black
    else do
      s <- asks scene
      let hits = concatMap (intersect ray) (shapes s)
      if null hits
        then return (backgroundColor s)
        else overallLighting importance depth (closest hits)

--------------------------------------------------------------------------------
-- BRDF functions

-- Random double from range (0.0, 1.0)
evalRoll :: Rand StdGen Double
evalRoll = getRandomR (0.0,1.0)

-- Helper to calculate Schlicks approximation of Fresnels equation
schlick :: Double -> Double  -> Normal3 -> Normal3 -> Double
schlick ind1 ind2 normal viewdir = r_0 + (1.0 - r_0)*(1 - abs (normal &. viewdir))^5
  where
    r_0 = (ind1 - ind2)^2/ (ind1 + ind2)^2

-- Helper to calculate the diffuse light at the surface normal, given
-- the light direction (from light source to surface)
diffuseCoeff :: Normal3 -> Normal3 -> Double
diffuseCoeff lightDir normal = max 0.0 (lightDir &. normal)

-- Helper to rotate coordinates from z to normal
rotateWorld :: Normal3 -> Normal3 -> Normal3
rotateWorld z norm =
  case fromNormal norm of
    (Vec3 0.0 1.0 0.0) -> z
    (Vec3 0.0 (-1.0) 0.0) -> flipNormal z
    a -> mkNormal.flip rmul matrix.fromNormal$z where
      matrix = Mat3 constantV (fromNormal norm) produced
      constantV = normalize$crossprod (Vec3 0.0 1.0 0.0) a
      produced = crossprod a constantV

calculateBRDF :: BRDF -> Double -> Int -> Intersection -> ReaderT World (Rand StdGen) Color
-- Lambertian BRDF
calculateBRDF (Lambert c) importance depth intersection = do
  -- Calculate point light sources
  l <- asks $lights.scene
  w <- ask
  let lits = runReader (filterM (pointIsLit.point$intersection) l) w
  -- let dirToLight x = mkNormal (x &- point intersection)
  let lightDirs = map (\x -> mkNormal (x &- point intersection)) lits
  colors <- mapM (raytrace importance 1.Ray (point intersection)) lightDirs
  let coefficients = map (diffuseCoeff (normalI intersection)) lightDirs
  let colorL = foldr addMix black $ zipWith (&*) colors coefficients
  -- Calculate monte carlo diffusion
  r_1 <- lift evalRoll
  r_2 <- lift evalRoll
  let phi = 2*r_1 - 1
  let x = phi * cos (2*pi*r_2)
  let y = cos (asin phi)
  let z = phi * sin (2*pi*r_2)
  -- let randomDir = mkNormal (Vec3 x y z)
  let normalHit = if (normalI intersection &. (direction.ray$intersection)) > 0
      then flipNormal.normalI$intersection else normalI intersection
  let randomDir = mkNormal (Vec3 x y z)
  -- let outRayDir = (normalI intersection)
  let outRayDir = rotateWorld randomDir normalHit -- (normalI$intersection)
  let outRay = Ray (point intersection) outRayDir
  let coeff = diffuseCoeff outRayDir normalHit -- (normalI intersection)
  let count = length coefficients + 1
  -- We need to get weighted average, so sum of coefficients doesn`t exceed 1
  pathColor <- raytrace (importance*coeff) (depth-1) outRay

  return $ c `prodMix` ((pathColor &+ colorL)&* (1.0/fromIntegral count))
-- Perfect reflection BRDF
calculateBRDF (Reflection inRI outRI c) importance depth intersection =
  let
    inRayDir = flipNormal.direction.ray$intersection
    outRayDir = reflect' (normalI intersection) inRayDir
    outRay = Ray (point intersection) (normalI intersection)
    fresnel = schlick inRI outRI (normalI intersection) inRayDir
  in fmap (pointwise (c &* fresnel)) (raytrace (fresnel*importance) (depth-1) outRay)
-- Perfect refraction BRDF
calculateBRDF (Refraction inRI outRI c) importance depth intersection =
  let
    inRayDir = flipNormal.direction.ray$intersection
    normal = normalI intersection
    normal2 = (if normal &. inRayDir <= 0 then flipNormal else id) normal
    eta = if normal &. inRayDir > 0 then outRI/inRI else inRI/outRI
    outRayDir = refract' eta normal2 inRayDir
    outRay = Ray (point intersection) outRayDir
    fresnelT = 1.0 - schlick inRI outRI (normalI intersection) inRayDir
  in fmap (pointwise (c &* fresnelT)) (raytrace (fresnelT*importance) (depth-1) outRay)
-- Emmisive BRDF (uniform)
calculateBRDF (Emmisive c) importance depth intersection = return c
