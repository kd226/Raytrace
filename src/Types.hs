{-|
Module      :
Description : Raytrace types
Copyright   : (c) Konrad Dobroś
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

Module containing types used in Raytace library.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
module Types where

import Data.Vect.Double
import Control.Monad.Random

--------------------------------------------------------------------------------
-- Basic types
type Color = Vec3

clamp :: Color -> Color
clamp (Vec3 r g b) = Vec3 (clampDouble r) (clampDouble g) (clampDouble b)
                where clampDouble f = max 0.0 (min 1.0 f)

addMix :: Color -> Color -> Color
addMix a b= a &+ b

subMix :: Color -> Color -> Color
subMix a b = a &- b

prodMix :: Color -> Color -> Color
prodMix = pointwise

type Point3 = Vec3

-- | BRDF models with their specific descriptions
data BRDF = Lambert { color::Color -- ^ Color of lambertian surface
                    } -- ^ Lambertian reflectance BRDF
          | Reflection { inRI::Double -- ^ Index of refraction of material
                                      -- inside the shape
                       , outRI::Double -- ^ Index of refraction of material
                                       -- outside the shape
                       , color::Color  -- ^ Change of color of reflected rays
                       } -- ^ Perfect reflection BRDF
          | Refraction { inRI::Double -- ^ Index of refraction of material
                                      -- inside the shape
                       , outRI::Double -- ^ Index of refraction of material
                                       -- outside the shape
                       , color::Color -- ^ Change of color of refracted rays
                       } -- ^ Perfect refraction BRDF
          | Emmisive { color::Color -- ^ Color of emmited light
                     } -- ^ Generic emmisive BRDF

-- | Material of specific point
data Material = Material{ brdfs::[(Double,BRDF)] -- ^ Pairs of and brdfs.
                         -- In order for material to be realistic sum of must be
                         -- lower or equal to 1.
                        }

-- | Ray in three dimensions
data Ray = Ray { base::Point3 -- ^ Point from which the ray starts
               , direction::Normal3 -- ^ Direction in which ray is pointing
               }
-- | All information about intersection
data Intersection = Intersection { normalI::Normal3 -- ^ Normal of intersection.
                                 , point::Point3 -- ^ Point of intersection.
                                 , ray::Ray -- ^ Ray which intersected.
                                 , material::Material -- ^ Material at point of
                                   -- intersection.
                                 }

-- | Scene description
data Scene = Scene { shapes::[Shape]
                   , lights::[Light]
                   , ambientLight::Color
                   , backgroundColor::Color
                   }

-- | View description
data View = View { camera::Point3
                 , clipPlane::Double
                 , forward::Normal3
                 , up::Normal3
                 , fov::Double
                 }

-- | Description of whole Raytrace world with all settings needed to render
-- the scene.
data World = World !Scene !View !Int !Int !Int !Int
scene :: World -> Scene
scene (World s _ _ _ _ _ ) = s

view :: World -> View
view (World _ v _ _ _ _) = v

rayDepth :: World -> Int
rayDepth (World _ _ rd _ _ _ ) = rd

samples :: World -> Int
samples (World _ _ _ s _ _) = s

width :: World -> Int
width (World _ _ _ _ w _) = w

height :: World -> Int
height (World _ _ _ _ _ h) = h

-- | Shapes in the scene
data Shape = Sphere { center::Point3
                    , radius::Double
                    , materialFunc::Point3 -> Material
                    } -- ^ Spherical shape.
          | Plane { normal::Normal3
                  , distance::Double
                  , materialFunc::Point3 -> Material
                  } -- ^ Shape as two sided infinte plane.

-- | Light point used as general check if point is lit
type Light = Point3
