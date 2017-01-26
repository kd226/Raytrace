{-|
Module      : Types
Description : Raytrace types
Copyright   : (c) Konrad Dobroś, 2017
License     : GPL-3
Maintainer  : dobros.konrad@gmail.com
Stability   : experimental
Portability : POSIX

Module containing types used in Raytace library.
-}

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Types where

import Data.Vect.Double
import Control.Monad.Random

--------------------------------------------------------------------------------
-- Basic types
type Color = Vec3

deriving instance Eq Vec3
instance Eq Normal3 where
  (==) a b = fromNormal a == fromNormal b


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

-- | BRDF models with their corresponding descriptions (Bounce Reflect Diffuse Functions).
data BRDF = Lambert { color::Color -- ^ Color of lambertian surface.
                    } -- ^ Lambertian reflectance BRDF.
          | Reflection { inRI::Double -- ^ Index of refraction of material
                                      -- inside the shape.
                       , outRI::Double -- ^ Index of refraction of material
                                       -- outside the shape.
                       , color::Color  -- ^ Change of color of reflected rays.
                       } -- ^ Perfect reflection BRDF.
          | Refraction { inRI::Double -- ^ Index of refraction of material
                                      -- inside the shape.
                       , outRI::Double -- ^ Index of refraction of material
                                       -- outside the shape.
                       , color::Color -- ^ Change of color of refracted rays.
                       } -- ^ Perfect refraction BRDF.
          | Emmisive { color::Color -- ^ Color of emmited light.
                     } -- ^ Generic emmisive BRDF.
                     deriving (Eq)

-- | Material of specific point.
data Material = Material{ brdfs::[(Double,BRDF)] -- ^ Pairs of and brdfs.
                         -- In order for material to be realistic sum of must be
                         -- lower or equal to 1.
                        } deriving (Eq)

-- | Ray in three dimensions.
data Ray = Ray { base::Point3 -- ^ Point from which the ray starts.
               , direction::Normal3 -- ^ Direction in which ray is pointing.
               } deriving (Eq)
-- | All information about intersection.
data Intersection = Intersection { normalI::Normal3 -- ^ Normal of intersection.
                                 , point::Point3 -- ^ Point of intersection.
                                 , ray::Ray -- ^ Ray which intersected.
                                 , material::Material -- ^ Material at point of
                                   -- intersection.
                                 } deriving (Eq)

-- | Scenes description.
data Scene = Scene { shapes::[Shape] -- ^ Shapes in a scene.
                   , lights::[Light] -- ^ Lights in a scene.
                   , ambientLight::Color -- ^ Ambient light of a scene
                   -- (light that doesn't depend on visibility from any light source).
                   , backgroundColor::Color -- Background light of a scene.
                   } deriving (Show)

-- | View description.
data View = View { camera::Point3 -- ^ Cameras position in the scene
                 , clipPlane::Double -- ^ Clipping plane of a camera (how close the objects can be seen)
                 , forward::Normal3 -- ^ Normal vector of where the camera is facing
                 , up::Normal3 -- ^ Normal vector of where the camera has "up" direction in
                 , fov::Double -- ^ Angle between point most to the left and most to the right, as seen by the camera
                 } deriving (Eq)

-- | Description of whole Raytrace world with all settings needed to render
-- the scene.
data World = World !Scene !View !Int !Int !Int !Int -- ^ Scene to be rendered,
-- view from which to render, depth of ray bounces, number of frames to be averaged
-- width and height of output render
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
data Shape = Sphere { center::Point3 -- ^ Center of a sphere.
                    , radius::Double -- ^ Radius of a sphere.
                    , materialFunc::Point3 -> Material -- ^ Function that
                    -- describes material of point on a sphere.
                    } -- ^ Spherical shape.
          | Plane { normal::Normal3 -- ^ Normal direction of a sphere.
                  , distance::Double -- ^ Distance from an origin - (0,0,0) along normal.
                  , materialFunc::Point3 -> Material -- ^ Function that
                  -- describes material of a point on a plane.
                  } -- ^ Shape as two sided infinte plane.
                  deriving (Show)
instance Eq Shape where
  (==) (Sphere c r _) (Sphere c2 r2 _) = c == c2 && r == r2
  (==) (Plane n d _ ) (Plane n2 d2 _ ) = n == n2 && d == d2
  (==) _ _ = False

instance Show (Point3 -> Material) where
  show _ = "Material function"


-- | Light point used as general check if point is lit
type Light = Point3
