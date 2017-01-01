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

--------------------------------------------------------------------------------
-- BRDF models
data BRDF = Lambert {color::Color}
          | Reflection {inRI::Double, outRI::Double, color::Color}
          | Refraction {inRI::Double, outRI::Double, color::Color}
          | Emmisive {color::Color}

--------------------------------------------------------------------------------
-- Material
data Material = Material{ brdfs::[(Double,BRDF)] }

-- An intersection is represented by the normal at the intersection point,
-- the point of intersection, the viewing ray coming in to the intersection and
-- the material at the intersection point.

data Ray = Ray { base::Point3, direction::Normal3 }

data Intersection = Intersection{normalI::Normal3, point::Point3,
  ray::Ray, material::Material}

--------------------------------------------------------------------------------
-- Scene

data Scene = Scene { shapes::[Shape]
                   , lights::[Light]
                   , ambientLight::Color
                   , backgroundColor::Color
                   }

data View = View { camera::Point3
                 , clipPlane::Double
                 , forward::Normal3
                 , up::Normal3
                 , fov::Double
                 }

-- data World = World { scene::Scene
--                    , view::View
--                    , rayDepth::Int
--                    , randomGenerator::StdGen
--                    , samples::Int
--                    }

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

--------------------------------------------------------------------------------
-- Shapes: currently sphere and plane


data Shape = Sphere { center::Point3, radius::Double
                   , materialFunc::Point3 -> Material}
          | Plane { normal::Normal3, distance::Double
                  , materialFunc::Point3 -> Material}

type Light = Point3
