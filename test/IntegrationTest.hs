-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}


module Main where

import SceneBuilder
import Predefined
import Data.Vect.Double
import Types
import Test.QuickCheck
import Data.List
import TestRaytrace

-- Check if order of adding shapes and lights doesn't matter
prop_SceneBuilderCommutative :: Scene -> (Scene -> Scene)
                             -> (Scene -> Scene)-> Bool
prop_SceneBuilderCommutative s a b = b (a s) == a (b s)

instance Arbitrary Vec3 where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Vec3 x y z

instance Arbitrary Normal3 where
  arbitrary = do
    vec <- arbitrary
    return $ mkNormal vec

instance Arbitrary Shape where
  arbitrary = do
    typ <- arbitrary :: Gen Int
    case typ `mod` 2 of
      0 -> do
        c <- arbitrary
        r <- arbitrary
        return $ Sphere c r flatRed
      _ -> do
        n <- arbitrary
        d <- arbitrary
        return $ Plane n d flatRed

instance Arbitrary Scene where
  arbitrary = do
    shapes <- arbitrary
    lights <- arbitrary
    amb <- arbitrary
    back <- arbitrary
    return $ Scene shapes lights amb back

instance Eq Scene where
  (==) (Scene sh li al bc) (Scene sh2 li2 al2 bc2) = (al == al2) && (bc == bc2)
      && null (sh \\ sh2) && null (li \\ li2)

instance {-# OVERLAPPING #-} Arbitrary (Scene -> Scene) where
  arbitrary = do
    func <- arbitrary :: Gen Int
    case func `mod` 2 of
      0 -> do
        shape <- arbitrary
        return $ addShape shape
      1 -> do
        point <- arbitrary
        rad <- arbitrary
        col <- arbitrary
        return $ addLight point rad col

instance Show (Scene -> Scene) where
  show s = "Function from scene to scene"

main :: IO ()
main = quickCheck prop_SceneBuilderCommutative
