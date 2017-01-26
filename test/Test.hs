module Main where

import Test.HUnit
import Data.List
import GHC.Stack
import Raytracer
import Types
import Predefined
import SceneBuilder
import Data.Vect.Double
-- import Data.Vect.Double.Base

assertWithinEpsilon :: (HasCallStack, Num a, Show a) => String -> (a -> Double) -> a -> a -> Double -> Assertion
assertWithinEpsilon premise f a b eps = if f (abs (a-b)) < eps
  then putStrLn ""
  else assertFailure (premise ++ "\nexpected: " ++ show a ++ "\ngot: " ++ show b
                      ++ "\nwith epsilon of: " ++ show eps
                      ++ "\nthe diffrence is: " ++ show ( f $ abs (a-b)))

instance (Num a) => Num [a] where
  (+) = zipWith (+)
  (-) = zipWith (-)
  (*) = zipWith (*)
  abs = fmap abs
  fromInteger a = [fromInteger a]
  signum = fmap signum

instance Num Vec3 where
  (+) = (&+)
  (-) = (&-)
  (*) = crossprod
  abs (Vec3 a b c) = Vec3 (abs a) (abs b) (abs c)
  fromInteger a = Vec3 (fromInteger a) (fromInteger a) (fromInteger a)
  signum (Vec3 a b c) = Vec3 (signum a) (signum b) (signum c)

testRootsP = TestCase (assertWithinEpsilon "for roots of 1*x^2-2.2*x+0.85 = 0"
                      sum [0.5, 1.7] (sort(roots 1 (-2.2) 0.85)) 0.001)

testRootsZ = TestCase (assertEqual "for roots of 1*x^2+2*x+1 = 0" [-1.0, -1.0]
                        (roots 1 2 1))

testRootsN = TestCase (assertEqual "for roots of -1*x^2+1*x+1 = 0" []
                        (roots 1 1 1))

rootTests = [TestLabel "Delta negative" testRootsN
  , TestLabel "Delta is zero" testRootsZ, TestLabel "Delta positive" testRootsP]

testAddShape = TestCase (assertEqual "after adding shape" [Sphere (Vec3 0 0 0) 0.1 flatRed]
         (shapes $ addShape (Sphere (Vec3 0 0 0) 0.1 flatRed) emptyScene))

testYawCamera = TestCase (assertWithinEpsilon "angle of rotated camera" norm
        (normalize (Vec3 0.0 0.5 (-0.5))) (fromNormal $ up (yawCamera (pi/4) sampleView )) 0.001)

testSceneBuilder = [TestLabel "Add shape count" testAddShape
              , TestLabel "Test up vector of camera yawed by 45" testYawCamera ]
main :: IO ()
main = mapM_ runTestTT (rootTests ++ testSceneBuilder)
