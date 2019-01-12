{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module World where
import Linear
import Control.DeepSeq
import GHC.Generics (Generic)

data Obj = Ball {oid :: Int, radius :: Double, mass :: Double, pos :: V3 Double, vel :: V3 Double}
         | Wall {oid :: Int, normal :: V3 Double} deriving (Show, Generic, NFData)
instance Eq Obj where
    x == y = oid x == oid y

data State = State Double [Obj] Int
data Hit = Hit {objA :: Obj, objB :: Obj, timeLeft :: Double} deriving (Show, Generic, NFData)

width :: Double
width = 400

g :: V3 Double
g = V3 0 (1 * (-9.80665)) 0

world :: [Obj]
world = sphere 10 80 8 ++ [Ball (1) 20 ((4/3) * pi * 20^3) (V3 (0.1) (180) (0)) (V3 (10) 0.7 0)] -- ++ balls 0 -- ++ sphere 5000 0.1 0.04 ++ sphere 10000 0.15 0.2 ++ sphere 15000 0.15 0.01 ++ 

sphere :: Int -> Double -> Double -> [Obj]
sphere idn rad atomR = ring idn rad atomR

ring :: Int -> Double -> Double -> [Obj]
ring idn rad r = [Ball (idn + i) r ((4/3) * pi * r^3) (V3 (x i) 150 (y i)) (V3 0 0 0) | i <- [1..n]]
    where     
        n = round $ 2 * pi * rad / (1.9*r)
        arc = 2 * pi / (fromIntegral n)
        x i = rad * (cos $ (fromIntegral i) * arc)
        y i = rad * (sin $ (fromIntegral i) * arc)

balls :: Int -> [Obj]
balls i = [ Ball (i+2) 1 1  (V3 0.5 0.5 0.3) (V3 (-0.01) 0.01 0.01)
          , Ball (i+3) 0.03 0.03 (V3 (-1) (-1) (-0.2)) (V3 (1) 1 0.004)
--         , Ball (i+4) 0.243 0.8 (V3 (-0.4) 0 0.39) (V3 (0.1) 0.01 0.02)
        ]

walls :: [Obj]
walls = [Wall 9999995 $ V3 0 0 1
       , Wall 9999996 $ V3 0 1 0
       , Wall 9999997 $ V3 1 0 0
       , Wall 9999998 $ V3 0 0 (-1)
       , Wall 9999999 $ V3 0 (-1) 0
       , Wall 10000000 $ V3 (-1) 0 0]
