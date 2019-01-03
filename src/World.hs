{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module World where
import Linear
import Control.DeepSeq
import GHC.Generics (Generic)

data Obj = Ball {oid :: Int, radius :: Double, mass :: Double, pos :: V3 Double, vel :: V3 Double}
         | Wall {oid :: Int, normal :: V3 Double} deriving (Show, Generic, NFData)
instance Eq Obj where
    x == y = oid x == oid y

data State = State Double [Obj]
data Hit = Hit {objA :: Obj, objB :: Obj, timeLeft :: Double} deriving (Show, Generic, NFData)

width :: Double
width = 3

world :: [Obj]
world = sphere 10 0.2 0.04 ++ balls 0 ++ sphere 5000 0.1 0.04 ++ sphere 10000 0.15 0.01 ++ sphere 15000 0.15 0.08

sphere :: Int -> Double -> Double -> [Obj]
sphere idn rad atomR = ring idn rad atomR

ring :: Int -> Double -> Double -> [Obj]
ring idn rad r = [Ball (idn + i) r 1 (V3 (x i) 0 (y i)) (V3 (y i) 0.1 (x i)) | i <- [1..n]]
    where     
        n = round $ 2 * pi * rad / (1.9*r)
        arc = 2 * pi / (fromIntegral n)
        x i = rad * (cos $ (fromIntegral i) * arc)
        y i = rad * (sin $ (fromIntegral i) * arc)

balls :: Int -> [Obj]
balls i = [ Ball (i+1) 0.231 0.4 (V3 0.39 0 0.5) (V3 0.01 0.01 0.007)
        , Ball (i+2) 0.34 0.8 (V3 0.5 0.39 0.3) (V3 (-0.01) 0.01 0.01)
        , Ball (i+3) 0.23 0.8 (V3 (-0.1) (-0.25) (-0.2)) (V3 (0.009) 0.01 0.004)
        , Ball (i+4) 0.243 0.8 (V3 (-0.4) 0 0.39) (V3 (0.1) 0.01 0.02)
        ]

walls :: [Obj]
walls = [Wall 9999995 $ V3 0 0 1
       , Wall 9999996 $ V3 0 1 0
       , Wall 9999997 $ V3 1 0 0
       , Wall 9999998 $ V3 0 0 (-1)
       , Wall 9999999 $ V3 0 (-1) 0
       , Wall 10000000 $ V3 (-1) 0 0]
