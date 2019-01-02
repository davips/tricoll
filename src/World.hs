module World where
import Linear

data Obj = Ball {oid :: Int, radius :: Double, mass :: Double, pos :: V3 Double, vel :: V3 Double}
         | Wall {oid :: Int, normal :: V3 Double} deriving Show
instance Eq Obj where
    x == y = oid x == oid y

data State = State Double [Obj]
data Hit = Hit {objA :: Obj, objB :: Obj, timeLeft :: Double} deriving Show

width :: Double
width = 3

world :: [Obj]
world = sphere 1 0.13 ++ balls

sphere :: Double -> Double -> [Obj]
sphere rad atomR = ring rad atomR

ring :: Double -> Double -> [Obj]
ring rad r = [Ball (i) r 1 (V3 (x i) 0 (y i)) (V3 0 0 (1)) | i <- [1..n]]
    where     
        n = round $ 2 * pi * rad / (1.9*r)
        arc = 2 * pi / (fromIntegral n)
        x i = rad * (cos $ (fromIntegral i) * arc)
        y i = rad * (sin $ (fromIntegral i) * arc)

balls :: [Obj]
balls = [ Ball 91 0.231 0.4 (V3 0.39 0 0.5) (V3 0.1 0.1 0.7)
        , Ball 92 0.34 0.8 (V3 0.5 0.39 0.3) (V3 (-0.1) 0.1 0.1)
        , Ball 93 0.23 0.8 (V3 (-0.1) (-0.25) (-0.2)) (V3 (0.9) 0.1 0.4)
        , Ball 94 0.243 0.8 (V3 (-0.4) 0 0.39) (V3 (0.1) 0.1 0.2)
        ]

walls :: [Obj]
walls = [Wall 9999995 $ V3 0 0 1
       , Wall 9999996 $ V3 0 1 0
       , Wall 9999997 $ V3 1 0 0
       , Wall 9999998 $ V3 0 0 (-1)
       , Wall 9999999 $ V3 0 (-1) 0
       , Wall 10000000 $ V3 (-1) 0 0]
