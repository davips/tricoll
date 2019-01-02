module World where
import Linear

data Obj = Ball {oid :: Int, rad :: Double, mas :: Double, pos :: V3 Double, vel :: V3 Double}
         | Wall {oid :: Int, normal :: V3 Double} deriving Show
instance Eq Obj where
    x == y = oid x == oid y

data State = State Double [Obj]
data Hit = Hit {objA :: Obj, objB :: Obj, timeLeft :: Double} deriving Show

width :: Double
width = 3

balls :: [Obj]
balls = [ Ball 1 0.231 0.4 (V3 0.39 0 0.5) (V3 0.1 0.1 0.7)
        , Ball 2 0.34 0.8 (V3 0.5 0.39 0.3) (V3 (-0.1) 0.1 0.1)
        , Ball 3 0.23 0.8 (V3 (-0.1) (-0.25) (-0.2)) (V3 (0.9) 0.1 0.4)
        , Ball 4 0.243 0.8 (V3 (-0.4) 0 0.39) (V3 (0.1) 0.1 0.2)
        , Ball 5 0.123 0.8 (V3 (-0.4) 0 0.39) (V3 (0.2) 0.1 0.2)
        , Ball 6 0.2423 0.8 (V3 (0.4) 0 0.39) (V3 (0.1) 0.1 0.2)
        , Ball 7 0.2423 0.8 (V3 (-0.4) 1 0.39) (V3 (0.2) 0.1 0.2)
        , Ball 8 0.4 0.8 (V3 0.5 0.39 0.3) (V3 (-0.1) 0.1 0.1)
        , Ball 9 0.3 0.8 (V3 (-0.1) (-0.25) (-0.2)) (V3 (0.09) 0.01 0.14)
        , Ball 10 0.1243 0.8 (V3 (-0.4) 0 0.39) (V3 (0.1) 0.1 0.2)
        , Ball 11 0.144 0.8 (V3 0.5 0.39 0.3) (V3 (-0.1) 0.1 0.1)
        , Ball 12 0.234 0.8 (V3 0.5 0.39 0.3) (V3 (-0.1) 0.1 0.1)
        , Ball 13 0.233 0.8 (V3 (-0.1) (-0.25) (-0.2)) (V3 (0.009) 1 0.14)
        , Ball 14 0.2343 0.8 (V3 (-0.4) 0 0.39) (V3 (0.01) 0.1 0)
        , Ball 15 0.1223 0.8 (V3 (-0.4) 0 0.39) (V3 (0.02) 0.1 0)
        , Ball 16 0.13423 0.8 (V3 (0.4) 0 0.39) (V3 (0.31) 0.1 0)
        , Ball 17 0.23423 0.8 (V3 (-0.4) 1 0.39) (V3 (0.22) 0.1 0)
        , Ball 18 0.24 0.8 (V3 0.5 0.39 0.3) (V3 (-0.1) 0.1 0.1)
        , Ball 19 0.23 0.8 (V3 (-0.1) (-0.25) (-0.2)) (V3 (0.19) 0.1 0.024)
        , Ball 20 0.143 0.8 (V3 (-0.4) 0 0.39) (V3 (0.001) 0.01 0.02)
        ]

walls :: [Obj]
walls = [Wall 9999995 $ V3 0 0 1
       , Wall 9999996 $ V3 0 1 0
       , Wall 9999997 $ V3 1 0 0
       , Wall 9999998 $ V3 0 0 (-1)
       , Wall 9999999 $ V3 0 (-1) 0
       , Wall 10000000 $ V3 (-1) 0 0]
