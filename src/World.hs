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
balls = [ Ball 1 0.31 0.4 (V3 0.39 0 0.5) (V3 1 1 0.7)
        , Ball 2 0.4 0.8 (V3 0.5 0.39 0.3) (V3 (-1) 1 1)
        , Ball 3 0.3 0.8 (V3 (-0.1) (-0.25) (-0.2)) (V3 (0.9) 1 1.4)
        , Ball 4 0.43 0.8 (V3 (-0.4) 0 0.39) (V3 (1) 1 2)
        , Ball 5 0.23 0.8 (V3 (-0.4) 0 0.39) (V3 (2) 1 2)
        , Ball 6 0.423 0.8 (V3 (0.4) 0 0.39) (V3 (1) 1 2)
        , Ball 7 0.423 0.8 (V3 (-0.4) 1 0.39) (V3 (2) 1 2)
        , Ball 8 0.4 0.8 (V3 0.5 0.39 0.3) (V3 (-1) 1 1)
        , Ball 9 0.3 0.8 (V3 (-0.1) (-0.25) (-0.2)) (V3 (0.9) 1 1.4)
        , Ball 10 0.243 0.8 (V3 (-0.4) 0 0.39) (V3 (1) 1 2)
        , Ball 11 0.44 0.8 (V3 0.5 0.39 0.3) (V3 (-1) 1 1)
        , Ball 12 0.34 0.8 (V3 0.5 0.39 0.3) (V3 (-1) 1 1)
        , Ball 13 0.33 0.8 (V3 (-0.1) (-0.25) (-0.2)) (V3 (0.9) 1 1.4)
        , Ball 14 0.343 0.8 (V3 (-0.4) 0 0.39) (V3 (1) 1 2)
        , Ball 15 0.223 0.8 (V3 (-0.4) 0 0.39) (V3 (2) 1 2)
        , Ball 16 0.3423 0.8 (V3 (0.4) 0 0.39) (V3 (1) 1 2)
        , Ball 17 0.3423 0.8 (V3 (-0.4) 1 0.39) (V3 (2) 1 2)
        , Ball 18 0.24 0.8 (V3 0.5 0.39 0.3) (V3 (-1) 1 1)
        , Ball 19 0.23 0.8 (V3 (-0.1) (-0.25) (-0.2)) (V3 (0.9) 1 1.4)
        , Ball 20 0.143 0.8 (V3 (-0.4) 0 0.39) (V3 (1) 1 2)
        ]

walls :: [Obj]
walls = [Wall 999995 $ V3 0 0 1
       , Wall 999996 $ V3 0 1 0
       , Wall 999997 $ V3 1 0 0
       , Wall 999998 $ V3 0 0 (-1)
       , Wall 999999 $ V3 0 (-1) 0
       , Wall 1000000 $ V3 (-1) 0 0]
