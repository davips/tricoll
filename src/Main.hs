{-# OPTIONS_GHC -Wall #-}
{-
TODO
 implement real collisions
 ball generator (auto mass calculator)
 consider mass in collisions
-}
module Main where
import Debug
import Linear
import Vis
import Data.Maybe
import Data.List

data Hit = Hit {objA :: Obj, objB :: Obj, timeLeft :: Double} deriving Show
data Obj = Ball {oid :: Int, rad :: Double, mas :: Double, pos :: V3 Double, vel :: V3 Double}
         | Wall {oid :: Int, normal :: V3 Double} deriving Show
instance Eq Obj where
    x == y = oid x == oid y

hit :: Obj -> Obj -> Hit    
hit a@(Ball _ ra ma pa va) b@(Ball _ rb mb pb vb) = Hit a b t
    where 
        abv = dot ab vab
        vv = dot vab vab
        abab = dot ab ab
        r = ra + rb
        ab = pa - pb
        vab = va - vb
        delta = 4 * abv * abv - 4 * vv * (abab - r * r)
        sqrtDelta =  if delta <= 0 then 1/0 else sqrt delta
        tp = (-2 * abv + sqrtDelta) / (2 * vv)
        tn = (-2 * abv - sqrtDelta) / (2 * vv)
        t = min (positivate tp) (positivate tn)

hit a@(Ball _ ra ma pa va) b@(Wall _ n) = Hit a b t
    where 
        projP = pa `dot` n
        projV = va `dot` n
        dist = abs $ (width / 2 - ra) - projP
        t = positivate $ dist / projV

hit (Wall _ _) _ = error "Walls are not allowed in the first argument of hit!"        

data State = State Double [Obj]

walls :: [Obj]
walls = [Wall 5 $ V3 0 0 1
       , Wall 6 $ V3 0 1 0
       , Wall 7 $ V3 1 0 0
       , Wall 8 $ V3 0 0 (-1)
       , Wall 9 $ V3 0 (-1) 0
       , Wall 10 $ V3 (-1) 0 0]

state0 :: State
state0 = State 0 [Ball 4 1 0.4 (V3 15 0 0) (V3 50 10 70)
                , Ball 3 5 0.8 (V3 5 0.25 0.3) (V3 (-3.01) 2 6)
                , Ball 2 5 0.8 (V3 (-10) (-0.25) (-0.2)) (V3 (2.89) 50 4)
                , Ball 1 3 0.8 (V3 (-40) 0 0) (V3 (0) 11 69)]
                
advance :: Double -> [Obj] -> [Obj]
advance dt balls
    | dt < 0 = error "time travel detected!"
    | dt == 0 = balls
    | timeToHit <= dt = advance (dt - timeToHit) $ doHits nextHits $ map (walk timeToHit) balls
--     | otherwise =  d2 (map (\x -> (idn $ ballA x, idn $ ballB x, fo $ timeLeft x)) hits) $ map (walk dt) balls
    | otherwise = map (walk dt) $ balls
    where
        hits = [hit a b | a <- balls, b <- balls ++ walls, oid a < oid b] -- TODO: should permutations be generated more efficiently?
        timeToHit = minimum $ map timeLeft hits
        nextHits = filter (\h -> timeLeft h == timeToHit) hits -- TODO: do take advantage of precalculated hits? there are a lot of them that are non-affected by the very next hit (nextHits)
 
-- fo :: Double -> Double
-- fo x = (fromIntegral $ round (1000.0 * x) :: Double) / 1000.0

positivate :: Double -> Double
positivate x = if x <= 0 then 1 / 0 else x

doHits :: [Hit] -> [Obj] -> [Obj]
doHits [] balls = balls
doHits _ [] = error "Empty list of objs!"
doHits (Hit oldA oldB _ : htail) balls = doHits htail balls'
    where
        a = fromJust $ find (==oldA) balls -- TODO: would an array of objs be faster?
        b = fromJust $ find (==oldB) $ walls ++ balls
        collided = doHit a b
        balls' = collided ++ filter (\x -> x /= a && x /= b) balls

doHit :: Obj -> Obj -> [Obj] -- TODO: verify if simultaneous hits are problematic. Maybe postpone "walk (0.00000001)" to be done as a batch after doHits call.
doHit (Ball ia ra ma pa va) (Ball ib rb mb pb vb) = [walk (0.00000001) $ Ball ia ra ma pa (-1 *^ va), walk (0.00000001) $ Ball ib rb mb pb (-1 *^ vb)]
doHit (Ball ia ra ma pa va) (Wall ib n) = [walk (0.00000001) $ Ball ia ra ma pa (-1 *^ va)]
doHit (Wall _ _) _ = error "Walls are not allowed in the first argument of doHit!"

walk :: Double -> Obj -> Obj
walk dt (Ball i r m p v) = Ball i r m (p + (dt) *^ v) v
walk _ w@(Wall _ _) = w

drawFun :: State -> VisObject Double --x=red y=green z=blue
drawFun (State t balls) = Trans (V3 (-100) (-200) (80)) $ VisObjects $ [axes, cube] ++ spheres
  where
    axes = Axes (width / 2, width)
    cube = Cube (width) Wireframe $ makeColor 1 1 1 1
    spheres = map sphe balls
    sphe (Ball _ r _ p _) = Trans p $ Sphere r Solid (makeColor 0.2 0.3 0.8 1) -- $ realToFrac 1.0)
    sphe (Wall _ _) = error "Walls are drawn separately for now!"

simFun :: Float -> State -> State
simFun tFloat (State t0 balls0) = State t balls
  where
    t = realToFrac tFloat
    dt = t - t0
    balls = advance dt balls0
    
main :: IO ()
main = do
  simulate (defaultOpts {optWindowName = "simulate test"}) 0.05 state0 drawFun simFun

-- main :: IO ()
-- main = do
--     putStr "23"
