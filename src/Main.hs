{-# OPTIONS_GHC -Wall #-}
{-
TODO
 wall collisions
 implement collisions
 ball generator (auto mass calculator)
 consider mass in collisions
-}
module Main where
import Linear
import Vis
import Data.Maybe
import Data.List

import Text.Printf
import qualified Debug.Trace as T
import Text.Show.Pretty



activateDebug :: Bool
activateDebug = True

width :: Double
width = 100

d1 :: Show t => t -> t
d1 res = if activateDebug then T.trace (ppShow res) res else res

d2 :: Show a => a -> t -> t
d2 res2 res = if activateDebug then T.trace (ppShow res2) res else res

data Ball = Ball {idn :: Int, rad :: Double, mas :: Double, pos :: V3 Double, vel :: V3 Double} deriving Show
data Hit = Hit {ballA :: Ball, ballB :: Ball, timeLeft :: Double} deriving Show
data State = State Double [Ball]
instance Eq Ball where
    x == y = idn x == idn y
  
advance :: Double -> [Ball] -> [Ball]
advance dt balls
    | dt < 0 = error "time travel detected!"
    | dt == 0 = balls
    | timeToHit <= dt = advance (dt - timeToHit) $ doHits nextHits $ map (walk timeToHit) balls
--     | otherwise =  d2 (map (\x -> (idn $ ballA x, idn $ ballB x, fo $ timeLeft x)) hits) $ map (walk dt) balls
    | otherwise = map (walk dt) balls
    where
        hits = [hit a b | a <- balls, b <- balls, idn a < idn b]  -- ++ [hitWall a w | a <- balls, w <- walls] -- TODO: should permutations be generated more efficiently?
        timeToHit = minimum $ map timeLeft hits
        nextHits = filter (\h -> timeLeft h == timeToHit) hits -- TODO: do take advantage of precalculated hits? there are a lot of them that are non-affected by the very next hit (nextHits)

fo :: Double -> Double
fo x = (fromIntegral $ round (1000.0 * x)) / 1000.0

-- hitWall :: Ball -> Wall -> Hit
-- hitWall a@(Ball _ ra ma pa va) b@(Ball _ rb mb pb vb) = Hit a b t

hit :: Ball -> Ball -> Hit
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
    
positivate :: Double -> Double
positivate x = if x <= 0 then 1 / 0 else x

doHits :: [Hit] -> [Ball] -> [Ball]
doHits [] balls = balls
doHits _ [] = error "Empty list of balls!"
doHits (Hit oldA oldB _ : htail) balls = doHits htail balls'
    where
        a = fromJust $ find (==oldA) balls -- TODO: would an array of balls be faster?
        b = fromJust $ find (==oldB) balls
        (a', b') = doHit a b
        balls' = [a', b'] ++ filter (\x -> idn x /= idn a && idn x /= idn b) balls 

doHit :: Ball -> Ball -> (Ball, Ball) -- TODO: only perform hit if it is still the case
doHit (Ball ia ra ma pa va) (Ball ib rb mb pb vb) = (walk (0.00000001) $ Ball ia ra ma pa (-1 *^ va), walk (0.00000001) $ Ball ib rb mb pb (-1 *^ vb))
    
walk :: Double -> Ball -> Ball
walk dt (Ball i r m p v) = Ball i r m (p + (dt) *^ v) v

drawFun :: State -> VisObject Double
drawFun (State _ balls) = VisObjects $ [axes, cube] ++ spheres
  where
    axes = Axes (width / 2, width)
    plane = Plane (V3 0 0 1) (makeColor 1 1 1 1) (makeColor 0.4 0.6 0.65 0.4)
    cube = Cube width Wireframe $ makeColor 1 1 1 1
    spheres = map sphe balls
    sphe (Ball _ r m p _) = Trans p $ Sphere r Solid (makeColor 0.2 0.3 0.8 $ realToFrac 1)

simFun :: Float -> State -> State
simFun tFloat (State t0 balls0) = State t balls
  where
    t = realToFrac tFloat
    dt = t - t0
    balls = advance dt balls0
    
main :: IO ()
main = do
  let state0 = State 0 [    Ball 4 1 0.4 (V3 5 0 0) (V3 0 0 0)
                        ,   Ball 3 0.5 0.8 (V3 3 0.25 0.3) (V3 (-3.01) 0 0)
                        ,   Ball 2 0.5 0.8 (V3 (-1) (-0.25) (-0.2)) (V3 (2.89) 0 0)
                        ,   Ball 1 3 0.8 (V3 (-5) 0 0) (V3 (0) 0 0)]
  simulate (defaultOpts {optWindowName = "simulate test"}) 0.05 state0 drawFun simFun
