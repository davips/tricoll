module Physics where
import Linear
import World
import Data.Maybe
import Data.List

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
doHit (Ball ia ra ma pa va) (Ball ib rb mb pb vb) = [walk (0.00000001) $ Ball ia ra ma pa va', walk (0.00000001) $ Ball ib rb mb pb vb']
    where
        ab = pa - pb
        abUnit = ab ^/ norm ab
        aOut = (abUnit `dot` va) *^ abUnit
        bOut = (abUnit `dot` vb) *^ abUnit
        va' = va - aOut + bOut
        vb' = vb - bOut + aOut
doHit (Ball ia ra ma pa va) (Wall ib n) = [walk (0.00000001) $ Ball ia ra ma pa (va * ((V3 1 1 1) - 2*n*n))]
doHit (Wall _ _) _ = error "Walls are not allowed in the first argument of doHit!"

walk :: Double -> Obj -> Obj
walk dt (Ball i r m p v) = Ball i r m (p + (dt) *^ v) v
walk _ w@(Wall _ _) = w
