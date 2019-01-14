module Physics where
import Linear
import World
import Data.Maybe
import Data.List
import Control.Parallel.Strategies(parMap, rdeepseq)
import Debug

hit :: Obj -> Obj -> Hit    -- 70%
hit a@(Ball _ ra _ pa va) b@(Ball _ rb _ pb vb) = Hit a b t
    where 
        abv = dot ab vab
        vv = dot vab vab
        abab = dot ab ab
        r = ra + rb
        ab = pa - pb -- 14%
        vab = va - vb -- 14%
        delta = 4 * abv * abv - 4 * vv * (abab - r * r)
        sqrtDelta =  if delta <= 0 then 1/0 else sqrt delta
        tp = (-2 * abv + sqrtDelta) / (2 * vv)
        tn = (-2 * abv - sqrtDelta) / (2 * vv)
        t = min (positivate tp) (positivate tn)
hit a@(Ball _ ra _ pa va) b@(Wall _ n) = Hit a b time
    where 
        projP = pa `dot` n
        projV = va `dot` n
        dist = abs $ (width / 2 - ra) - projP
        time = positivate $ dist / projV
hit (Wall _ _) _ = error "Walls are not allowed in the first argument of hit!"        

advance :: Double -> [Obj] -> [Obj]
advance dt objs
    | dt < 0 = error "time travel detected!"
    | dt == 0 = objs
    | timeToHit <= dt = advance (dt - timeToHit) $ doHits nextHits $ map (walk timeToHit) objs
    | otherwise = map (walk dt) $ objs
    where
--         hits = parMap rdeepseq id [hit a b | a <- objs, b <- objs ++ walls, oid a < oid b]
        hits = concat $ paralellize
        paralellize = parMap rdeepseq combine objs
        combine a = [hit a b | b <- allo, oid a < oid b] -- TODO: combinations (of ids/Ints) can be calculated globally once (if there was an array of Objs)
        allo = objs ++ walls
--         hits = [hit a b | a <- objs, b <- objs ++ walls, oid a < oid b]
        timeToHit = minimum $ map timeLeft hits
        nextHits = filter (\h -> timeLeft h == timeToHit) hits -- TODO: do take advantage of precalculated hits? there are a lot of them that are non-affected by the very next hit (nextHits)
 
positivate :: Double -> Double
positivate t = if t <= 0 then 1 / 0 else 0.9999999 * t -- fromIntegral (floor $ t * 1000000) / 1000000

doHits :: [Hit] -> [Obj] -> [Obj]
doHits [] objs = objs
doHits _ [] = error "Empty list of objs!"
doHits (Hit oldA oldB _ : htail) objs = doHits htail objs'
    where
        a = fromJust $ find (==oldA) objs -- TODO: would an array of objs be faster? (doHits uses less than 1% of CPU, anyway)
        b = fromJust $ find (==oldB) $ walls ++ objs
        collided = doHit a b
        objs' = collided ++ filter (\x -> x /= a && x /= b) objs

doHit :: Obj -> Obj -> [Obj] -- TODO: verify if hit still need to be performed (due to a previous hit from a third object)
doHit (Ball ia ra ma pa va) (Ball ib rb mb pb vb) = [Ball ia ra ma pa va', Ball ib rb mb pb vb']
    where
        ab = pa - pb
        abUnit = ab ^/ norm ab
        uva = abUnit `dot` va
        uvb = abUnit `dot` vb
        p = elasticity * 2 * (uva - uvb) / (ma + mb)
        va' = va - p * mb *^ abUnit
        vb' = vb + p * ma *^ abUnit
doHit (Ball ia ra ma pa va) (Wall _ n) = [Ball ia ra ma pa (elasticity *^ va * inv)] -- TODO: correct elasticity loss
    where
        inv = (V3 1 1 1) - 2*n*n
doHit (Wall _ _) _ = error "Walls are not allowed in the first argument of doHit!"

walk :: Double -> Obj -> Obj
walk dt (Ball i r m p v) = Ball i r m p' v'
    where
        p' = p + (dt) *^ v + g ^* (dt^2/2)
        v0 = v + (g ^* dt)
        v' = v0
--         v' = if norm v0 < 0.00000001 then V3 0 0 0 else v0
walk _ w@(Wall _ _) = w

kenergy :: [Obj] -> Double
kenergy objs = sum $ map ke objs
    where
        ke (Ball _ _ m _ v) = abs $ m * (norm v) ^ 2 / 2

penergy :: [Obj] -> Double
penergy objs = sum $ map pe objs
    where
        pe (Ball _ r m (V3 _ y _) _) = abs $ m * (norm g) * h y r
        h y r = (width / 2) + y - r
