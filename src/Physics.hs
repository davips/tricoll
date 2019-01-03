module Physics where
import Linear
import World
import Data.Maybe
import Data.List
import Control.Parallel.Strategies(parMap, rdeepseq)
-- import Debug

hit :: Obj -> Obj -> Hit    
hit a@(Ball _ ra _ pa va) b@(Ball _ rb _ pb vb) = Hit a b t
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
hit a@(Ball _ ra _ pa va) b@(Wall _ n) = Hit a b t
    where 
        projP = pa `dot` n
        projV = va `dot` n
        dist = abs $ (width / 2 - ra) - projP
        t = positivate $ dist / projV
hit (Wall _ _) _ = error "Walls are not allowed in the first argument of hit!"        

-- pmap :: (t -> a) -> [t] -> [a]
-- pmap _ [] = []
-- pmap f xs = runEval $ do    
--     as' <- rpar (map f as)
--     bs' <- rpar (map f bs)
--     return as' ++ bs'
--     where
--         (as, bs) = splitAt (length xs / 2) xs


advance :: Double -> [Obj] -> [Obj]
advance dt objs
    | dt < 0 = error "time travel detected!"
    | dt == 0 = objs
    | timeToHit <= dt = advance (dt - timeToHit) $ doHits nextHits $ map (walk timeToHit) objs
--     | otherwise =  d2 (map (\x -> (idn $ ballA x, idn $ ballB x, fo $ timeLeft x)) hits) $ map (walk dt) objs
    | otherwise = map (walk dt) $ objs
    where
--         hits = parMap rdeepseq id [hit a b | a <- objs, b <- objs ++ walls, oid a < oid b]
        hits = concat $ parMap rdeepseq (\a -> [hit a b | b <- objs ++ walls, oid a < oid b]) objs
--         hits = [hit a b | a <- objs, b <- objs ++ walls, oid a < oid b]
        timeToHit = minimum $ map timeLeft hits
        nextHits = filter (\h -> timeLeft h == timeToHit) hits -- TODO: do take advantage of precalculated hits? there are a lot of them that are non-affected by the very next hit (nextHits)
 
-- fo :: Double -> Double
-- fo x = (fromIntegral $ round (1000.0 * x) :: Double) / 1000.0

positivate :: Double -> Double
positivate x = if x <= 0 then 1 / 0 else x

doHits :: [Hit] -> [Obj] -> [Obj]
doHits [] objs = objs
doHits _ [] = error "Empty list of objs!"
doHits (Hit oldA oldB _ : htail) objs = doHits htail objs'
    where
        a = fromJust $ find (==oldA) objs -- TODO: would an array of objs be faster?
        b = fromJust $ find (==oldB) $ walls ++ objs
        collided = doHit a b
        objs' = collided ++ filter (\x -> x /= a && x /= b) objs

doHit :: Obj -> Obj -> [Obj] -- TODO: verify if simultaneous hits are problematic. Maybe postpone "walk (0.00000001)" to be done as a batch after doHits call.
doHit (Ball ia ra ma pa va) (Ball ib rb mb pb vb) = [walk (0.00000001) $ Ball ia ra ma pa va', walk (0.00000001) $ Ball ib rb mb pb vb']
    where
        ab = pa - pb
        abUnit = ab ^/ norm ab
        aOut = (abUnit `dot` va) *^ abUnit
        bOut = (abUnit `dot` vb) *^ abUnit
        va' = va - aOut + bOut
        vb' = vb - bOut + aOut
doHit (Ball ia ra ma pa va) (Wall _ n) = {-d2 (error "bateu!" :: Double)-} [walk (0.00000001) $ Ball ia ra ma pa (va * ((V3 1 1 1) - 2*n*n))]
doHit (Wall _ _) _ = error "Walls are not allowed in the first argument of doHit!"

walk :: Double -> Obj -> Obj
walk dt (Ball i r m p v) = Ball i r m (p + (dt) *^ v) v
walk _ w@(Wall _ _) = w
