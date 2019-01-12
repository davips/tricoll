{-# OPTIONS_GHC -Wall #-}
module Main where
import Draw
import Physics
import World
import Vis
import Debug
import GHC.Float

state0 :: State
state0 = State 0 world 0

simFun :: Float -> State -> State
simFun tFloat (State t0 objs0 frames) = State t objs $ frames + 1
    where
        t = float2Double tFloat    
        dt = if t < 1000000 then t - t0 else error "interrompido!"
        objs = d2 frames $ advance dt objs0 -- TODO: the calls to simFun are not sequential; bug in not-gloss?
    {-d2 (kenergy objs0 / 1000 + penergy objs0 / 1000) $-}

myOptions :: Options
myOptions = defaultOpts {optWindowName = "tricoll", optInitialCamera = Just $ Camera0 90 (-90) 750}

main :: IO ()
main = do
  simulate myOptions (1 / fps) state0 drawFun simFun
