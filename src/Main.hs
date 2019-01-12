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
    objs = advance dt objs0
    
main :: IO ()
main = do
  simulate (defaultOpts {optWindowName = "simulate test"}) (1 / fps) state0 drawFun simFun
