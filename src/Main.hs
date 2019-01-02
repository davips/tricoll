{-# OPTIONS_GHC -Wall #-}
{-
TODO
 add gravity
 ball generator (auto mass calculator)
 consider mass in collisions
-}
module Main where
import Draw
import Physics
import World
import Vis

state0 :: State
state0 = State 0 balls                

simFun :: Float -> State -> State
simFun tFloat (State t0 objs0) = State t objs
  where
    t = realToFrac tFloat    
    dt = t - t0
    objs = advance dt objs0
    
main :: IO ()
main = do
  simulate (defaultOpts {optWindowName = "simulate test"}) (1 / fps) state0 drawFun simFun
