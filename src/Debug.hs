{-# OPTIONS_GHC -Wall #-}
module Debug(d1, d2) where

import qualified Debug.Trace as T
import Text.Show.Pretty

activateDebug :: Bool
activateDebug = True

d1 :: Show t => t -> t
d1 res = if activateDebug then T.trace (ppShow res) res else res

d2 :: Show a => a -> t -> t
d2 res2 res = if activateDebug then T.trace (ppShow res2) res else res
