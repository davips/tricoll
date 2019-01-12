module Draw where
import Vis
import World

fps :: Double
fps = 20

drawFun :: State -> VisObject Double --x=red y=green z=blue
drawFun (State elapsed objs frames) = VisObjects $ [axes, cube, text] ++ spheres
  where
    axes = Axes (0.1, width)
    cube = Cube (width) Wireframe $ makeColor 1 1 1 1
    spheres = map draw objs
    draw (Ball _ r _ p _) = Trans p $ Sphere r Solid (makeColor 0.2 0.3 0.8 1) -- $ realToFrac 1.0)
    draw (Wall _ _) = error "Walls are drawn separately, as a cube, for now!"
    rate = if elapsed == 0 then -1 else fo $ fromIntegral frames / elapsed
    text = Text2d (show rate ++ " fps") (1, 1) TimesRoman24 $ makeColor 1 1 1 1

fo :: Double -> Double
fo x = fromIntegral (round (1000.0 * x) :: Int) / 1000.0
    
