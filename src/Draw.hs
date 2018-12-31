module Draw(drawFun) where
import Vis
import World
import Linear

drawFun :: State -> VisObject Double --x=red y=green z=blue
drawFun (State t balls) = VisObjects $ [axes, cube] ++ spheres
  where
    axes = Axes (0.1, width)
    cube = Cube (width) Wireframe $ makeColor 1 1 1 1
    spheres = map draw balls
    draw (Ball _ r _ p _) = Trans p $ Sphere r Solid (makeColor 0.2 0.3 0.8 1) -- $ realToFrac 1.0)
    draw (Wall _ _) = error "Walls are drawn separately, as a cube, for now!"
