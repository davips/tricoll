# Revision history for tricoll

## TODO
* for bottom and top walls the time-to-hit calculation should consider g (perhaps including a g vector in Wall)
* replace (t = 0.999999 * ...) by a better method to avoid:
    1) deadlock after moving to collision position, where it takes zero time to hit and at the same time nobody knows if the object is coming or going away;
    2) missing collisions when using energy dissipation (elasticity < 1) or, currently, maybe, in other rare cases; 
* elasticity should affect only the normal component when colliding to a wall, not the whole speed vector
* optimize
    use min(Ax-Bx, Ay-By) / (Va-Vb) as a rough estimate of t
    adopt a grid that indexes the positions to exclude non-neighbors from hit calculations (limit speed to guarantee no phantom balls)
    make a version walk-first-detect-hit-after
    create data structure to avoid useless recalculation of distant collisions (using arrays or minheap? implement in C or Haskell?)
    create hierarchies of balls to allow calculation of macro collisions and avoid unneeded calculations among macroballs
    define cubes (parallel to the axes) that enclose groups of balls and allow the use of the concept of neighborhood to reduce calculations among balls that are far from each other
* check if the sollution for escaping balls has any drawback (t = 0.999999 * ...)
* discover why (and reimplement not-gloss?) calls to simFun are not sequential (is it because of slow iterations? bug in not-gloss?) and FPS estimate is wrong

## 0.1.0.3  -- 2019-01-16
* now, walls are perfectly ellastic as originally

## 0.1.0.2  -- 2019-01-12
* fix increase of total energy (kinetic + potential) during simulation with gravity ("a * dt^2 / 2" was missing)
* add gravity
* new dimensions to accomodate the real value of gravity
* add friction (loss of energy as heat in collisions)

## 0.1.0.1  -- 2019-01-12
* consider mass in collisions (http://www.gamasutra.com/view/feature/3015/pool_hall_lessons_fast_accurate_.php?page=3)
* multithread

## 0.1.0.0  -- 2018-12-31
* Colliding balls inside a still cube.
