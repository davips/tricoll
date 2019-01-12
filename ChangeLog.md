# Revision history for tricoll

## TODO
* optimize
    use min(Ax-Bx, Ay-By) / (Va-Vb) as a rough estimate of t
    adopt a grid that indexes the positions to exclude non-neighbors from hit calculations (limit speed to guarantee no phantom balls)
    make a version walk-first-detect-hit-after
    create data structure to avoid useless recalculation of distant collisions (using arrays or minheap? implement in C or Haskell?)
    create hierarchies of balls to allow calculation of macro collisions and avoid unneeded calculations among macroballs
    define cubes (parallel to the axes) that enclose groups of balls and allow the use of the concept of neighborhood to reduce calculations among balls that are far from each other
* ball generator with auto mass calculator
* add gravity
* discover why calls to simFun are not sequential (is it because of slow iterations?) and FPS estimate is wrong
* find a better sollution than "positivate t = if t <= 0.00000000001 ..." for escaping balls

## 0.1.0.1  -- 2019-01-12
* consider mass in collisions (http://www.gamasutra.com/view/feature/3015/pool_hall_lessons_fast_accurate_.php?page=3)
* multithread

## 0.1.0.0  -- 2018-12-31
* Colliding balls inside a still cube.
