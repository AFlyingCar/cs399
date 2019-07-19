module Pong

import Data.Vect

import Graphics.Util.Glfw
import Graphics.Util.Transforms
import Graphics.Rendering.Gl

public export
VertList : Type
VertList = List (Double, Double, Double, Double)

public export
Vect2D : Type
Vect2D = Vect 4 Double

public export
vertices : VertList
vertices = [
  (0.0, 0.0, 0.0, 1.0),
  (0.1, 0.0, 0.0, 1.0),
  (0.0, 0.5, 0.0, 1.0),
  (0.1, 0.5, 0.0, 1.0)
  ]

public export
puck_verts : VertList
puck_verts = [
  (0.0, 0.0, 0.0, 1.0),
  (0.05, 0.0, 0.0, 1.0),
  (0.0, 0.05, 0.0, 1.0),
  (0.05, 0.05, 0.0, 1.0)
  ]

-- list of vertices, and a position
public export
data GameObject = MkGameObject VertList Vect2D

-- same as GO, but also has a velocity
public export
data Puck = MkPuck GameObject Vect2D

-- (P1 Score, P2 Score) (PuckX, PuckY) P1Height P2Height
public export
record PongState where
    constructor MkPongState
    scores : (Integer, Integer)
    puck_pos : (Double, Double)
    puck_vel : (Double, Double)
    p1 : GameObject
    p2 : GameObject

public export
DIMENSIONX:  Int
DIMENSIONX = 800

public export
DIMENSIONY:  Int
DIMENSIONY = 600 

public export
HALF_DIMX: Double
HALF_DIMX = (cast DIMENSIONX) / 2

public export
HALF_DIMY: Double
HALF_DIMY = (cast DIMENSIONY) / 2

public export
DIMENSIONS:  (Int, Int)
DIMENSIONS = (DIMENSIONX, DIMENSIONY)

public export 
FOV: Angle
FOV = Degree 45.0

public export
ASPECT: Double
ASPECT = (cast DIMENSIONX) / (cast DIMENSIONY)

public export 
NEARPLANE: Double
NEARPLANE = 0.1

public export
FARPLANE: Double
FARPLANE = 10

public export
CENTERX:  Double
CENTERX = (cast DIMENSIONX) / 2.0

public export
CENTERY:  Double
CENTERY = (cast DIMENSIONY) / 2.0

public export
CENTER: (Double, Double)
CENTER = (CENTERX, CENTERY)

public export
DEFAULT_VELOCITY: (Double, Double)
DEFAULT_VELOCITY = (-2, 0)

-- TODO: Should this be hardcoded??
public export
DT: Double
DT = 0.1

public export
P1_XPOSITION: Double
P1_XPOSITION = -0.8

public export
P2_XPOSITION: Double
P2_XPOSITION = 0.8

public export
P1_INIT_POSITION: Vect2D
P1_INIT_POSITION = fromList [-0.8, HALF_DIMY, 0.0, 1.0]

public export
P2_INIT_POSITION: Vect2D
P2_INIT_POSITION = fromList [0.8, HALF_DIMY, 0.0, 1.0]

public export
Z_COORDINATE: Double
Z_COORDINATE = 0

public export
MOVE_SPEED: Double
MOVE_SPEED = 1

public export
MAX_Y_VALUE: Double
MAX_Y_VALUE = (cast DIMENSIONY) - 200

public export
MIN_Y_VALUE: Double
MIN_Y_VALUE = 50

public export
data Rect = MkRect (Double, Double) (Double, Double)

public export
isColliding: Rect -> Rect -> Bool
isColliding (MkRect (minx1, miny1) (maxx1, maxy1)) (MkRect (minx2, miny2) (maxx2, maxy2)) =
  ((minx1 + (maxx1 - minx1)) >= minx2) && ((minx2 + (maxx2 - minx2)) >= minx1) && ((miny1 + (maxy1 - miny1)) >= miny2) && ((miny2 + (maxy2 - miny2)) >= miny1)

public export
minDouble: List Double -> Double
minDouble doubles = helper doubles (negate 20000) where
  helper : List Double -> Double -> Double
  helper (d :: rest) smallest = if d < smallest then d else (helper rest smallest)
  helper Nil smallest = smallest

public export
maxDouble: List Double -> Double
maxDouble doubles = helper doubles 20000000 where
  helper : List Double -> Double -> Double
  helper (d :: rest) biggest = if d > biggest then d else (helper rest biggest)
  helper Nil biggest = biggest

public export
makeRectFromVerts: List (Double, Double, Double, Double) -> Rect
makeRectFromVerts [(v1x, v1y, _, _), (v2x, v2y, _, _), (v3x, v3y, _, _), (v4x, v4y, _, _)] = MkRect (minDouble [v1x, v2x, v3x, v4x], minDouble [v1y, v2y, v3y, v4y]) (maxDouble [v1x, v2x, v3x, v4x], maxDouble [v1y, v2y, v3y, v4y])
makeRectFromVerts Nil = MkRect (0, 0) (0, 0) -- Error Case

public export 
updateScore: Integer -> Integer -> (Double, Double) -> (Integer, Integer)
updateScore p1 p2 puck@(x, _) = if x <= 0
                                   then (p1, p2 + 1)
                                else if x >= (cast DIMENSIONX)
                                   then (p1 + 1, p2)
                                   else (p1, p2)

-- Make sure that we "bounce" on the top and bottom of the screen, and reset at the edges
public export 
updatePuckPos: Double -> (Double, Double) -> (Double, Double) -> (Double, Double)
updatePuckPos dt old_pos@(x,y) vel@(i, j) = if (x <= 0) || (x >= (cast DIMENSIONX))
                                                then CENTER
                                                else if (y <= 0) || (y >= (cast DIMENSIONY))
                                                    then (x + (i * dt), y)
                                                    else (x + (i * dt), y + (j * dt))

-- Note: we don't actually need DT here, since velocity is constant unless
--  colliding with a puck
-- When we collide with the top or bottom of the screen, invert the y-velocity
--  to simulate a "bouncing" effect
public export 
updatePuckVel: (Double, Double) -> (Double, Double) -> (Double, Double)
updatePuckVel pos@(x,y) old_vel@(i, j) = if (x <= 0) || (x >= (cast DIMENSIONX))
                                            then DEFAULT_VELOCITY
                                            else if (y <= 0) || (y >= (cast DIMENSIONY))
                                                then (i, -j)
                                                else (i, j)

public export 
getFunctionKeyState: GlfwWindow -> FunctionKey -> KeyEventTy
getFunctionKeyState win key = (state (glfwGetFunctionKey win key)) where
    state: IO KeyEventTy -> KeyEventTy
    state keystate = unsafePerformIO keystate

public export
getKeyState: GlfwWindow -> Char -> KeyEventTy
getKeyState win key = (state (glfwGetKey win key)) where
    state: IO KeyEventTy -> KeyEventTy
    state keystate = unsafePerformIO keystate

public export
worldYPosToScreenYPos: Double -> Double
worldYPosToScreenYPos y = (y - HALF_DIMY) / HALF_DIMY

public export
worldXPosToScreenXPos: Double -> Double
worldXPosToScreenXPos x = (x - HALF_DIMX) / HALF_DIMX

-- https://www.youtube.com/watch?v=6LJExJ7vpYg
public export
getPlayer1Transform: PongState -> TransformationMatrix
getPlayer1Transform (MkPongState _ _ _ (MkGameObject _ (_ :: h :: _ :: _)) _) = translate [P1_XPOSITION, worldYPosToScreenYPos h, 0]

public export
getPlayer2Transform: PongState -> TransformationMatrix
getPlayer2Transform (MkPongState _ _ _ _ (MkGameObject _ (_ :: h :: _ :: _))) = translate [P2_XPOSITION, worldYPosToScreenYPos h, 0]

public export
getPuckTransform: PongState -> TransformationMatrix
getPuckTransform (MkPongState _ (x, y) _ _ _) = translate [worldXPosToScreenXPos x, worldYPosToScreenYPos y, 0]

public export
movePlayerUp: Double -> Double
movePlayerUp height = if height >= MAX_Y_VALUE -- TODO
                          then height
                          else height + MOVE_SPEED

public export
movePlayerDown: Double -> Double
movePlayerDown height = if height <= MIN_Y_VALUE
                           then height
                           else height - MOVE_SPEED

public export
updatePuck: PongState -> ((Double, Double), (Double, Double))
updatePuck (MkPongState s (x, y) (i, j) p1 p2) = do
  let paddle_rect = makeRectFromVerts vertices
  let puck_rect = makeRectFromVerts puck_verts

  let new_pos = updatePuckPos DT (x, y) (i, j)
  let new_vel = updatePuckVel new_pos (i, j)

  (new_pos, new_vel)

public export
updatePlayer: GameObject -> (KeyEventTy, KeyEventTy) -> GameObject
updatePlayer (MkGameObject verts (x :: y :: z :: w)) (up, down) =  do
  let new_y = if up == GLFW_PRESS
                then movePlayerUp y
              else if down == GLFW_PRESS
                then movePlayerDown y
                else y
  MkGameObject verts (x :: new_y :: z :: w)

public export
gameLoop: GlfwWindow -> PongState -> PongState
gameLoop win pongState@(MkPongState (p1_score, p2_score) (puckx, pucky) vel p1 p2) = do
    let up_key = getFunctionKeyState win GLFW_KEY_UP
    let down_key = getFunctionKeyState win GLFW_KEY_DOWN

    let w_key = getKeyState win 'w'
    let s_key = getKeyState win 's'

    let (new_puck_pos, new_puck_vel) = updatePuck pongState

    let new_score = updateScore p1_score p2_score new_puck_pos

    let new_p1 = updatePlayer p1 (w_key, s_key)
    let new_p2 = updatePlayer p2 (up_key, down_key)

    MkPongState new_score new_puck_pos new_puck_vel new_p1 new_p2

