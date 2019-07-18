module Pong

import Data.Vect

import Graphics.Util.Glfw
import Graphics.Util.Transforms
import Graphics.Rendering.Gl

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

-- (P1 Score, P2 Score) (PuckX, PuckY) P1Height P2Height
public export
record PongState where
    constructor MkPongState
    scores : (Integer, Integer)
    puck_pos : (Double, Double)
    puck_vel : (Double, Double)
    p1_height : Double
    p2_height : Double

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
getPlayer1Transform (MkPongState _ _ _ h _) = translate [P1_XPOSITION, worldYPosToScreenYPos h, 0]

public export
getPlayer2Transform: PongState -> TransformationMatrix
getPlayer2Transform (MkPongState _ _ _ _ h) = translate [P2_XPOSITION, worldYPosToScreenYPos h, 0]

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
gameLoop: GlfwWindow -> PongState -> PongState
gameLoop win pongState@(MkPongState (p1_score, p2_score) (puckx, pucky) vel p1_height p2_height) = do
    let up_key = getFunctionKeyState win GLFW_KEY_UP
    let down_key = getFunctionKeyState win GLFW_KEY_DOWN

    let w_key = getKeyState win 'w'
    let s_key = getKeyState win 's'

    let new_puck_pos = updatePuckPos DT (puckx, pucky) vel
    let new_puck_vel = updatePuckVel new_puck_pos vel
    let new_score = updateScore p1_score p2_score new_puck_pos

    let new_p1_pos = if up_key == GLFW_PRESS
                         then movePlayerUp p1_height
                     else if down_key == GLFW_PRESS
                         then movePlayerDown p1_height
                         else p1_height

    let new_p2_pos = if w_key == GLFW_PRESS
                         then movePlayerUp p2_height
                     else if s_key == GLFW_PRESS
                         then movePlayerDown p2_height
                         else p2_height

    MkPongState new_score new_puck_pos new_puck_vel new_p1_pos new_p2_pos

