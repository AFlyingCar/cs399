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
DIMENSIONY = 400 

public export
DIMENSIONS:  (Int, Int)
DIMENSIONS = (DIMENSIONX, DIMENSIONY)

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
DEFAULT_VELOCITY = (1, 0)

-- TODO: Should this be hardcoded??
public export
DT: Double
DT = 0.1

public export
P1_XPOSITION: Double
P1_XPOSITION = 5

public export
P2_XPOSITION: Double
P2_XPOSITION = (cast DIMENSIONX) - 5

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
getKeyState: GlfwWindow -> FunctionKey -> KeyEventTy
getKeyState win key = (state (glfwGetFunctionKey win key)) where
    state: IO KeyEventTy -> KeyEventTy
    state keystate = unsafePerformIO keystate

-- https://www.youtube.com/watch?v=6LJExJ7vpYg
public export
matrixToList: (Vect n (Vect m Double)) -> List Double
matrixToList Nil = []
matrixToList matrix = accumMatrixValues matrix [] where
    accumMatrixValues: (Vect n (Vect m Double)) -> List Double -> List Double
    accumMatrixValues Nil values = values
    accumMatrixValues (row :: rest) values = accumMatrixValues rest (accumRowValues row values) where
        accumRowValues: (Vect m Double) -> List Double -> List Double
        accumRowValues Nil values = values
        accumRowValues (val :: rest) values = accumRowValues rest (val :: values)

public export
getPlayer1Transform: PongState -> List Double
getPlayer1Transform (MkPongState _ _ _ h _) = matrixToList (translate [P1_XPOSITION, h, 1.0])

public export
getPlayer2Transform: PongState -> List Double
getPlayer2Transform (MkPongState _ _ _ _ h) = matrixToList (translate [P2_XPOSITION, h, 1.0])

public export
getPuckTransform: PongState -> List Double
getPuckTransform (MkPongState _ (x, y) _ _ _) = matrixToList (translate [x, y, 1.0])

public export
gameLoop: GlfwWindow -> PongState -> PongState
gameLoop win pongState@(MkPongState (p1_score, p2_score) (puckx, pucky) vel p1_height p2_height) = do
    let up_key = getKeyState win GLFW_KEY_UP
    let down_key = getKeyState win GLFW_KEY_DOWN
    let new_puck_pos = updatePuckPos DT (puckx, pucky) vel
    let new_puck_vel = updatePuckVel new_puck_pos vel
    let new_score = updateScore p1_score p2_score new_puck_pos

    -- TODO: How shall we update p2_height?
    if up_key == GLFW_PRESS
        then MkPongState new_score new_puck_pos new_puck_vel (p1_height + 1) (p2_height)
    else if down_key == GLFW_PRESS
        then MkPongState new_score new_puck_pos new_puck_vel (p1_height - 1) (p2_height)
        else MkPongState new_score new_puck_pos new_puck_vel p1_height p2_height

