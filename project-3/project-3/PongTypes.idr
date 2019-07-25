module PongTypes

import Data.Vect

public export
VertList : Type
VertList = List (Double, Double, Double, Double)

public export
Vect2D : Type
Vect2D = Vect 4 Double

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
    puck : Puck
    p1 : GameObject
    p2 : GameObject

public export
data Rect = MkRect (Double, Double) (Double, Double)

