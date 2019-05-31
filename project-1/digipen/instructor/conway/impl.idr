module digipen.instructor.conway.impl

import digipen.instructor.conway.spec

-- Easy conversion from
public export
BoolToNat : Bool -> Nat
BoolToNat True = 1
BoolToNat False = 0

-- template<typename a>
-- a GetValueFrom(Nat idx, a err_val, List<a> list)
public export
GetValueFrom : Nat -> Lazy a -> List a -> a
GetValueFrom idx err_val Nil = err_val
GetValueFrom idx err_val list = maybe err_val id (Prelude.List.index' idx list)

public export
GetValueFrom2D : (Nat,Nat) -> Lazy a -> List (List a) -> a
GetValueFrom2D pos err_val Nil = err_val
GetValueFrom2D (x,y) err_val list = GetValueFrom x err_val (GetValueFrom y Nil list)

public export
record World a where
  constructor MkWorld
  area : Area
  cells : List (List Bool)

  value : a

public export
GetNeighborCount : Position -> World () -> Nat

public export
GetNCBoard : World a -> List(List Nat)

public export
implementation GameOfLife World where
  -- An opaque reference to a cell in some LifeWorld context.
  -- Cell : Type
  -- (x, y)
  -- True = Alive
  -- False = Dead
  Cell = (Position, Bool)

  -- Initialize a LifeWorld with a given liveness function.
  -- start : Area -> ( Position -> Bool ) -> LifeWorld ()
  start area live_func = let Extents(x1,y1)(x2,y2) = area
                             xs = enumFromTo( min x1 x2 )( max x1 x2 )
                             ys = enumFromTo( min y1 y2 )( max y1 y2 )
                         in
                           MkWorld area [[live_func (x,y) | x <- xs] | y <- ys] ()

  -- Produce a new LifeWorld state and return its liveness function.
  -- evolve : LifeWorld a -> LifeWorld( Position -> Bool )
  evolve (MkWorld area cells _) = MkWorld area cells
                                  (\(x,y) => let nc_board = GetNCBoard (MkWorld area cells ())
                                                 num_neigh = GetValueFrom2D (toNat x, toNat y) 0 nc_board
                                                 world = MkWorld area cells ()
                                                 is_live = snd (value (cell (x,y) (MkWorld area cells ())))
                                             in
                                             if is_live && num_neigh < 2 then False else
                                               if is_live && (num_neigh ==  2 || num_neigh == 3) then True else
                                                 if is_live && num_neigh > 3 then False else
                                                   if (not is_live) && num_neigh == 3 then True else is_live)

--                                (\pos => let num_neigh = value (countNeighbors (cell pos (MkWorld area cells ()))) in
--                                         if num_neigh < 2 then False else
--                                           if num_neigh ==  2 || num_neigh == 3 then True else
--                                             if num_neigh > 3 then False else False)

  -- Create a way to look up information about a cell within some
  -- LifeWorld's context.
  -- cell : Position -> LifeWorld () -> LifeWorld Cell
  cell (x, y) (MkWorld area cells _) = let Extents(x1,y1)(x2,y2) = area in
                                          MkWorld area cells
                                            ((x, y),
                                             (GetValueFrom2D (toNat (x - x1), toNat (y - y1)) False cells))

  -- Check how many neighbors some cell has.
  -- countNeighbors : LifeWorld Cell -> LifeWorld Nat
  countNeighbors (MkWorld area cells value) =  let Extents(x1,y1)(x2,y2) = area
                                                   ((x,y), _) = value
                                               in
                                          MkWorld area cells (
                                            BoolToNat (GetValueFrom2D (toNat (x - x1), toNat (y + 1 - y1)) False cells) +
                                            BoolToNat (GetValueFrom2D (toNat x,        toNat (y + 1 - y1)) False cells) +
                                            BoolToNat (GetValueFrom2D (toNat (x + x1), toNat (y + 1 - y1)) False cells) +
                                            BoolToNat (GetValueFrom2D (toNat (x - x1), toNat y) False cells) +
                                            BoolToNat (GetValueFrom2D (toNat (x + x1), toNat y) False cells) +
                                            BoolToNat (GetValueFrom2D (toNat (x - x1), toNat (y - 1 - y1)) False cells) +
                                            BoolToNat (GetValueFrom2D (toNat x,        toNat (y - 1 - y1)) False cells) +
                                            BoolToNat (GetValueFrom2D (toNat (x + x1), toNat (y - 1 - y1)) False cells)
                                          )

  -- Check if some cell is alive.
  -- isLive : LifeWorld Cell -> LifeWorld Bool
  isLive (MkWorld area cells value) = MkWorld area cells (snd value)

  -- Discard the world context from a value.
  -- extract : LifeWorld a -> a
  extract world = value world

-- GetNeighborCount : Position -> World () -> Nat
GetNeighborCount (x,y) (MkWorld area cells _) =
  value (countNeighbors (cell (x,y) (MkWorld area cells ())))

-- GetNCBoard : World a -> List(List Nat)
GetNCBoard (MkWorld (Extents(x1,y1)(x2,y2)) cells _) =
      let world = MkWorld (Extents(x1,y1)(x2,y2)) cells ()
          xs = enumFromTo( min x1 x2 )( max x1 x2 )
          ys = enumFromTo( min y1 y2 )( max y1 y2 )
      in
      [[(GetNeighborCount (x,y) world) | x <- xs ] | y <- ys ]

