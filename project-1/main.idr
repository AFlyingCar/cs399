
import digipen.instructor.conway.spec
import digipen.instructor.conway.impl

area0 : Area
area0 = Extents(0,0)(5,5)

board0 : List (List Bool)
board0 = [[False, False, False, False, False],
          [False, True, True, True, False],
          [False, False, False, False, False],
          [False, False, False, False, False],
          [False, False, False, False, False]]

world0 : World ()
world0 = start area0 (\(x,y) => GetValueFrom (toNat x) False (GetValueFrom (toNat y) Nil board0) )

world1 : World (Position->Bool)
world1 = evolve world0

check_w1 : (Position->Bool)
check_w1 = value world1

drawBoardNC : List(List Nat) -> IO ()
drawBoardNC board_nc = sequence_( map( putStrLn . rowString ) ys ) where
  xs : List Int
  xs = enumFromTo 0 5
  ys : List Int
  ys = enumFromTo 0 5

  getRow : Nat->List (List Nat)->List Nat
  getRow idx list = GetValueFrom (toNat idx) Nil list

  cellString : Position -> String
  cellString (x,y) = show (GetValueFrom (toNat x) 0 (getRow (toNat y) board_nc))

  rowString : Int -> String
  rowString y = unwords (map(\x => cellString (x,y)) xs)

ncb0 : List (List Nat)
ncb0 = GetNCBoard world0

main : IO ()
-- main = let ld = Prelude.Basics.snd (value (cell (2,1) world0)) in
--           putStrLn (if ld then "Live" else "Dead")
-- main = putStrLn (show (value (countNeighbors (cell (2,1) world0))))
-- main = putStrLn (show (check_w1 (0,0)))
main = (drawBoardNC ncb0)
-- main = putStrLn (show (GetValueFrom2D (2,1) False board0))
-- main = putStrLn (show (value (isLive (cell (2,1) world0))))
-- main = drawBoard area0 (\(x,y) => GetValueFrom (toNat x) False (GetValueFrom (toNat y) Nil board0) ) -- 0
-- main = drawBoard area0 (value (evolve world0))                                                    -- 1
-- main = drawBoard area0 (value (evolve (evolve world0)))                                           -- 2

