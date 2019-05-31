
import digipen.instructor.conway.spec
import digipen.instructor.conway.impl

area0 : Area
area0 = Extents(0,0)(10,10)

-- Blinker
board0 : List (List Bool)
board0 = [[False, False, False, False, False],
          [False, True, True, True, False],
          [False, False, False, False, False],
          [False, False, False, False, False],
          [False, False, False, False, False]]

-- Beacon
board1 : List (List Bool)
board1 = [[False, False, False, False, False],
          [False, True, True, False, False],
          [False, True, True, False, False],
          [False, False, False, True, True],
          [False, False, False, True, True]]

-- Glider
board2 : List (List Bool)
board2 = [[False, False, True, False, False],
          [True, False, True, False, False],
          [False, True, True, False, False],
          [False, False, False, False, False],
          [False, False, False, False, False]]

-- Different stages of evolution for the world, for ease of use
world0 : World ()
world0 = start area0 (\(x,y) => GetValueFrom (toNat x) False (GetValueFrom (toNat y) Nil board2) )
world1 : World (Position->Bool)
world1 = evolve world0
world2 : World (Position->Bool)
world2 = evolve world1
world3 : World (Position->Bool)
world3 = evolve world2
world4 : World (Position->Bool)
world4 = evolve world3

-- Helper function to draw the board, but with neighbor counts rather than live/dead
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

-- The Neighbor Count board of different stages of the world evolution, for ease of use
ncb0 : List (List Nat)
ncb0 = GetNCBoard world0
ncb1 : List (List Nat)
ncb1 = GetNCBoard world1

main : IO ()
-- main = (drawBoardNC ncb0)
-- main = drawBoard area0 (\(x,y) => GetValueFrom (toNat x) False (GetValueFrom (toNat y) Nil board2) ) -- 0
main = drawBoard area0 (value world4)                                                                -- N

