
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

world0 : GameOfLife w => w ()
world0 = start area0 (\(x,y) => GetValueFrom (toNat x) False (GetValueFrom (toNat y) Nil board0) )

main : IO ()
main = drawBoard area0 (value (evolve world0))

