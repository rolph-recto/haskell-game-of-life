--life.hs
--Conway's Game of Life simulator
--by Rolph Recto

--Board datatype
--Defines (in order) width, height, and array of values
data Board = Board [[Int]] deriving (Show)

--Create a new board from an existing array
createBoard :: [[Int]] -> Board
createBoard array = Board array

--Returns width of board
--This function assumes that the board as at least 1 row
boardWidth :: Board-> Int
boardWidth (Board array) = length (array !! 0)

--Returns height of board
boardHeight :: Board -> Int
boardHeight (Board array) = length array

--Returns the value at a certain position
boardValue :: Board -> Int -> Int -> Int
boardValue (Board array) x y = (array !! y) !! x

--Updates the board using a rule function
--The rule function takes in the coordinates of a cell
--and returns a new value for that cell
updateBoard :: Board -> (Board -> Int -> Int -> Int) -> Board
updateBoard board rule = createBoard [ [ (rule board x y) | x <- [0..((boardWidth board)-1)]] | y <- [0..((boardHeight board)-1)] ]

--utility for Game of Life rule function
--determines if a board position is valid
validPosition :: Board -> Int -> Int -> Bool
validPosition board x y = if x >= 0 && x < (boardWidth board) && y >= 0 && y < (boardHeight board) then True else False

--utility for Game of Life rule function
--counts live neighbors adjacent to a cell
countLiveNeighbors :: Board -> Int -> Int -> Int
countLiveNeighbors board x y =
	(if validPosition board (x-1) (y-1) then (boardValue board (x-1) (y-1)) else 0) + 
	(if validPosition board (x) (y-1) then (boardValue board (x) (y-1)) else 0) + 
	(if validPosition board (x+1) (y-1) then (boardValue board (x+1) (y-1)) else 0) + 
	(if validPosition board (x+1) (y) then (boardValue board (x+1) (y)) else 0) + 
	(if validPosition board (x+1) (y+1) then (boardValue board (x+1) (y+1)) else 0) + 
	(if validPosition board (x) (y+1) then (boardValue board (x) (y+1)) else 0) + 
	(if validPosition board (x-1) (y+1) then (boardValue board (x-1) (y+1)) else 0) + 
	(if validPosition board (x-1) (y) then (boardValue board (x-1) (y)) else 0)

--rule function for Conway's Game of Life
gameOfLife board x y =
	let neighbors = countLiveNeighbors board x y
	in if (boardValue board x y) == 1 then
			--a live cell must have exactly 2 or 3 live neighbors to live
			--too few (<2) and the cell will die of "loneliness"
			--too many (>3) and the cell will die of "overcrowding"
			if neighbors < 2 || neighbors > 3 then 0 else 1
		else
			--a dead cell revives with exactly 3 neighbors from "reproduction"
			if neighbors == 3 then 1 else 0