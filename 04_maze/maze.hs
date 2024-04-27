type Maze = [[Char]]

generateMaze3 :: Int -> Int -> Char -> Maze
generateMaze3 rows cols wallChar =
    let rowWall = replicate cols wallChar
        cell = wallChar : replicate (cols - 2) '.' ++ [wallChar]
        mazeMiddle = replicate (rows - 2) cell
	in [rowWall] ++ mazeMiddle ++ [rowWall]

generateMaze2 :: Int -> Int -> Char -> Maze
generateMaze2 rows cols wallChar =
    let rowWall = replicate cols wallChar
        cell = replicate (cols - 2) '.' -- -2 to leave space for walls
        mazeMiddle = [cell | _ <- [1..rows-2]]
    in [rowWall] ++ mazeMiddle ++ [rowWall]

generateMaze :: Int -> Int -> Maze
generateMaze row cols = replicate row (replicate cols '#')

displayMaze :: Maze -> IO ()
displayMaze = mapM_ putStrLn

main :: IO ()
main = do
    let maze = generateMaze3 10 10 '#'
    displayMaze maze
