type Maze = [[Char]]

generateMaze :: Int -> Int -> Maze
generateMaze row cols = replicate row (replicate cols '#')

displayMaze :: Maze -> IO ()
displayMaze = mapM_ putStrLn

main :: IO ()
main = do
    let maze = generateMaze 10 10
    displayMaze maze
