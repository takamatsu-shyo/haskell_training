--module Main (main) where

import System.Random
import Control.Monad.State

type Maze = [[Char]]

generateMaze :: Int -> Int -> State StdGen Maze
generateMaze rows cols = do
    let maze = replicate rows (replicate cols '#')
    maze' <- generateMaze' 1 1 maze
    return maze'

generateMaze' :: Int -> Int -> Maze -> State StdGen Maze
generateMaze' row col maze = do
    let maze' = maze // [(row, maze !! row // [(col, '.')])]
    directions <- shffule [(0,2),(0,-2),(2,0),(-2,0)]

    newMaze <- foldM (\m (dr, dc) -> do
        let newRow = row + dr
        let newCol = col + dc
        if newRow >= 1 && newRow < length m - 1 && newCol >= 1 && newCol < length (m !! newRow) - 1 && m !! newRow !! newCol == '#'
            then do
                let m1 = m // [(row + (dr `div` 2), m !! (row + (dr `div` 2)) // [(col + (dc `div` 2), '.')])]
                let m2 = m1 // [(newRow, m1 !! newRow // [(newCol, ',')])]
                generateMaze' newRow newCol m2
            else return m
        ) maze' directions
    return newMaze

shffule :: [a] -> State StdGen [a]
shffule xs = do
    g <- get
    let (ys, g') = shffule' xs g
    put g'
    return ys

shffule' :: [a] -> StdGen -> ([a], StdGen)
shffule' [] g = ([], g)
shffule' xs g =
    let (n, g') = randomR (0, length xs - 1) g
        (ys, zs) = splitAt n xs
        x = head zs
    in (x : ys ++ tail zs, g')

(//) :: [a] -> [(Int, a)] -> [a]
(//) list updates = foldl (\acc (i, x) -> take i acc ++ [x] ++ drop (i+1) acc) list updates


main :: IO ()
main = do
    gen <- getStdGen
    let maze = evalState (generateMaze 21 21) gen
    displayMaze maze

displayMaze :: Maze -> IO ()
displayMaze = mapM_ putStrLn
