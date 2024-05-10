import Text.Printf
import Data.List

-- Constants
g :: Float
g = 9.81  -- Gravity
v0 :: Float
v0 = 4.0  -- Initial velocity (m/s)

-- Calculating the projectile's position
position :: Float -> Float -> (Float, Float)
position theta t = (x, y)
  where
    x = v0 * cos (theta * pi / 180) * t
    y = v0 * sin (theta * pi / 180) * t - 0.5 * g * t^2

-- Update grid function with boundary check
updateGrid :: [[Char]] -> Int -> Int -> [[Char]]
updateGrid grid x y
  | x < 0 || x >= length (head grid) || y < 0 || y >= length grid = grid
  | otherwise = take y grid ++ [updateLine (grid !! y) x] ++ drop (y + 1) grid

updateLine :: [Char] -> Int -> [Char]
updateLine line x
  | x < 0 || x >= length line = line
  | otherwise = take x line ++ ['*'] ++ drop (x + 1) line

-- Plot trajectory adjusts ratio and points based on grid size
plotTrajectory :: [(Float, Float)] -> IO ()
plotTrajectory points = do
  let maxY = maximum $ map snd points
      maxX = maximum $ map fst points
      ratioX = maxX / fromIntegral (width - 1)  -- scale based on grid width
      ratioY = maxY / fromIntegral (height - 1) -- scale based on grid height
      points' = [(floor (x / ratioX), floor (y / ratioY)) | (x, y) <- points]
      grid = replicate height (replicate width ' ') -- create empty grid
      grid' = foldl updateGrid grid points'
  putStrLn $ unlines $ reverse $ map concat $ map (map (:[])) grid'

-- Constants for grid size
width :: Int
width = 60

height :: Int
height = 20

-- Generate trajectory points
trajectory :: Float -> [(Float, Float)]
trajectory theta = takeWhile ((>=0) . snd) [position theta t | t <- [0,0.1..]]
--
---- ASCII Plot
--plotTrajectory :: [(Float, Float)] -> IO ()
--plotTrajectory points = do
--  let maxY = maximum $ map snd points
--      maxX = maximum $ map fst points
--      ratioX = maxX / 60  -- scale to 60 characters wide
--      ratioY = maxY / 20  -- scale to 20 characters high
--      points' = [(floor (x / ratioX), floor (y / ratioY)) | (x, y) <- points]
--      grid = [[' ' | _ <- [1..60]] | _ <- [1..20]]
--      grid' = foldl (\acc (x, y) -> updateGrid acc x y) grid points'
--  putStrLn $ unlines $ reverse $ map concat $ map (map (:[])) grid'
--
---- Update grid function
--updateGrid :: [[Char]] -> Int -> Int -> [[Char]]
--updateGrid grid x y = take y grid ++ [updateLine (grid !! y) x] ++ drop (y + 1) grid
--
--updateLine :: [Char] -> Int -> [Char]
--updateLine line x = take x line ++ ['*'] ++ drop (x + 1) line

-- Main function to handle IO
main :: IO ()
main = do
  putStrLn "Enter launch angle (degrees):"
  theta <- readLn
  plotTrajectory $ trajectory theta

