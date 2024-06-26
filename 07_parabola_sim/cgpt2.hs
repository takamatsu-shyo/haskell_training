import Text.Printf
import Data.List
import Data.Char (intToDigit)

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

-- Generate trajectory points
trajectory :: Float -> [(Float, Float)]
trajectory theta = takeWhile ((>=0) . snd) [position theta t | t <- [0,0.01..]]

-- ASCII Plot
plotTrajectory :: [(Float, Float)] -> IO ()
plotTrajectory points = do
  let maxY = maximum $ map snd points
      maxX = maximum $ map fst points
      ratioX = maxX / fromIntegral (width - 1)  -- scale based on grid width
      ratioY = maxY / fromIntegral (height - 1) -- scale based on grid height
      --points' = [(floor (x / 1), floor (y / 1)) | (x, y) <- points]
      points' = [(floor (x / ratioX), floor (y / ratioY)) | (x, y) <- points]
      grid = replicate height (replicate width ' ') -- create empty grid
      grid' = foldl (\acc (x, y) -> updateGrid acc x y) grid points' -- Correct usage
  putStrLn $ unlines $ reverse $ map concat $ map (map (:[])) grid'

-- Update grid function with boundary check
updateGrid :: [[Char]] -> Int -> Int -> [[Char]]
updateGrid grid x y
  | x < 0 || x >= length (head grid) || y < 0 || y >= length grid = grid
  | otherwise = take y grid ++ [updateLine (grid !! y) x] ++ drop (y + 1) grid

updateLine :: [Char] -> Int -> [Char]
updateLine line x
  | x < 0 || x >= length line = line
  | otherwise = take x line ++ ['*'] ++ drop (x + 1) line

-- Constants for grid size
width :: Int
width = 60

height :: Int
height = 20

-- Main function to handle IO
main :: IO ()
main = do
  putStrLn "Enter launch angle (degrees):"
  theta <- readLn
  plotTrajectory $ trajectory theta

