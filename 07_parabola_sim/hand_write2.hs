import Text.Printf
import Data.List
import Data.Char (intToDigit)
import Data.Maybe
import Text.Read (readMaybe)

-- Constants
g :: Float
g = 9.81 -- Gravity
v0 :: Float
v0 = 200 -- Initial velocity (m/s)

-- Input validation
getValidInput :: String -> IO Float
getValidInput prompt = do
    putStrLn prompt
    input <- getLine
    case readMaybe input of
        Just value ->
            if value < 0
            then do
                putStrLn "Negative value is not acceptable. Set it to zero"
                return 0
            else return value

-- Calculating the projectile's position
position :: Float -> Float -> Float -> (Float, Float)
position theta t y0 = (x, y)
    where 
        vx = cos (theta * pi / 180) :: Float
        vy = sin (theta * pi / 180) :: Float
        x = v0 * vx * t
        y = v0 * vy * t - 0.5 * g * t^2 + y0

-- Generate trajectory points
trajectory :: Float -> Float -> [(Float, Float)]
trajectory theta y0 = takeWhile ((>=0) . snd) [position theta t y0 | t <- [0,0.01..]]

-- ASCII plot
plotTrajectory :: [(Float, Float)] -> Float -> IO ()
plotTrajectory points y0 = do
    let maxY = maximum $ map snd points
        maxX = maximum $ map fst points
        ratioX = 6000 / fromIntegral (width - 1)
        ratioY = 3500 / fromIntegral (height - 1)

        points' = [(floor (x / ratioX), floor(y / ratioY)) | (x,y) <- points]
        grid = replicate height (replicate width ' ')
        grid' = foldl (\acc (x,y) -> updateGrid acc x y) grid points'
    putStrLn $ unlines $ reverse $ map concat $ map (map (:[])) grid'

maxHeight :: [(Float, Float)] -> Float
maxHeight points = maximum $ map snd points

-- Update grid function with boundary check
updateGrid :: [[Char]] -> Int -> Int -> [[Char]]
updateGrid grid x y
    | x < 0 || x >= length (head grid) || y < 0 || y >= length grid = grid
    | otherwise = take y grid ++ [updateLine (grid !! y) x] ++ drop (y + 1) grid

updateLine :: [Char] -> Int -> [Char]
updateLine line x
    | x < 0 || x >= length line = line
    | otherwise = take x line ++ ['*'] ++ drop (x + 1) line

width :: Int
width = 80

height :: Int
height = 20

main :: IO()
main = do
    --putStrLn "Enter launch angle (deg):"
    theta <- getValidInput "Enter launch angle (deg):"
    putStrLn "Enter launch hight (meter):"
    y0 <- readLn 

    let traj = trajectory theta y0

    plotTrajectory traj y0
    let (lastX, _) = last traj
    printf "Flying distance: %.2f meters\n" lastX

    let highestY = maxHeight traj
    printf "Highest hight: %.2f meters\n" highestY

