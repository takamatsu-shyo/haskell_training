circumference :: Double -> Double
circumference r = 2 * pi * r

areaOfCircle :: Double -> Double
areaOfCircle r = r * r * pi

main :: IO ()
main = do
    putStrLn "Enter the radius of the circle:"
    radius <- readLn
    let area = areaOfCircle radius
        circ = circumference radius
    putStrLn $ "Area         : " ++ show area
    putStrLn $ "Circumference: " ++ show circ
