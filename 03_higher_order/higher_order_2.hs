doubleList :: [Int] -> [Int]
doubleList = map (*2)

filterEven :: [Int] -> [Int]
filterEven = filter even

sumOfSquare :: [Int] -> Int
sumOfSquare = sum . map (^2)

main :: IO ()
main = do
    let numbers = [1, 2, 3, 4, 6]
        doubled = doubleList numbers
        filtered = filterEven numbers
        squared = sumOfSquare numbers
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Doubled list:  " ++ show doubled
    putStrLn $ "Filtered list: " ++ show filtered
    putStrLn $ "Squread list:  " ++ show squared
