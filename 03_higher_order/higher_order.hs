doubleList :: [Int] -> [Int]
doubleList = map (*2)

main :: IO ()
main = do
    let numbers = [1, 2, 3, 4, 6]
        doubled = doubleList numbers
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Doubled list:  " ++ show doubled
