import Control.Monad (replicateM_)

add :: Int -> Int -> Int
add x y = x + y

main :: IO ()
-- main = replicateM_ 10 (putStrLn "Hello 5")
-- main = putStrLn $ "1 + 1 is " ++ show (1 + 1) 
-- main = putStrLn $ "1 + 1 is " ++ show (add 1 1)
main = putStrLn $ show $ 1 + 1
