sieve :: [Int] -> [Int]
sieve [] = []
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primeUpTo :: Int -> [Int]
primeUpTo n = sieve [2..n]

main :: IO ()
main = do
    let list = [x | x <- [1..10]]
    print list

    let square = [x^2 | x <- [1..10]]
    print square

    let even = [x | x <- [1..20], x `mod` 2 == 0]
    print even

    let pairs = [(x,y) | x <- [1, 2, 3], y <- [4, 5]]
    print pairs

    let filterSquares = [x^2 | x <- [1..100], (x^2) `mod` 3 == 0] 
    print filterSquares

    let primeUp100 = primeUpTo 100000
    print primeUp100
