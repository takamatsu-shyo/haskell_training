import Control.Monad (replicateM_)

main :: IO ()
main = do
	putStrLn "How many times?"
	n <- readLn
--	replicateM_ n (putStrLn "Hello, world")
	replicateM_ n $ putStrLn "Hello, world"
