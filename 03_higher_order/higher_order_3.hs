import Data.Char (toLower)
import Data.Text (strip, pack, unpack)

trim :: String -> String
trim = unpack . strip . pack

normalize :: String -> String
normalize = trim . map toLower

main :: IO ()
main = putStrLn $ normalize "     HeLLO WoRlD   "
