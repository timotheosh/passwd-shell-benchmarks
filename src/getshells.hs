import Data.List (reverse)
import Data.Map (fromListWith, toList)
import System.IO ()

main :: IO ()
main = do
  contents <- readFile "passwd"
  let shells = getFrequency . map getShell $ lines contents
  let output = map formatOutput shells

  mapM_ putStrLn output

getShell :: String -> String
getShell = reverse . takeWhile (/= ':') . dropWhile (== '\r') . reverse

getFrequency :: [String] -> [(String, Int)]
getFrequency xs = toList $ fromListWith (+) $ map (\x -> (x, 1)) xs

formatOutput :: (String, Int) -> String
formatOutput (shell, n) = shell ++ ": " ++ show n
