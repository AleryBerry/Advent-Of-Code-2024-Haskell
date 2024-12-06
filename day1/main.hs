import Control.Exception (catch)
import Control.Exception.Base (IOException)
import Data.Bifunctor (bimap)
import Data.List (sort)
import GHC.Settings.Utils (maybeRead)

split :: [Int] -> ([Int], [Int])
split [] = ([], [])
split [x] = ([], [])
split (x : y : xs) = bimap (x :) (y :) (split xs)

numberScore :: Int -> [Int] -> Int
numberScore x [] = 0
numberScore x (y : ys)
  | x == y = x + numberScore x ys
  | otherwise = 0 + numberScore x ys

similarityScore :: ([Int], [Int]) -> Int
similarityScore (x, y) = sum $ map (`numberScore` y) x

main :: IO ()
main = do
  file <- catch (readFile "input.txt") ((\_ -> putStrLn "Failed reading file." >> return "") :: IOException -> IO String)
  let numbers = mapM maybeRead (words file)
  let splittedNumbers = split <$> numbers
  let sortedSplit = bimap sort sort <$> splittedNumbers
  print $ sum . map abs . uncurry (zipWith (-)) <$> sortedSplit
  print $ similarityScore <$> sortedSplit
