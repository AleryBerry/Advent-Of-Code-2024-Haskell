import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Data.List (sort, sortBy)
import Data.Ord (Down (Down), comparing)
import GHC.Settings.Utils (maybeRead)

isNotDistanced :: [Int] -> Bool
isNotDistanced [x] = True
isNotDistanced (x : y : xs) = distance <= 3 && distance > 0 && isNotDistanced (y : xs)
  where
    distance = abs (x - y)

isListOrdered :: [Int] -> Bool
isListOrdered x = x == sort x || x == sortBy (comparing Down) x

removeLevels :: Int -> [Int] -> [Bool]
removeLevels x y =
  if x >= length y
    then []
    else
      (isListOrdered list && isNotDistanced list) : removeLevels (x + 1) y
  where
    list = take x y ++ drop (x + 1) y

countTruth :: [Bool] -> Int
countTruth [] = 0
countTruth (x : xs) = if x then 1 + countTruth xs else 0 + countTruth xs

main :: IO ()
main = do
  file <- catch (readFile "input.txt") ((\_ -> putStrLn "Failed reading file." >> return "") :: IOException -> IO String)
  let reports = mapM (mapM (maybeRead :: String -> Maybe Int)) <$> map words . lines $ file
  let valuedLevels = map (\x -> isListOrdered x && isNotDistanced x) <$> reports
  let valuedLevelsPart2 = fmap (fmap or) $ map (removeLevels 0) <$> reports
  print $ countTruth <$> valuedLevels
  print $ countTruth <$> valuedLevelsPart2
