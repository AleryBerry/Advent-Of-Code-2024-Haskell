import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Data.List (findIndices, sortBy)

type Position = (Int, Int)

data Direction = North | South | East | West
  deriving (Eq, Ord, Show)

move :: Direction -> Position -> Position
move North (a, b) = (a, b - 1)
move South (a, b) = (a, b + 1)
move East (a, b) = (a + 1, b)
move West (a, b) = (a - 1, b)

neighbors :: Position -> [Position]
neighbors pos = (`move` pos) <$> [North, South, East, West]

walk :: [Position] -> [Position] -> [Position]
walk d path
  | (not . null) n = walk d (head n : path)
  | otherwise = reverse path
  where
    n = filter (\x -> x `elem` d && x `notElem` path) $ neighbors (head path)

cheats :: [Position] -> Int -> Int -> Int
cheats path idx amt
  | idx == length path = amt
  | otherwise = cheats path (idx + 1) $ amt + length indexes
  where
    (a, b) = path !! idx
    indexes = filter (\a -> distance (newPath !! a) - 2 /= a) $ findIndices (\x -> distance x == 2) newPath
    newPath = drop (idx + 100) path
    distance (x, y) = abs (a - x) + abs (b - y)

cheatsP2 :: [Position] -> Int -> Int -> Int
cheatsP2 path idx amt
  | idx == length path = amt
  | otherwise = cheatsP2 path (idx + 1) $ amt + length indexes
  where
    (a, b) = path !! idx
    indexes = filter (\a -> (a + idx) - (idx + distance (newPath !! a)) >= 0) $ findIndices (\x -> distance x <= 20) newPath
    newPath = drop (idx + 100) path
    distance (x, y) = abs (a - x) + abs (b - y)

main :: IO ()
main = do
  file <- catch (readFile "input.txt") ((\_ -> putStrLn "Failed reading file." >> return "") :: IOException -> IO String)
  let columns = length $ head $ lines file
      grid = zipWith (\a b -> ((b `mod` columns, b `div` columns), a)) (concat $ lines file) [0 ..]
      obstacles = fst <$> filter (\(_, b) -> b == '#') grid
      road = filter (\(_, b) -> b /= '#') grid
      [(start, _), (end, _)] = sortBy (\(_, a) (_, b) -> b `compare` a) $ filter (\(a, b) -> b == 'E' || b == 'S') road
      path = walk (fst <$> road) [start]
  print $ cheatsP2 path 0 0
