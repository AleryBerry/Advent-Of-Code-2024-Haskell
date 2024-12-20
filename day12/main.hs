import Control.Arrow
import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Control.Monad (replicateM)
import Data.Bifunctor (bimap)
import Data.Bifunctor qualified
import Data.List (delete, permutations)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Debug.Trace (trace)

type Position = (Int, Int)

checkAll :: [Position] -> [Position] -> Maybe Position
checkAll [] y = Nothing
checkAll (x : xs) y = if any (check x) y then Just x else checkAll xs y
  where
    check (m, n) (a, b) = (abs (a - m) == 1 && (b - n) == 0) || (abs (b - n) == 1 && (a - m) == 0)

split' :: [Position] -> [Position] -> [Position]
split' n [] = n
split' n xs
  | result /= (-1, -1) = split' (result : n) (delete result xs)
  | otherwise = split' n (tail xs)
  where
    result = fromMaybe (-1, -1) $ checkAll xs n

split :: Position -> [Position] -> [[Position]]
split p@(a, b) [] = []
split p@(a, b) ps@(x : xs)
  | null yay = [re]
  | otherwise = re : split (head yay) yay
  where
    yay = S.toList $ S.difference (S.fromList ps) (S.fromList re)
    re = split' [head ps] (tail ps)

getHeads :: [Position] -> Int
getHeads [] = 0
getHeads xs = sum hasSides
  where
    hasSides = (\x -> 4 - length (filter id $ (`elem` xs) <$> x)) <$> checks
    checks = (\(a, b) -> bimap (a +) (b +) <$> ones) <$> xs
    ones = (head &&& last) <$> permutations [0, 1] ++ permutations [-1, 0]

numOfSides :: S.Set Position -> Int
numOfSides region = sum $ numCorners <$> S.toList region
  where
    numCorners (y, x) =
      length $
        filter
          ( \(adj1, adj2, corner) ->
              all (`S.notMember` region) [adj1, adj2] || all (`S.member` region) [adj1, adj2] && (corner `S.notMember` region)
          )
          touching8Neighbors
      where
        touching8Neighbors = [((y + dy, x), (y, x + dx), (y + dy, x + dx)) | dy <- [-1, 1], dx <- [-1, 1]]

main :: IO ()
main = do
  file <- catch (readFile "input.txt") ((\_ -> putStrLn "Failed reading file." >> return "") :: IOException -> IO String)
  let map = lines file
  let trailheads = M.fromListWith S.union $ zipWith (\a b -> (a, S.singleton (b `mod` length map, b `div` (length . head $ map)))) (concat map) [0 ..]
  let stuff = S.toList <$> M.elems trailheads
  let splittedList = (\a -> split (head a) a) <$> stuff
  let keys = M.fromList $ zip (M.keys trailheads) splittedList
  print $ sum $ M.elems $ M.map (\x -> sum $ (\x2 -> length x2 * numOfSides (S.fromList x2)) <$> x) keys
