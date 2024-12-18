import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Data.List (nub, permutations)
import Data.Maybe (fromMaybe, isNothing)
import Data.Set qualified as S
import GHC.Settings.Utils (maybeRead)

type Position = (Int, Int)

type State = (S.Set Position, [Position])

type Limits = (Int, Int)

type TopoMap = [[Int]]

getLevel :: Position -> Limits -> TopoMap -> Maybe Int
getLevel p@(x, y) l@(a, b) area
  | x >= a || x < 0 || y >= b || y < 0 = Nothing
  | otherwise = Just $ area !! y !! x

findPath :: [Position] -> TopoMap -> Maybe State
findPath _ [] = Nothing
findPath f@(p@(x, y) : xs) area
  | getLevel p l area == Just 9 = Just (S.fromList f, [p])
  | isNothing $ getLevel p l area = Nothing
  | (not . null) paths = Just $ foldl1 (\(a, m) (c, n) -> (S.union a c, m ++ n)) paths
  | otherwise = Just (S.fromList f, [])
  where
    l@(lx, ly) = (length area, length . head $ area)
    paths = filter (\(m, e) -> (not . null) e) $ fromMaybe (S.empty, []) . nextSearch <$> ones
    nextSearch (a, b) = if nextLevel == (currentLevel + 1) then findPath ((x + a, y + b) : f) area else Nothing
      where
        nextLevel = fromMaybe 0 $ getLevel (x + a, y + b) l area
    currentLevel = fromMaybe 0 $ getLevel p l area
    ones :: [(Int, Int)] = (\x -> (head x, last x)) <$> permutations [0, 1] ++ permutations [-1, 0]
findPath x area = Just (S.fromList x, [])

main :: IO ()
main = do
  file <- catch (readFile "input.txt") ((\_ -> putStrLn "Failed reading file." >> return "") :: IOException -> IO String)
  let topoMap = (\x -> (\y -> fromMaybe (-1) $ (maybeRead :: String -> Maybe Int) . (: []) $ y) <$> x) <$> lines file
  let trailheads = map (\(a, b) -> (b `mod` length topoMap, b `div` (length . head $ topoMap))) $ filter (\(a, b) -> a == 0) $ zip (concat topoMap) [0 ..]
  let result = mapM (\a -> snd <$> findPath [a] topoMap) trailheads
  print $ (sum . (length <$>)) . (nub <$>) <$> result
  print $ sum . (length <$>) <$> result