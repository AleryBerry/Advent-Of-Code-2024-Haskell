import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Control.Monad (when)
import Data.Bifunctor (bimap)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Maybe qualified as S
import Data.Set qualified as S
import Debug.Trace (trace)
import GHC.Settings.Utils (maybeRead)

type Velocity = (Int, Int)

type Position = (Int, Int)

type Robot = (Position, Velocity)

type State = [Robot]

type Boundaries = (Int, Int)

getVectors :: [String] -> Maybe Robot
getVectors str = case (position, velocity) of
  ((Just a, Just b), (Just c, Just d)) -> Just ((a, b), (c, d))
  _ -> Nothing
  where
    velocity = (toInt (getFirstNumber velStr), toInt (getSecondNumber velStr))
    position = (toInt (getFirstNumber posStr), toInt (getSecondNumber posStr))
    toInt = maybeRead :: String -> Maybe Int
    velStr = str !! 1
    posStr = head str
    getFirstNumber = tail . dropWhile (/= '=') . takeWhile (/= ',')
    getSecondNumber = tail . dropWhile (/= ',')

updateRobot :: Int -> Robot -> Robot
updateRobot n (p@(p1, p2), v@(v1, v2)) = ((p1 + v1 * n, p2 + v2 * n), v)

update :: Int -> State -> State
update n = map (updateRobot n)

getQuadrants :: Boundaries -> [Position] -> [Int]
getQuadrants (x, y) positions = length <$> [first, second, third, fourth]
  where
    first = filter (\(a, b) -> a < x `div` 2 && b < y `div` 2) positions
    second = filter (\(a, b) -> a > x `div` 2 && b < y `div` 2) positions
    third = filter (\(a, b) -> a < x `div` 2 && b > y `div` 2) positions
    fourth = filter (\(a, b) -> a > x `div` 2 && b > y `div` 2) positions

regions :: S.Set Position -> Int
regions s
  | Just c <- S.lookupMin s = let r = go c S.empty in 1 + regions (S.difference s r)
  | otherwise = 0
  where
    go :: Position -> S.Set Position -> S.Set Position
    go c@(x, y) r
      | c `S.member` r = r
      | otherwise =
          let r' = S.insert c r
              nexts = [n | n <- [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)], n `S.member` s]
           in foldr go r' nexts

part2 :: Maybe [Robot] -> Int -> IO ()
part2 robots n = do
  let moved = (boundaries <$>) . update n <$> robots
      s = maybe S.empty S.fromList moved
      cc = regions s
      boundaries = (\(a, b) -> bimap (mod a) (mod b) (101, 103)) . fst
  when (cc < 250) $ do
    putStrLn $ "After " ++ show n ++ " seconds, " ++ show cc ++ " CCs:"
    putStr "Part 2: " >> print n

main :: IO ()
main = do
  file <- catch (readFile "input.txt") ((\_ -> putStrLn "Failed reading file." >> return "") :: IOException -> IO String)
  let state = mapM (getVectors . words) (lines file)
  let limits = (101, 103)
  let boundaries = (\(a, b) -> bimap (mod a) (mod b) limits) . fst
  let newState = (boundaries <$>) . update 100 <$> state
  print $ product . getQuadrants limits <$> newState
  mapM_ (part2 state) [0 .. 10000]
