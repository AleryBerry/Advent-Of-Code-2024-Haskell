import Control.Applicative (liftA2)
import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Control.Monad (join)
import Data.Bifunctor (second)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List (delete, find, minimumBy, nub, sortBy)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (isNothing, listToMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as S
import Debug.Trace (trace)
import GHC.Integer.GMP.Internals (sqrInteger)
import GHC.Num (integerSqr)

type Dijkstra = Set (Cost Int, Position, Maybe Direction)

data Direction = North | South | East | West
  deriving (Eq, Show, Ord)

data Cost a = Value a | Infinity
  deriving (Eq, Show)

instance Ord (Cost Int) where
  compare :: Cost Int -> Cost Int -> Ordering
  compare (Value a) (Value b) = a `compare` b
  compare Infinity Infinity = EQ
  compare Infinity _ = GT
  compare _ Infinity = LT

instance Num (Cost Int) where
  (+) :: Cost Int -> Cost Int -> Cost Int
  (Value a) + (Value b) = Value (a + b)
  Infinity + _ = Infinity
  _ + Infinity = Infinity
  fromInteger a = Value (fromInteger a)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  negate = fmap negate

instance Applicative Cost where
  (<*>) :: Cost (a -> b) -> Cost a -> Cost b
  (<*>) (Value f) (Value a) = Value (f a)
  (<*>) Infinity (Value a) = Infinity
  (<*>) (Value f) Infinity = Infinity
  pure :: a -> Cost a
  pure = Value

instance Monad Cost where
  (>>=) :: Cost a -> (a -> Cost b) -> Cost b
  (>>=) Infinity f = Infinity
  (>>=) (Value h) f = f h

instance Functor Cost where
  fmap :: (a -> b) -> Cost a -> Cost b
  fmap f Infinity = Infinity
  fmap f (Value a) = Value (f a)

type Position = (Int, Int)

type Score = Int

directionToPosition :: Direction -> Position
directionToPosition North = (0, -1)
directionToPosition South = (0, 1)
directionToPosition East = (1, 0)
directionToPosition West = (-1, 0)

positionToDirection :: Position -> Direction
positionToDirection (0, -1) = North
positionToDirection (0, 1) = South
positionToDirection (1, 0) = East
positionToDirection (-1, 0) = West

move :: Direction -> Position -> Position
move North (a, b) = (a, b - 1)
move South (a, b) = (a, b + 1)
move East (a, b) = (a + 1, b)
move West (a, b) = (a - 1, b)

neighbors :: Position -> [Position]
neighbors pos = (\a -> a pos) <$> [move North, move South, move East, move West]

dijkstra :: Dijkstra -> Position -> Set Position -> Dijkstra
dijkstra graph goal unvisited
  | null unvisited = graph
  | Just (cost, curr, Just prev) <- start =
      if curr == goal
        then graph
        else
          if S.null $ actualNeighbors curr
            then dijkstra graph goal (S.delete curr unvisited)
            else dijkstra (newGraph (updateCosts curr cost prev)) goal (S.delete curr unvisited)
  | otherwise = graph
  where
    start = S.lookupMin $ S.filter (\(_, a, _) -> a `S.member` unvisited) graph
    actualNeighbors x = S.filter (\(a, b, c) -> b `elem` neighbors x && b `elem` unvisited) graph
    updateCosts curr cost prev = S.map (calcu curr cost prev) (actualNeighbors curr)
    newGraph p = S.union p $ S.filter (\(a, b, c) -> b `S.notMember` S.map (\(a, b, c) -> b) p) graph
    difference :: Position -> Position -> Position
    difference (a, b) (c, d) = (a - c, b - d)
    calcu curr cost prev (a, b, p)
      | prev == positionToDirection (b `difference` curr) = (min a (cost + 1), b, Just prev)
      | otherwise = (min a (cost + 1001), b, if min a (cost + 1001) /= a then Just $ positionToDirection (b `difference` curr) else Just prev)

main :: IO ()
main = do
  file <- catch (readFile "input.txt") ((\_ -> putStrLn "Failed reading file." >> return "") :: IOException -> IO String)
  let rows = length $ lines file
      columns = length $ head $ lines file
      graph = filter (\(_, c) -> c /= '#') $ zipWith (\a b -> ((b `mod` columns, b `div` columns), a)) (concat $ lines file) [0 ..]
      [(start, _), (end, _)] = sortBy (\(_, c) (_, d) -> d `compare` c) $ filter (\(_, c) -> c == 'S' || c == 'E') graph
      unvisited = S.fromAscList $ filter (/= start) (fst <$> graph)
      unvisitedValues = S.map (Infinity :: Cost Int,,Nothing) unvisited
      posSet = S.fromList (fst <$> graph)
      dijks = dijkstra (S.insert (Value 0, start, Just East) unvisitedValues) end posSet
  print $ (\(a, b, c) -> a) <$> find (\(_, x, _) -> x == end) dijks
