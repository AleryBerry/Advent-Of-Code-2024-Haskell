import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Data.Bifunctor qualified
import Data.Foldable (fold)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Set qualified as Set

data State = State World VisitedPositions (Maybe GuardState)
  deriving (Show)

type World = Map.Map Position Square

type VisitedPositions = Set.Set GuardState

data GuardState = GuardState {pos :: Position, dir :: Direction}
  deriving (Eq, Show)

instance Ord GuardState where
  compare :: GuardState -> GuardState -> Ordering
  (GuardState pos dir) `compare` (GuardState pos2 dir2) = if pos == pos2 then dir `compare` dir2 else pos `compare` pos2

data Square = Empty | Guard Direction | Obstacle
  deriving (Eq, Show)

data Position = Position {x :: Int, y :: Int}
  deriving (Eq, Show)

(<+>) :: Position -> Position -> Position
(<+>) a b = Position {x = x a + x b, y = y a + y b}

instance Ord Position where
  compare :: Position -> Position -> Ordering
  (Position a b) `compare` (Position c d) = if b == d then a `compare` c else b `compare` d

data Direction = North | South | East | West
  deriving (Eq, Show, Ord)

readSquare :: Char -> Maybe Square
readSquare '.' = Just Empty
readSquare '^' = Just $ Guard North
readSquare '>' = Just $ Guard East
readSquare '<' = Just $ Guard West
readSquare 'v' = Just $ Guard South
readSquare '#' = Just Obstacle
readSquare _ = Nothing

readWorld :: [String] -> World
readWorld text = Map.mapMaybe id $ Map.fromList info
  where
    info = zipWith (\x a -> (Position {x = x `mod` width, y = x `div` width}, a)) [0 ..] $ map readSquare $ fold text
    width = length $ head text

readDirection :: Direction -> Position
readDirection North = Position {x = 0, y = -1}
readDirection South = Position {x = 0, y = 1}
readDirection West = Position {x = -1, y = 0}
readDirection East = Position {x = 1, y = 0}

turn90Degrees :: GuardState -> GuardState
turn90Degrees (GuardState pos North) = GuardState pos East
turn90Degrees (GuardState pos East) = GuardState pos South
turn90Degrees (GuardState pos South) = GuardState pos West
turn90Degrees (GuardState pos West) = GuardState pos North

searchGuard :: World -> Maybe GuardState
searchGuard world
  | (not . null) list = Just $ (\(a, b) -> GuardState {pos = a, dir = b}) $ head list
  | otherwise = Nothing
  where
    list = (\(a, b) -> (a, fromMaybe North (isGuard b))) <$> filter (\(a, b) -> isJust $ isGuard b) (Map.toList world)
    isGuard (Guard dir) = Just dir
    isGuard _ = Nothing

play :: State -> Maybe State
play (State _ _ Nothing) = Nothing
play (State world visitedPositions (Just guard))
  | guard `elem` visitedPositions = Nothing
  | isNothing (Map.lookup newPosition world) = Just $ State world (Set.insert guard visitedPositions) (Just guard)
  | isObstacle (Map.lookup newPosition world) = play $ State world (Set.insert guard visitedPositions) (Just $ turn90Degrees guard)
  | otherwise = play $ State newWorld (Set.insert guard visitedPositions) (Just $ GuardState {pos = newPosition, dir = dir guard})
  where
    newWorld = Map.insert newPosition (Guard (dir guard)) $ Map.insert (pos guard) Empty world
    direction = readDirection $ dir guard
    newPosition = pos guard <+> direction
    isObstacle (Just Obstacle) = True
    isObstacle _ = False

main :: IO ()
main = do
  file <- catch (readFile "input.txt") ((\_ -> putStrLn "Failed reading file." >> return "") :: IOException -> IO String)
  let world = readWorld $ lines file
  let guard = fromMaybe (GuardState {pos = Position {x = -1, y = -1}, dir = North}) $ searchGuard world
  let (State _ visitedPositions _) = fromMaybe (State world Set.empty (Just guard)) $ play $ State world Set.empty (Just guard)
  let positions = Set.map (\(GuardState pos dir) -> pos) visitedPositions
  let worldsToTest = Set.foldr (\pos acc -> State (Map.insert pos Obstacle world) Set.empty (Just guard) : acc) [] (Set.filter (\a -> a /= pos guard) positions)
  print $ length worldsToTest
  print $ length positions
  print $ length $ filter isNothing $ play <$> worldsToTest
