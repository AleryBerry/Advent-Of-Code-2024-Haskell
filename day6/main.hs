import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Data.Bifunctor qualified
import Data.Foldable (fold)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Set qualified as Set

type Space = Map.Map Position Square

data State = State Space (Set.Set Position) (Maybe (Position, Direction))
  deriving (Show)

data Square = Empty | Guard Direction | Obstacle
  deriving (Eq, Show)

data Position = Position {x :: Int, y :: Int}
  deriving (Eq, Show)

(<+>) :: Position -> Position -> Position
(<+>) a b = Position {x = x a + x b, y = y a + y b}

instance Ord Position where
  (Position a b) `compare` (Position c d) =
    if b == d
      then a `compare` c
      else b `compare` d

data Direction = Up | Down | Right | Left
  deriving (Eq, Show)

readDirection :: Direction -> Position
readDirection Main.Up = Position {x = 0, y = -1}
readDirection Main.Down = Position {x = 0, y = 1}
readDirection Main.Left = Position {x = -1, y = 0}
readDirection Main.Right = Position {x = 1, y = 0}

turn90Degrees :: Direction -> Direction
turn90Degrees Main.Up = Main.Right
turn90Degrees Main.Right = Main.Down
turn90Degrees Main.Down = Main.Left
turn90Degrees Main.Left = Main.Up

readSquare :: Char -> Maybe Square
readSquare '.' = Just Empty
readSquare '^' = Just $ Guard Main.Up
readSquare '>' = Just $ Guard Main.Right
readSquare '<' = Just $ Guard Main.Left
readSquare 'v' = Just $ Guard Main.Down
readSquare '#' = Just Obstacle
readSquare a = Nothing

readSpace :: [String] -> Space
readSpace text = Map.mapMaybe id $ Map.fromList info
  where
    info = zipWith (\x a -> (Position {x = x `mod` width, y = x `div` width}, a)) [0 ..] $ map readSquare $ fold text
    width = length $ head text

searchGuard :: Space -> Maybe (Position, Direction)
searchGuard space
  | (not . null) list = Just $ head list
  | otherwise = Nothing
  where
    list = (\(a, b) -> (a, fromMaybe Main.Up (isGuard b))) <$> filter (\(a, b) -> isJust $ isGuard b) (Map.toList space)
    isGuard (Guard dir) = Just dir
    isGuard _ = Nothing

moveGuard :: State -> Maybe State
moveGuard (State _ _ Nothing) = Nothing
moveGuard (State space prev (Just guard))
  | isNothing (Map.lookup newPosition space) = Just $ State space (Set.insert (fst guard) prev) (Just guard)
  | isObstacle (Map.lookup newPosition space) = moveGuard $ State space (Set.insert (fst guard) prev) (Just $ Data.Bifunctor.second turn90Degrees guard)
  | otherwise = moveGuard $ State newSpace (Set.insert (fst guard) prev) (Just (newPosition, snd guard))
  where
    newSpace = Map.insert newPosition (Guard (snd guard)) $ Map.insert (fst guard) Empty space
    direction = readDirection $ snd guard
    newPosition = fst guard <+> direction
    isObstacle (Just Obstacle) = True
    isObstacle _ = False

main :: IO ()
main = do
  file <- catch (readFile "input.txt") ((\_ -> putStrLn "Failed reading file." >> return "") :: IOException -> IO String)
  let space = readSpace $ lines file
  let (State e r y) = fromMaybe (State space Set.empty (searchGuard space)) $ moveGuard $ State space Set.empty (searchGuard space)
  print $ length r
