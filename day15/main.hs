import Control.Concurrent.STM (check)
import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Control.Monad (join)
import Data.List (groupBy, sortOn, transpose)
import Data.Map qualified as M
import Data.Maybe (isNothing)
import Debug.Trace (trace)

data Entity = Box | Robot | Wall | BigBoxL | BigBoxR
  deriving (Show, Eq, Ord)

type Position = (Int, Int)

type State = (M.Map Position (Maybe Entity), Position)

charToEntity :: Char -> Maybe Entity
charToEntity '#' = Just Wall
charToEntity '@' = Just Robot
charToEntity 'O' = Just Box
charToEntity '[' = Just BigBoxL
charToEntity ']' = Just BigBoxR
charToEntity _ = Nothing

parse :: [String] -> M.Map Position (Maybe Entity)
parse file = M.fromList $ zipWith (\a b -> ((b `mod` (length . head $ file), b `div` (length . head $ file)), charToEntity a)) (concat file) [0 ..]

move :: Char -> Position -> Position
move '<' (x, y) = (x - 1, y)
move '>' (a, b) = (a + 1, b)
move '^' (a, b) = (a, b - 1)
move 'v' (a, b) = (a, b + 1)
move _ p = p

printState (a, p) = foldl1 (>>) $ print <$> transpose (((\(a, b) -> convert b) <$>) <$> groupBy (\((x1, y1), b) ((x2, y2), d) -> y1 /= y2) (M.toList a))
  where
    convert (Just Wall) = '#'
    convert (Just Robot) = '@'
    convert (Just Box) = 'O'
    convert (Just BigBoxL) = '['
    convert (Just BigBoxR) = ']'
    convert Nothing = '.'

canMoveBigBox :: State -> Char -> Bool
canMoveBigBox state@(positions, currPos) direction
  | object `notElem` [Just BigBoxL, Just BigBoxR] = False
  | (Nothing, Nothing) <- checkPos = True
  | (Nothing, Just _) <- checkPos = canMoveBigBox (positions, nextMove siblingPos) direction
  | (Just _, Nothing) <- checkPos = canMoveBigBox (positions, nextMove currPos) direction
  | (_, Just Wall) <- checkPos = False
  | (Just Wall, _) <- checkPos = False
  | otherwise =
      canMoveBigBox (positions, nextMove currPos) direction
        && canMoveBigBox (positions, nextMove siblingPos) direction
  where
    checkPos = (join $ M.lookup (nextMove currPos) positions, join $ M.lookup (nextMove siblingPos) positions)
    sibling = join $ M.lookup siblingPos positions
    siblingPos
      | Just BigBoxL <- object = move '>' currPos
      | Just BigBoxR <- object = move '<' currPos
    object = join $ M.lookup currPos positions
    nextMove = move direction

nextState :: State -> Char -> State
nextState state@(positions, currPos) direction
  | Nothing <- checkPos = (moveObject (object, currPos) positions, nextMove currPos)
  | Just Wall <- checkPos = state
  | movingBigBox, boxPositions /= positions, (x, y) <- nextState (boxPositions, siblingPos) direction = (moveObject (object, currPos) x, nextMove currPos)
  | checkPos `elem` [Just BigBoxL, Just BigBoxR] && movingVertical && not movingBigBox = state
  | Just _ <- checkPos, boxPositions /= positions = (moveObject (object, currPos) boxPositions, nextMove currPos)
  | otherwise = state
  where
    siblingPos
      | Just BigBoxL <- checkPos = move '>' $ nextMove currPos
      | Just BigBoxR <- checkPos = move '<' $ nextMove currPos
      | _ <- checkPos = currPos
    movingBigBox = movingVertical && canMoveBigBox (positions, nextMove currPos) direction
    movingVertical = direction == 'v' || direction == '^'
    (boxPositions, _) = nextState (positions, nextMove currPos) direction
    object = join $ M.lookup currPos positions
    checkPos = join $ M.lookup (nextMove currPos) positions
    moveObject (obj, pos) = M.insert pos Nothing . M.insert (nextMove pos) obj
    nextMove = move direction

transform :: [String] -> [String]
transform str = concatMap transform <$> str
  where
    transform '#' = "##"
    transform 'O' = "[]"
    transform '@' = "@."
    transform '.' = ".."

main :: IO ()
main = do
  file <- catch (readFile "input.txt") ((\_ -> putStrLn "Failed reading file." >> return "") :: IOException -> IO String)

  let state = parse $ takeWhile (/= "") $ lines file
      movements = concat $ tail $ dropWhile (/= "") $ lines file
      robot = fst $ head $ filter (\(a, b) -> b == Just Robot) $ M.toList state
      result = foldl nextState (state, robot) movements
  let test = sum $ (\(a, b) -> a + 100 * b) . fst <$> filter (\(a, b) -> b == Just Box) (M.toList (fst result))

  let part2 = parse $ transform $ takeWhile (/= "") $ lines file
      robot2 = fst $ head $ filter (\(a, b) -> b == Just Robot) $ M.toList part2
  let as = scanl nextState (part2, robot2) movements
  let aee = printState <$> as
  print $ sum $ (\(a, b) -> a + 100 * b) . fst <$> filter (\(a, b) -> b == Just BigBoxL) (M.toList (fst $ last as))

-- mapM_ print $ printState result
