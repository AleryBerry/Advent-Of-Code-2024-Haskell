{-# LANGUAGE BangPatterns #-}

import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Control.Monad (join)
import Data.List (groupBy, sortOn, transpose)
import Data.Map qualified as M
import Data.Maybe (isNothing)
import Debug.Trace (trace)

data Entity = Box | Robot | Wall
  deriving (Show, Eq, Ord)

type Position = (Int, Int)

type State = (M.Map Position (Maybe Entity), Position)

charToEntity :: Char -> Maybe Entity
charToEntity '#' = Just Wall
charToEntity '@' = Just Robot
charToEntity 'O' = Just Box
charToEntity _ = Nothing

move :: Char -> Position -> Position
move '<' (x, y) = (x - 1, y)
move '>' (a, b) = (a + 1, b)
move '^' (a, b) = (a, b - 1)
move 'v' (a, b) = (a, b + 1)
move _ p = p

printState :: State -> [String]
printState (a, p) = transpose $ ((\(a, b) -> convert b) <$>) <$> groupBy (\((x1, y1), b) ((x2, y2), d) -> y1 /= y2) (M.toList a)
  where
    convert (Just Wall) = '#'
    convert (Just Robot) = '@'
    convert (Just Box) = 'O'
    convert Nothing = '.'

nextState :: State -> Char -> State
nextState (state, currPos) direction
  | Nothing <- checkPos = (moveRobot state, nextMove)
  | Just Wall <- checkPos = (state, currPos)
  | Just Box <- checkPos = if test /= state then (moveRobot test, nextMove) else (state, currPos)
  | _ <- checkPos = (state, currPos)
  where
    (test, _) = nextState (state, nextMove) direction
    object = join $ M.lookup currPos state
    moveRobot = M.insert currPos Nothing . M.insert nextMove object
    checkPos = join $ M.lookup nextMove state
    nextMove = move direction currPos

main :: IO ()
main = do
  file <- catch (readFile "input.txt") ((\_ -> putStrLn "Failed reading file." >> return "") :: IOException -> IO String)
  let movements = concat $ tail $ dropWhile (/= "") $ lines file
      area = takeWhile (/= "") $ lines file
      columns = length $ head area
      rows = length area
      getChar x = area !! (x `div` rows) !! (x `mod` columns)
      state = M.fromList $ [((x `mod` columns, x `div` columns), charToEntity (getChar x)) | x <- [0 .. (length (concat area) - 1)]]
      robot = fst $ head $ filter (\(a, b) -> b == Just Robot) $ M.toList state
      result = foldl nextState (state, robot) movements
  let test = sum $ (\(a, b) -> a + 100 * b) . fst <$> filter (\(a, b) -> b == Just Box) (M.toList (fst result))
  print test
  mapM_ print $ printState result

-- foldl1 (>>) $ mapM_ print . printState . nextState (state, robot) <$> (movements)
