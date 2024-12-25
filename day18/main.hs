import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Data.List (find, groupBy, insertBy, transpose)
import Data.Set (Set)
import Data.Set qualified as S
import Debug.Trace (trace)
import GHC.Float (float2Int, int2Float)

type Position = (Int, Int)

data Direction = North | South | East | West
  deriving (Eq, Ord)

data Node = Node {fCost :: Int, hCost :: Int, coords :: Position, prev :: Position}
  deriving (Eq)

instance Show Node where
  show (Node f h c p) = "{ " ++ show c ++ ", " ++ show (f + h) ++ ", " ++ show p ++ " }"

instance Ord Node where
  compare :: Node -> Node -> Ordering
  (Node f h c _) `compare` (Node f2 h2 c2 _)
    | c == c2 = EQ
    | (f + h) == (f2 + h2) = h `compare` h2
    | otherwise = (f + h) `compare` (f2 + h2)

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

mag :: Position -> Int
mag (x, y) = float2Int $ 10000 * sqrt (int2Float ((x * x) + (y * y)))

distance :: Position -> Position -> Int
distance (x, y) (x2, y2) = mag (x - x2, y - y2)

aStar :: Position -> Position -> Set Node -> Set Position -> Maybe (Set Node)
aStar start goal costs unvisited
  | S.null costs = aStar start goal (S.insert (Node {prev = (0, 0), fCost = start `distance` start, hCost = start `distance` goal, coords = start}) costs) unvisited
  | S.null uv = Nothing
  | coords chooseNode == goal = Just costs
  | otherwise = aStar start goal newCosts (S.delete (coords chooseNode) unvisited)
  where
    chooseNode = S.findMin uv
    uv = S.filter (\a -> coords a `S.member` unvisited) costs
    newCosts =
      S.union
        ( S.singleton
            Node
              { fCost = coords chooseNode `distance` start,
                hCost = coords chooseNode `distance` goal,
                coords = coords chooseNode,
                prev = prev chooseNode
              }
        )
        $ S.union
          neiCosts
          costs
    neiCosts = S.map (\a -> Node {fCost = (a `distance` coords chooseNode) + fCost chooseNode, hCost = a `distance` goal, coords = a, prev = coords chooseNode}) nei
    nei = S.filter (`elem` neighbors (coords chooseNode)) unvisited

size :: Int
size = 70

walk :: Set Node -> Maybe Node -> [Position]
walk e Nothing = []
walk e (Just a@(Node _ _ c p)) = p : walk (S.delete a e) (find (\a -> coords a == p) e)

createWorld :: (Ord a) => Int -> Set a -> [a] -> Set a
createWorld amount grid obstacles = S.difference grid (S.fromList $ take amount obstacles)

searchByte :: Int -> Int -> Set Position -> [Position] -> Int
searchByte inner outer grid obstacles =
  if abs (inner - outer) <= 1
    then amount
    else case generate (createWorld amount grid obstacles) of
      Nothing -> searchByte inner amount grid obstacles
      _ -> searchByte amount outer grid obstacles
  where
    amount = (inner + outer) `div` 2

generate :: Set Position -> Maybe (Set Node)
generate = aStar (0, 0) (size, size) S.empty

main :: IO ()
main = do
  file <- catch (readFile "input.txt") ((\_ -> putStrLn "Failed reading file." >> return "") :: IOException -> IO String)
  let obstacles = (\a -> ((read :: String -> Int) (takeWhile (/= ',') a), (read :: String -> Int) (tail $ dropWhile (/= ',') a))) <$> lines file
  let grid = S.fromList [(x, y) | x <- [0 .. size], y <- [0 .. size]]
  let str = transpose $ (\x -> (\a -> if a `elem` obstacles then '#' else '.') <$> x) <$> groupBy (\(a, b) (c, d) -> b /= d) (S.toList grid)
  foldl1 (>>) $ print <$> str
  let byte = searchByte 1024 (length obstacles) grid obstacles
  let world = generate $ createWorld (byte) grid obstacles
  print $ obstacles !! (byte)
  print $ result world
  where
    result :: Maybe (Set Node) -> Maybe Int
    result err = (\x -> length $ walk x (find (\a -> coords a == (size, size)) x)) <$> err
