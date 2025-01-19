import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Control.Monad (join)
import Data.Function (on)
import Data.List (intercalate, intersect, maximumBy, nub, union)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Debug.Trace (trace)

type Connections = Map String (Set String)

data TConnection = TConnection String String String

instance Show TConnection where
  show :: TConnection -> String
  show (TConnection a b c) = show (a, b, c)

instance Eq TConnection where
  (==) :: TConnection -> TConnection -> Bool
  (==) (TConnection a b c) (TConnection d e f) =
    (a == d && b == e && c == f)
      || (a == d && b == f && c == e)
      || (a == e && b == f && c == d)
      || (a == e && b == d && c == f)
      || (a == f && b == d && c == e)
      || (a == f && b == e && c == d)

instance Ord TConnection where
  compare :: TConnection -> TConnection -> Ordering
  compare x@(TConnection a b c) y@(TConnection d e f)
    | x == y = EQ
    | otherwise = (a, b, c) `compare` (d, e, f)

type Connection = (String, String)

findTConnections :: Connections -> [String] -> [TConnection]
findTConnections connections usedKeys
  | (not . null) nextKeys = findTConnections' (head nextKeys) ++ findTConnections connections (head nextKeys : usedKeys)
  | otherwise = []
  where
    nextKeys = filter (`notElem` usedKeys) (M.keys connections)
    findTConnections' key =
      concatMap
        (filter startsWithT)
        ( S.map
            (\key2 -> S.toList $ S.map (TConnection key key2) $ S.filter (`S.member` keyElems) (connections M.! key2))
            (S.filter (`notElem` usedKeys) keyElems)
        )
      where
        keyElems = connections M.! key

getConnections :: [Connection] -> Connections
getConnections [] = M.empty
getConnections ((a, b) : xs) = insert b a . insert a b $ getConnections xs
  where
    insert x y = M.insertWith S.union x (S.singleton y)

findMaxCliques :: Connections -> [Set String]
findMaxCliques connections = go S.empty (M.keysSet connections) S.empty
  where
    go :: Set String -> Set String -> Set String -> [Set String]
    go clique candidates forbidden =
      case S.minView candidates of
        Nothing -> [clique | S.size forbidden == 0]
        Just (vertex, remaining) ->
          go
            (S.insert vertex clique)
            (S.intersection candidates $ connections M.! vertex)
            (S.intersection forbidden $ connections M.! vertex)
            ++ go clique remaining (S.insert vertex forbidden)

startsWithT :: TConnection -> Bool
startsWithT (TConnection a b c) = head a == 't' || head b == 't' || head c == 't'

main :: IO ()
main = do
  file <- catch (readFile "input.txt") ((\_ -> putStrLn "Failed reading file." >> return "") :: IOException -> IO String)
  let connections = getConnections $ (\a -> (take 2 a, drop 3 a)) <$> words file
  print $ length $ nub $ findTConnections connections []
  print $ intercalate "," $ S.toAscList $ maximumBy (compare `on` S.size) $ findMaxCliques connections
