import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Data.Bifunctor (Bifunctor (bimap), second)
import Data.List (delete, isPrefixOf)
import Data.Map (Map)
import Data.Map qualified as M

type Wires = Map String Bool

data Gate = OR | XOR | AND
  deriving (Show, Eq)

data Connection = Connection String Gate String String
  deriving (Show, Eq)

handleFirstInput :: [String] -> Map [Char] Bool
handleFirstInput = M.fromList . ((second (strToBool . drop 2) . break (== ':')) <$>)
  where
    strToBool :: String -> Bool
    strToBool "0" = False
    strToBool _ = True

handleSecondInput :: [String] -> [Connection]
handleSecondInput = (listToConnection . words <$>) . drop 1
  where
    listToConnection [x, op, y, _, z] = Connection x (strToGate op) y z
    strToGate "XOR" = XOR
    strToGate "OR" = OR
    strToGate "AND" = AND

solution :: Wires -> [Connection] -> Wires
solution wires connections
  | (not . null) currentConnections,
    (x, y) <- handleConnection (head currentConnections) =
      solution (M.insert x y wires) (delete (head currentConnections) connections)
  | otherwise = wires
  where
    currentConnections = filter (\(Connection a b c _) -> a `M.member` wires && c `M.member` wires) connections
    handleConnection (Connection a b c d) = (d, gateToFunc b (wires M.! a) (wires M.! c))
    gateToFunc :: Gate -> Bool -> Bool -> Bool
    gateToFunc AND a b = a && b
    gateToFunc OR a b = a || b
    gateToFunc XOR a b = a /= b

getResult :: Wires -> Int
getResult = foldr (\x y -> fromEnum x + 2 * y) 0 . M.elems . M.filterWithKey (\a _ -> "z" `isPrefixOf` a)

main :: IO ()
main = do
  file <- catch (readFile "input.txt") ((\_ -> putStrLn "Failed reading file." >> return "") :: IOException -> IO String)
  let (x, y) = bimap handleFirstInput handleSecondInput . break null . lines $ file
  print $ getResult $ solution x y
