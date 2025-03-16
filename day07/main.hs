import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Control.Monad (replicateM)
import Data.List (permutations)
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (Sum))
import GHC.Settings.Utils (maybeRead)

split :: (Char -> Bool) -> String -> [String]
split p s = case dropWhile p s of
  "" -> []
  s' -> w : split p s''
    where
      (w, s'') = break p s'

toInt :: String -> Int
toInt = fromMaybe 0 . (maybeRead :: String -> Maybe Int)

checkEquations :: (Int, [Int]) -> Maybe Int
checkEquations (result, numbers)
  | any ((==) result . operateEquation) equations = Just result
  | otherwise = Nothing
  where
    operations = replicateM (length numbers) [(+), (*), \a b -> toInt $ show b ++ show a]
    equations = map (zipWith (\a b -> b a) (drop 1 numbers)) operations
    operateEquation = foldl (\acc x -> x acc) (head numbers)

main :: IO ()
main = do
  file <- catch (readFile "input.txt") ((\_ -> putStrLn "Failed reading file." >> return "") :: IOException -> IO String)
  let numbers :: [(Int, [Int])] = map ((\x -> (toInt $ head x, (map toInt . words . (!! 1)) x)) . split (== ':')) $ lines file
  print $ foldMap (maybe mempty Sum . checkEquations) numbers