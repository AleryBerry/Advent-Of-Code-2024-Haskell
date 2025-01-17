import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Data.List (groupBy, isPrefixOf)
import Data.Map (Map)
import Data.Map qualified as M

solve :: [String] -> [String] -> [(String, Int)]
solve [] towels = []
solve (x : xs) towels = (x, a) : solve xs towels
  where
    (a, b) = isDesignPossible (0, M.empty) x towels

isDesignPossible :: (Int, Map String Int) -> String -> [String] -> (Int, Map String Int)
isDesignPossible (result, cache) "" _ = (result + 1, cache)
isDesignPossible (result, cache) design towels
  | (Just e) <- design `M.lookup` cache = (result + e, cache)
  | (not . null) prefixes = (c + result, M.insert design c d)
  | otherwise = (result, cache)
  where
    (c, d) = foldl (\(g, acc) a -> isDesignPossible (g, acc) (afterX a) towels) (0, cache) prefixes
    afterX prefix = drop (length prefix) design
    prefixes = filter (`isPrefixOf` design) towels

main :: IO ()
main = do
  file <- catch (readFile "input.txt") ((\_ -> putStrLn "Failed reading file." >> return "") :: IOException -> IO String)
  let towels = filter (\a -> a /= ' ' && a /= ',') <$> groupBy (\a b -> b /= ',') (head (lines file))
      designs = drop 2 $ lines file
  print $ length $ filter (\(k, a) -> k `elem` designs && a > 0) $ solve designs towels
  print $ sum $ map snd $ filter (\(k, a) -> k `elem` designs && a > 0) $ solve designs towels
