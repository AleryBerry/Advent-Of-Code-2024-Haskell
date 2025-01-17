import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Data.Bits (xor)
import Data.Foldable (foldl', maximumBy)
import Data.Function (on)
import Data.List (elemIndex, findIndex)
import Data.Map (Map)
import Data.Map qualified as M

type Sequences = Map [Int] Int

secretNumber :: Int -> Int
secretNumber num = calc num
  where
    calc = op3 . op2 . op1
    op1 num = prune $ num `xor` (num * 64)
    op2 num = prune $ num `xor` (num `div` 32)
    op3 num = prune $ num `xor` (num * 2048)
    prune num = num `mod` 16777216

lastDigit :: Int -> Int
lastDigit = (\a -> read [a]) . last . show

differences :: [Int] -> [Int]
differences [_] = []
differences (x : y : xs) = (y - x) : differences (y : xs)

sequences :: [Int] -> Sequences
sequences x = foldl' (\acc (idx, x) -> M.insertWith (\a b -> b) (seq idx) x acc) M.empty (zip [0 ..] validPrices)
  where
    seq idx = take 4 . drop idx $ diffs
    diffs = differences x
    validPrices = drop 4 x

main :: IO ()
main = do
  file <- catch (readFile "input.txt") ((\_ -> putStrLn "Failed reading file." >> return "") :: IOException -> IO String)
  let operations = iterate secretNumber . (read :: String -> Int) <$> words file
      secrets = take 2001 <$> operations
      prices = (lastDigit <$>) <$> secrets
      seqs = sequences <$> prices
  print $ sum $ last <$> secrets
  print $ maximumBy (\(_, a) (_, b) -> a `compare` b) $ M.toList $ M.unionsWith (+) seqs
