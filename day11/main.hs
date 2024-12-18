import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Data.IntMap (fromList)
import Data.IntMap as IntMap (IntMap, assocs, fromList, fromListWith)
import Data.Maybe (fromMaybe)
import GHC.Settings.Utils (maybeRead)

blinks :: IntMap Int -> IntMap Int
blinks numbers = IntMap.fromListWith (+) [(number', n) | (number, n) <- IntMap.assocs numbers, number' <- blink number]

blink :: Int -> [Int]
blink 0 = [1]
blink number
  | even len = [read $ take (len `div` 2) str, read $ drop (len `div` 2) str]
  | otherwise = [number * 2024]
  where
    str = show number
    len = length str

main :: IO ()
main = do
  file <- catch (readFile "input.txt") ((\_ -> putStrLn "Failed reading file." >> return "") :: IOException -> IO String)
  let numbers = fromMaybe 0 . (maybeRead :: String -> Maybe Int) <$> words file
  print $ sum . last . take 76 . iterate blinks $ IntMap.fromList [(i, 1) | i <- numbers]
