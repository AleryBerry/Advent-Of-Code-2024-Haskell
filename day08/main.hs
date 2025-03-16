import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Data.Set qualified as S
import Debug.Trace (trace)

type Grid = [((Int, Int), Char)]

result1 :: (Int, Int) -> Grid -> [(Int, Int)]
result1 (m, n) grid = result
  where
    result =
      [ (x, y)
        | (c1@(x1, y1), ch1) <- grid,
          (c2@(x2, y2), ch2) <- grid,
          ch1 == ch2,
          c1 /= c2,
          let x = 2 * x1 - x2,
          let y = 2 * y1 - y2
      ]

result2 :: (Int, Int) -> Grid -> [(Int, Int)]
result2 (m, n) grid = concat result
  where
    result =
      [ zip x y
        | (c1@(x1, y1), ch1) <- grid,
          (c2@(x2, y2), ch2) <- grid,
          ch1 == ch2,
          c1 /= c2,
          let x = takeWhile (\x -> x < n && x >= 0) [x1, 2 * x1 - x2 ..],
          let y = takeWhile (\y -> y < m && y >= 0) [y1, 2 * y1 - y2 ..]
      ]

main :: IO ()
main = do
  file <- catch (readFile "input.txt") ((\_ -> putStrLn "Failed reading file." >> return "") :: IOException -> IO String)
  let m = length $ lines file
  let n = length $ head $ lines file
  let grid = [((x, y), antenna) | (y, line) <- zip [0 ..] (lines file), (x, antenna) <- zip [0 ..] line, antenna /= '.']
  let getResult = S.fromList . filter (\(x, y) -> x >= 0 && y >= 0 && x < n && y < m)
  print $ length $ getResult $ result2 (m, n) grid
