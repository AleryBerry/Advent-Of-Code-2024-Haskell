import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Data.Bifunctor (first, second)
import Data.Map (Map)
import Data.Map qualified as M
import Debug.Trace (trace)

type Lock = (Int, Int, Int, Int, Int)

handleInput :: [String] -> ([Lock], [Lock])
handleInput [] = ([], [])
handleInput ("" : xs) = handleInput xs
handleInput locks = f (getLock :) (handleInput $ drop 7 locks)
  where
    f = if isKey then first else second
    getLock = foldl (\(a, b, c, d, e) x -> (a + checkValue (head x), b + checkValue (x !! 1), c + checkValue (x !! 2), d + checkValue (x !! 3), e + checkValue (x !! 4))) (0, 0, 0, 0, 0) lockInput
    isKey = "....." == head locks
    lockInput = take 7 locks
    checkValue '#' = 1
    checkValue '.' = 0

doesItFit :: Lock -> Lock -> Int -> Bool
doesItFit (a, b, c, d, e) (a2, b2, c2, d2, e2) num = a + a2 < num && b + b2 < num && c + c2 < num && d + d2 < num && e + e2 < num

main :: IO ()
main = do
  file <- catch (readFile "input.txt") ((\_ -> putStrLn "Failed reading file." >> return "") :: IOException -> IO String)
  let (a, b) = handleInput $ lines file
  print $ length [(x, y) | x <- a, y <- b, doesItFit x y 8]
