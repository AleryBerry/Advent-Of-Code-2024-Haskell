import Control.Applicative (liftA)
import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Control.Monad (liftM)
import Data.Char (isDigit)
import Data.List (inits, nub)
import Data.Map qualified as M
import Data.Set qualified as S
import Debug.Trace (trace)
import GHC.Settings.Utils (maybeRead)

type Prize = (Int, Int)

type Button = (Int, Int)

type Game = (Button, Button, Prize)

unwrapTuple :: (Maybe a, Maybe b, Maybe c) -> Maybe (a, b, c)
unwrapTuple (Just a, Just b, Just c) = Just (a, b, c)
unwrapTuple _ = Nothing

getPlays :: [String] -> [[String]]
getPlays xs
  | result == xs = [result]
  | otherwise = result : getPlays (drop (length result + 1) xs)
  where
    result = takeWhile (/= "") xs

getNumber :: [Char] -> Maybe Int
getNumber x = (maybeRead :: String -> Maybe Int) $ filter isDigit x

getButton :: String -> Maybe Button
getButton str = case p of
  (Just a, Just b) -> Just (a, b)
  _ -> Nothing
  where
    p@(x, y) = (getNumber $ head s, getNumber $ s !! 1)
    s = words $ tail $ dropWhile (/= ':') str

getPrize :: String -> Maybe Prize
getPrize str = case p of
  (Just a, Just b) -> Just (convert a, convert b)
  _ -> Nothing
  where
    convert x = (10000000000000 + x)
    p@(x, y) = (getNumber $ s !! 1, getNumber $ s !! 2)
    s = words str

isValid :: [String] -> Maybe Game
isValid x = unwrapTuple (getButton (head x), getButton (x !! 1), getPrize (x !! 2))

solve :: Game -> Int
solve ((ax, ay), (bx, by), (px, py))
  | (rm == 0) && (rn == 0) = 3 * n + m
  | otherwise = 0
  where
    (m, rm) = (px * ay - py * ax) `quotRem` (bx * ay - by * ax)
    (n, rn) = (px * ay - m * bx * ay) `quotRem` (ax * ay)

main :: IO ()
main = do
  file <- catch (readFile "input.txt") ((\_ -> putStrLn "Failed reading file." >> return "") :: IOException -> IO String)
  let wot = isValid <$> getPlays (lines file)
  print $ wot
  print $ sum <$> mapM (solve <$>) wot
