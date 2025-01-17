import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Data.List (foldl', nub)
import Data.Map qualified as M

type Coord = (Int, Int)

type Table = M.Map (Char, Char) [String]

type Memo = M.Map (Int, Char, Char) Int

charToCoord :: Char -> Maybe Coord
charToCoord '0' = Just (1, 3)
charToCoord 'A' = Just (2, 3)
charToCoord '1' = Just (0, 2)
charToCoord '2' = Just (1, 2)
charToCoord '3' = Just (2, 2)
charToCoord '4' = Just (0, 1)
charToCoord '5' = Just (1, 1)
charToCoord '6' = Just (2, 1)
charToCoord '7' = Just (0, 0)
charToCoord '8' = Just (1, 0)
charToCoord '9' = Just (2, 0)
charToCoord 'a' = Just (2, 0)
charToCoord '^' = Just (1, 0)
charToCoord '<' = Just (0, 1)
charToCoord 'v' = Just (1, 1)
charToCoord '>' = Just (2, 1)
charToCoord _ = Nothing

seqc :: Table -> Int -> String -> Int
seqc table times code = fst $ seqc' M.empty (times + 1) 'A' code
  where
    seqc' :: Memo -> Int -> Char -> String -> (Int, Memo)
    seqc' memo 0 _ code = (length code, memo)
    seqc' memo times start code = foldl' loop (0, memo) $ zip (start : code) code
      where
        loop :: (Int, Memo) -> (Char, Char) -> (Int, Memo)
        loop (total, memo) (a, b) = case (times, a, b) `M.lookup` memo of
          Just x -> (total + x, memo)
          Nothing ->
            let (x, memo') = case table M.! (a, b) of
                  [p] -> seqc' memo (times - 1) 'a' p
                  [p1, p2] ->
                    let (x1, m1) = seqc' memo (times - 1) 'a' p1
                        (x2, m2) = seqc' m1 (times - 1) 'a' p2
                     in if x1 < x2 then (x1, m2) else (x2, m2)
             in (total + x, M.insert (times, a, b) x memo')

complexity :: Table -> Int -> String -> Int
complexity table times code = read (init code) * seqc table times code

path :: Char -> Char -> [String]
path a b
  | a == '<' || x1 == 0 && y2 == 3 = [reverse moves ++ "a"]
  | b == '<' || y1 == 3 && x2 == 0 = [moves ++ "a"]
  | otherwise = nub [reverse moves ++ "a", moves ++ "a"]
  where
    (Just (x1, y1), Just (x2, y2)) = (charToCoord a, charToCoord b)
    moves = replicate (abs (y2 - y1)) (if y2 > y1 then 'v' else '^') ++ replicate (abs (x2 - x1)) (if x2 > x1 then '>' else '<')

main :: IO ()
main = do
  file <- catch (readFile "input.txt") ((\_ -> putStrLn "Failed reading file." >> return "") :: IOException -> IO String)
  let keys = "A0123456789a<v>^"
      table = M.fromList [((a, b), path a b) | a <- keys, b <- keys]
  print $ sum $ complexity table 2 <$> words file
