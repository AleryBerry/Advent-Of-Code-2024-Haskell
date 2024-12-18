import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Data.Either (fromRight, isRight)
import Data.Sequence qualified as S
import GHC.Settings.Utils (maybeRead)

type Block = Either Int (Int, Int)

part1 :: Int -> [Maybe Int] -> [Maybe Int]
part1 _ [] = []
part1 !n ((Just file) : (Just free) : xs) = replicate file (Just n) <> replicate free Nothing <> part1 (n + 1) xs
part1 !n [Just file] = replicate file (Just n)

part2 :: Int -> [Maybe Int] -> [Block]
part2 _ [] = []
part2 !n ((Just file) : (Just free) : xs) = [Right (file, n), Left free] ++ part2 (n + 1) xs
part2 !n [Just file] = [Right (file, n)]

merge :: [Maybe Int] -> [Maybe Int] -> [Int]
merge [] _ = []
merge _ [] = []
merge (Just x : xs) ys = x : merge xs ys
merge (Nothing : xs) (Just y : ys) = y : merge xs ys
merge xs (Nothing : ys) = merge xs ys

moveFiles :: S.Seq Block -> S.Seq Block
moveFiles S.Empty = S.Empty
moveFiles (xs S.:|> Left a) = moveFiles xs S.:|> Left a
moveFiles (xs S.:|> Right (a, b)) =
  case maybeFits (a, b) xs of
    Nothing -> moveFiles xs S.:|> Right (a, b)
    Just x -> moveFiles x S.:|> Left a

maybeFits :: (Int, Int) -> S.Seq Block -> Maybe (S.Seq Block)
maybeFits _ S.Empty = Nothing
maybeFits (size, id) (Left n S.:<| xs)
  | n >= size = Just $ Right (size, id) S.:<| Left (n - size) S.:<| xs
  | otherwise = (Left n S.:<|) <$> maybeFits (size, id) xs
maybeFits a (x S.:<| xs) = (x S.:<|) <$> maybeFits a xs

total :: Int -> S.Seq Block -> Int
total _ S.Empty = 0
total n (Left m S.:<| xs) = total (n + m) xs
total n (Right (m, c) S.:<| xs) = sum (take m $ map (* c) [n ..]) + total (n + m) xs

main :: IO ()
main = do
  file <- catch (readFile "input.txt") ((\_ -> putStrLn "Failed reading file." >> return "") :: IOException -> IO String)
  let input = part2 0 $ (maybeRead :: String -> Maybe Int) . (: "") <$> file
  let files = reverse $ fromRight (0, 0) <$> filter isRight input
  print input
  print $ total 0 $ moveFiles $ S.fromList input