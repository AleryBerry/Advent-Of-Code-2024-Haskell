import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, isNothing)
import GHC.Base (liftA2)
import GHC.Settings.Utils (maybeRead)

data Mul = Mul {first :: Int, second :: Int}
  deriving (Read, Show, Eq)

createMul :: Maybe Int -> Maybe Int -> Maybe Mul
createMul a b
  | isNothing a || isNothing b = Nothing
  | otherwise = Just $ Mul {first = fromMaybe 0 a, second = fromMaybe 0 b}

maybeConcat :: Maybe [t] -> Maybe [t] -> Maybe [t]
maybeConcat a Nothing = a
maybeConcat Nothing b = b
maybeConcat a b = liftA2 (++) a b

findNumber :: Maybe Int -> String -> Maybe Int
findNumber Nothing text = Nothing
findNumber number "" = number
findNumber number text
  | head text == ',' || head text == ')' = number
  | otherwise = findNumber ((maybeRead :: String -> Maybe Int) (show (fromMaybe 0 number) ++ [head text])) (drop 1 text)

findMuls :: Maybe String -> String -> Maybe [Mul]
findMuls word "" = Nothing
findMuls Nothing text
  | "do()" `isPrefixOf` text = findMuls (Just "") (drop 4 text)
  | otherwise = findMuls Nothing (drop 1 text)
findMuls (Just "mul(") text
  | isNothing number || isNothing number2 = findMuls (Just "") (drop 1 text)
  | otherwise = sequenceA [createMul number number2] `maybeConcat` findMuls (Just "") (drop (length number2) lastText)
  where
    number = findNumber ((maybeRead :: String -> Maybe Int) [head text]) (drop 1 text)
    number2 = findNumber ((maybeRead :: String -> Maybe Int) [head lastText]) (drop 1 lastText)
    lastText = drop (length (show $ fromMaybe 0 number) + 1) text
findMuls _ text
  | "don't()" `isPrefixOf` text = findMuls Nothing (drop 7 text)
  | "mul(" `isPrefixOf` text = findMuls (Just "mul(") (drop 4 text)
  | otherwise = findMuls (Just "") (drop 1 text)

main :: IO ()
main = do
  file <- catch (readFile "input.txt") ((\_ -> putStrLn "Failed reading file." >> return "") :: IOException -> IO String)
  print $ foldl (\acc x -> acc + (first x * second x)) 0 <$> findMuls (Just "") file
