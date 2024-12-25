import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Control.Monad (guard)
import Data.Bits (xor)
import Data.Char (isDigit)
import Data.List (isSuffixOf, tails)
import Debug.Trace (trace)

data Instruction = ADV | BXL | BST | JNZ | BXC | OUT | BDV | CDV
  deriving (Eq, Show, Ord)

type Program = [Int]

type Registers = (Int, Int, Int)

type Output = [Int]

type Computer = (Registers, Program, Pointer, Output)

type Operand = Int

type Pointer = Int

valueToInstruction :: Int -> Maybe Instruction
valueToInstruction 0 = Just ADV
valueToInstruction 1 = Just BXL
valueToInstruction 2 = Just BST
valueToInstruction 3 = Just JNZ
valueToInstruction 4 = Just BXC
valueToInstruction 5 = Just OUT
valueToInstruction 6 = Just BDV
valueToInstruction 7 = Just CDV
valueToInstruction _ = Nothing

getValue :: Computer -> Operand -> Maybe Int
getValue ((a, b, c), _, _, _) 4 = Just a
getValue ((a, b, c), _, _, _) 5 = Just b
getValue ((a, b, c), _, _, _) 6 = Just c
getValue _ x
  | x >= 0 && x <= 3 = Just x
  | otherwise = Nothing

run' :: Computer -> Instruction -> Operand -> Maybe Computer
run' computer@((a, b, c), d, p, o) ADV operand
  | (Just val) <- getValue computer operand = Just ((a `quot` 2 ^ val, b, c), d, p + 2, o)
  | Nothing <- getValue computer operand = Nothing
run' ((a, b, c), d, p, o) BXL operand = Just ((a, b `xor` operand, c), d, p + 2, o)
run' computer@((a, b, c), d, p, o) BST operand
  | (Just val) <- getValue computer operand = Just ((a, val `mod` 8, c), d, p + 2, o)
  | Nothing <- getValue computer operand = Nothing
run' ((0, _, _), _, _, _) JNZ _ = Nothing
run' computer@((a, b, c), d, p, o) JNZ operand = Just ((a, b, c), d, operand, o)
run' computer@((a, b, c), d, p, o) BXC _ = Just ((a, b `xor` c, c), d, p + 2, o)
run' computer@(r, d, p, o) OUT operand
  | (Just val) <- getValue computer operand = Just (r, d, p + 2, val `mod` 8 : o)
  | Nothing <- getValue computer operand = Nothing
run' computer@((a, _, c), d, p, o) BDV operand
  | (Just val) <- getValue computer operand = Just ((a, a `quot` 2 ^ val, c), d, p + 2, o)
  | Nothing <- getValue computer operand = Nothing
run' computer@((a, b, _), d, p, o) CDV operand
  | (Just val) <- getValue computer operand = Just ((a, b, a `quot` 2 ^ val), d, p + 2, o)
  | Nothing <- getValue computer operand = Nothing

run :: Computer -> [Int]
run computer@(_, d, p, o)
  | length d > p + 1 && p >= 0,
    (Just op) <- valueToInstruction (d !! p),
    (Just newComputer) <- run' computer op (d !! (p + 1)) =
      run
        newComputer
  | otherwise = reverse o

search :: Computer -> Int -> Int -> Int
search ((_, _, _), d, p, o) b c = head (go =<< [0 .. 7])
  where
    go a =
      case run ((a, 0, 0), d, p, o) of
        out
          | out == d -> [a]
          | out `isSuffixOf` d -> go =<< take 8 [8 * a ..]
          | otherwise -> []

main :: IO ()
main = do
  file <- catch (readFile "input.txt") ((\_ -> putStrLn "Failed reading file." >> return "") :: IOException -> IO String)
  let (x : y : z : xs) = (read :: String -> Int) . dropWhile (not . isDigit) <$> take 3 (lines file)
  let program = (\a -> (read :: String -> Int) . (: []) <$> a) <$> filter isDigit $ last (lines file)
  let computer = ((x, y, z), program, 0, [])
  print $ run computer
