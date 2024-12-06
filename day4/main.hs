import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, isJust)
import Debug.Trace (trace)

data Coords = Coords {x :: Int, y :: Int}
  deriving (Eq, Show)

getLetterFromCoords :: Coords -> [String] -> Maybe Char
getLetterFromCoords coords text
  | y coords < 0 || x coords < 0 = Nothing
  | y coords >= length text || x coords >= length (text !! y coords) = Nothing
  | otherwise = Just $ (text !! y coords) !! x coords

xmasFound :: [String] -> String -> String -> Coords -> Coords -> Bool
xmasFound text wordToFind word coords direction
  | newWord == wordToFind = True
  | newWord `isPrefixOf` wordToFind = xmasFound text wordToFind newWord nextCoords direction
  | otherwise = False
  where
    nextCoords = Coords {x = x coords + x direction, y = y coords + y direction}
    newWord = word ++ [fromMaybe '_' (getLetterFromCoords nextCoords text)]

findAllOcurrences :: [Coords] -> String -> Coords -> [String] -> Int
findAllOcurrences directions word coords text = sum $ map (fromEnum . xmasFound text word [head word] coords) directions

findX :: Coords -> [String] -> Int
findX coords text =
  fromEnum $ all (\(a, b) -> (getLetterFromCoords a text == Just 'M' && getLetterFromCoords b text == Just 'S') || (getLetterFromCoords a text == Just 'S' && getLetterFromCoords b text == Just 'M')) diagonals
  where
    diagonals =
      [ (Coords {x = x coords + 1, y = y coords + 1}, Coords {x = x coords - 1, y = y coords - 1}),
        (Coords {x = x coords + 1, y = y coords - 1}, Coords {x = x coords - 1, y = y coords + 1})
      ]

iterateText :: Int -> Char -> (Coords -> [String] -> Int) -> [String] -> Int
iterateText idx start func text
  | idx >= length (head text) * length text = 0
  | getLetterFromCoords Coords {x = xCoord, y = yCoord} text == Just start = func Coords {x = xCoord, y = yCoord} text + iterateText (idx + 1) start func text
  | otherwise = iterateText (idx + 1) start func text
  where
    xCoord = idx `mod` length (head text)
    yCoord = idx `div` length (head text)

main :: IO ()
main = do
  file <- catch (readFile "input.txt") ((\_ -> putStrLn "Failed reading file." >> return "") :: IOException -> IO String)
  print $ iterateText 0 'X' (findAllOcurrences directions "XMAS") (lines file) -- Part 1
  print $ iterateText 0 'A' findX (lines file) -- Part 2
  where
    directions =
      [ Coords {x = 1, y = 0},
        Coords {x = -1, y = 0},
        Coords {x = 0, y = 1},
        Coords {x = 0, y = -1},
        Coords {x = -1, y = -1},
        Coords {x = 1, y = -1},
        Coords {x = -1, y = 1},
        Coords {x = 1, y = 1}
      ]
