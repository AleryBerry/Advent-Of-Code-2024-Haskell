import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Data.Either (fromLeft, fromRight, isLeft, isRight)
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)
import GHC.Settings.Utils (maybeRead)

type Rule = (String, String)

type Page = String

type Report = [Page]

pagesBeforeFromRules :: Page -> [Rule] -> [Page]
pagesBeforeFromRules page rules = filter (/= page) $ map snd rulesFromPage
  where
    rulesFromPage = filter (\x -> fst x == page) rules

isPageAfter :: Page -> Report -> Bool
isPageAfter page report = page `elem` report

checkPage :: Page -> [Page] -> [Rule] -> Bool
checkPage page report rules = all (\pageBefore -> pageBefore `isPageAfter` drop 1 (dropWhile (/= page) report)) (pagesBeforeFromRules page rules)

fixReport :: Int -> [Page] -> [(Page, Page)] -> Report
fixReport idx report rules
  | idx >= (length report - 1) = fixReport 0 report rules
  | all (\page -> checkPage page report filteredRules) report = report
  | checkPage (report !! idx) movedReport filteredRules = fixReport (idx + 1) movedReport rules
  | otherwise = fixReport idx movedReport rules
  where
    filteredRules = filter (\(a, b) -> a `elem` report && b `elem` report) rules
    ts = drop (idx + 2) report
    hs = take idx report
    movedReport = hs ++ [report !! (idx + 1), report !! idx] ++ ts

checkReport :: Report -> [Rule] -> Either Report Report
checkReport report rules
  | all (\page -> checkPage page report filteredRules) report = Right report
  | otherwise = Left report
  where
    filteredRules = filter (\(a, b) -> a `elem` report && b `elem` report) rules

convertToRule :: [String] -> Rule
convertToRule pairs = (head pairs, pairs !! 1)

split :: Char -> String -> [Page]
split char "" = []
split char element
  | char == head element = split char (drop 1 element)
  | otherwise = takeWhile (/= char) element : split char (dropWhile (/= char) element)

getRulesAndReports :: ([String], [String]) -> ([Rule], [Report])
getRulesAndReports (a, b) = (map (convertToRule . split '|') a, map (split ',') (drop 1 b))

main :: IO ()
main = do
  file <- catch (readFile "input.txt") ((\_ -> putStrLn "Failed reading file." >> return "") :: IOException -> IO String)
  let (rules, reports) = getRulesAndReports $ break (== "") $ lines file
  let rightReports = filter (not . null) $ map (\x -> fromRight [] $ checkReport x rules) reports
  print $ sum <$> mapM (\x -> (maybeRead :: String -> Maybe Int) $ x !! (length x `div` 2)) rightReports
  let badReports = filter (not . null) $ map (\x -> fromLeft [] $ checkReport x rules) reports
  let badReportsFixed = map (\x -> fixReport 0 x rules) badReports
  print $ sum <$> mapM (\x -> (maybeRead :: String -> Maybe Int) $ x !! (length x `div` 2)) badReportsFixed
