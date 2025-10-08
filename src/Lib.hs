module Lib
    ( main
    ) where

import Prelude
import System.Environment
import Data.Char
import Data.List

data TestOrReal = Test | Real deriving (Read, Show)

main :: IO ()
main = do
    args <- getArgs
    let day :: Int = read $ args !! 0
    let testOrReal :: TestOrReal = read $ args !! 1
    let variant = args !! 2

    inputData <- readFile $ "data/" <> (toLowerStr $ show testOrReal) <> "/" <> "day" <> show day <> ".txt"
    let answer = dispatch day inputData  
    
    putStrLn answer

toLowerStr :: String -> String
toLowerStr = map toLower

-- in the CLI, find: day testOrReal variant
-- pass info into 
dispatch :: Int -> String -> String
dispatch day inputData = case day of 
    1 -> show $ day1Solver inputData
    _ -> error "Not implemented yet"

day1Solver :: String -> Int
day1Solver inputData = undefined -- needs parser

day1Helper :: [(Int, Int)] -> Int
day1Helper pairs =
        let diffList = findDiffsBetweenLists $ makeTwoSortedLists pairs
        in sum diffList
    where
        makeTwoSortedLists :: [(Int, Int)] -> ([Int], [Int])
        makeTwoSortedLists listOfIntPairs =
            let (xs, ys) = unzip listOfIntPairs
                in (sort xs, sort ys)

        findDiffsBetweenLists :: ([Int], [Int]) -> [Int]
        findDiffsBetweenLists (xs, ys) = (\(x, y) -> abs $ y - x) <$> zip xs ys
