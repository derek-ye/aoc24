module Day1 (day1Solver) where

import Prelude
import Data.List
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Char qualified as MPC

day1Solver :: String -> String -> Int
day1Solver fileName inputData = do
    case runParser parseFile "day1" inputData of
        Left e -> error $ "Can't parse file: " ++ show e -- when do we want to use error over
        Right parsedData -> day1Helper parsedData

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

type Parser = Parsec Void String

parseFile :: Parser [(Int, Int)]
parseFile = manyTill parseLine eof

parseLine :: Parser (Int, Int)
parseLine = do
    col1Num <- decimal
    MPC.space
    col2Num <- decimal
    MPC.space
    pure (col1Num, col2Num)