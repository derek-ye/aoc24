module Day1 (day1Solver) where

import Prelude
import Data.List
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Char qualified as MPC
import Types (Variant(..))
import Data.Map (Map)
import Data.Map qualified as Map
import Debug.Trace

day1Solver :: String -> String -> Variant -> Int
day1Solver fileName inputData variant =
    let helper = case variant of
                    Star1 -> day1Helper
                    Star2 -> day1Part2Helper
    in
        case runParser parseFile "day1" inputData of
            Left e -> error $ "Can't parse file: " ++ show e -- when do we want to use error over
            Right parsedData -> helper parsedData

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

day1Part2Helper :: [(Int, Int)] -> Int
day1Part2Helper pairs = let
    (left, right) = unzip pairs
    -- Get a map of the right list + the # times those numbers occur
    rightCountMap' = updateCountMap right

    -- For each number in left, calculate a similarity score and add it
    in sum $ fmap (getSimilarityScore rightCountMap') left
    where
        updateCountMap :: [Int] -> Map Int Int
        updateCountMap right = foldl' oneIter Map.empty right

        oneIter :: Map Int Int -> Int -> Map Int Int
        oneIter m k = Map.alter f k m
            where
                f :: Maybe Int -> Maybe Int
                f Nothing = Just 1
                f (Just x) = Just $ x + 1

        getSimilarityScore :: Map Int Int -> Int -> Int
        getSimilarityScore countMap n =
            case Map.lookup n countMap of
                Just v -> n * v
                Nothing -> 0 -- the right side doesn't have the number on the left side, so 0 it has occurrences

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
