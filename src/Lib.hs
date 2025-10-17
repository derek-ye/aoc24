module Lib
    ( main
    ) where

import Prelude
import System.Environment
import Data.Char
import Day1

data TestOrReal = Test | Real deriving (Read, Show)

main :: IO ()
main = do
    args <- getArgs
    let day :: Int = read $ args !! 0
    let testOrReal :: TestOrReal = read $ args !! 1
    let variant = args !! 2

    let fileName = "data/" <> (toLowerStr $ show testOrReal) <> "/" <> "day" <> show day <> ".txt"
    inputData <- readFile $ fileName
    let answer = dispatch day fileName inputData
    
    putStrLn answer

toLowerStr :: String -> String
toLowerStr = map toLower

dispatch :: Int -> String -> String -> String
dispatch day fileName inputData = case day of 
    1 -> show $ day1Solver fileName inputData
    _ -> error "Not implemented yet"
