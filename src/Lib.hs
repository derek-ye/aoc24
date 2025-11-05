module Lib
    ( main
    ) where

import Prelude
import System.Environment
import Data.Char
import Day1
import Types

main :: IO ()
main = do
    args <- getArgs
    let day :: Int = read $ args !! 0
    let testOrReal :: TestOrReal = read $ args !! 1
    let variant :: Variant = read $ args !! 2

    let fileName = "data/" <> (toLowerStr $ show testOrReal) <> "/" <> "day" <> show day <> ".txt"
    inputData <- readFile $ fileName
    let answer = dispatch day fileName inputData variant

    putStrLn answer

toLowerStr :: String -> String
toLowerStr = map toLower

dispatch :: Int -> String -> String -> Variant -> String
dispatch day fileName inputData variant = case day of 
    1 -> show $ day1Solver fileName inputData variant
    _ -> error "Not implemented yet"
