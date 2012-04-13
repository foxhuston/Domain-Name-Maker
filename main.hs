module Main (main) where

import System (getArgs)
import System.IO

import Data.List
import Data.Maybe

suffixen = ["com", "net", "org", "in", "info", "us", "me", "co", "ca", "mobi", "biz", "de", "tv", "eu", "cc", "ws", "bz", "cm", "nu"]

main :: IO ()
main = do
    args <- getArgs
    let fileName = args !! 0
    
    handle <- openFile fileName ReadMode
    hSetEncoding handle utf8
    file <- hGetContents handle

    let terms = lines file
    mapM_ putStrLn $ sepTerms terms suffixen

sepTerms :: [String] -> [String] -> [String]
sepTerms terms suffixes =
    let
        foundTerms = map (flip termSuffix suffixes) terms 
        justTerms  = filter isJust foundTerms
    in map fromJust justTerms
    

termSuffix :: String -> [String] -> Maybe String
termSuffix term suffixes = do
    foundSuffix <- find (`isSuffixOf` term) suffixes
    let len = length foundSuffix
    return $ injectDot len term

injectDot :: Int -> String -> String
injectDot index term =
    let
        index'          = length term - index
        (before, after) = splitAt index' term
    in before ++ "." ++ after
