module Pangram (isPangram) where

import Data.Char (toLower)

allLowerLetters :: [Char]
allLowerLetters = ['a'..'z']

stringToLower :: [Char] -> [Char]
stringToLower = map toLower

isPangram :: String -> Bool
isPangram text =  all (\c -> c `elem` stringToLower text) allLowerLetters
