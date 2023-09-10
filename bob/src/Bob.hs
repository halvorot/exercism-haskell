module Bob (responseFor) where

import Data.Char

allCapitalLetters :: [Char]
allCapitalLetters = ['A'..'Z']

allLetters :: [Char]
allLetters = ['a'..'z'] ++ allCapitalLetters

filterToOnlyLetters :: String -> String
filterToOnlyLetters = filter (`elem` allLetters)

removeTrailingWhitespaces :: String -> String
removeTrailingWhitespaces text 
            | isSpace (last text) = removeTrailingWhitespaces (init text)
            | otherwise = text

responseFor :: String -> String
responseFor text
            | isSilence         = "Fine. Be that way!"
            | isYellQuestion    = "Calm down, I know what I'm doing!"
            | isQuestion        = "Sure."
            | isYell            = "Whoa, chill out!"
            | otherwise         = "Whatever."
            where
                isQuestion = last (removeTrailingWhitespaces text) == '?'
                isYell = all (`elem` allCapitalLetters) (filterToOnlyLetters text) && not (null (filterToOnlyLetters text))
                isYellQuestion = isYell && isQuestion
                isSilence = all isSpace text || null text
