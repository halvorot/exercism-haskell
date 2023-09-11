module DNA (toRNA) where

transformDnaChar :: Char -> Char
transformDnaChar c  = case c of
    'C' -> 'G'
    'G' -> 'C'
    'T' -> 'A'
    'A' -> 'U'
    x -> x

isValidChar :: Char -> Bool
isValidChar = (`elem` ['C','G','T','A'])

isInvalidChar :: Char -> Bool
isInvalidChar c = not (isValidChar c)

toRNA :: String -> Either Char String
toRNA xs
    | any isInvalidChar xs  = Left $ head $ filter isInvalidChar xs
    | length xs == 1        = Right [transformDnaChar $ head xs]
    | otherwise             = Right $ map transformDnaChar xs
