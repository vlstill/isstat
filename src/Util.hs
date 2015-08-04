module Util ( splitBy, trim, separateBy, separate ) where

import Data.Char

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p xs = case break p xs of
                  (a, [])   -> a : []
                  (a, _:bs) -> a : splitBy p bs

trim :: (a -> Bool) -> [a] -> [a]
trim p xs = dropWhile p . reverse . dropWhile p $ reverse xs

separateBy :: Char -> String -> [String]
separateBy c = map (trim isSpace) . splitBy (== c)

separate :: String -> [String]
separate = separateBy ':'
