module View.PpHelper where

import Data.List

newLine :: String
newLine = "\n"

listOf :: (Show a) => String -> [a] -> String
listOf c xs = brackets $ intercalate c (map show xs)

brackets :: String -> String
brackets s = "{" ++ s ++ "}"