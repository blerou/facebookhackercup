import Data.Char
import Data.List

main = interact $ unlines . beautifulStringsSolver . drop 1 . lines

beautifulStringsSolver :: [String] -> [String]
beautifulStringsSolver strings = zipWith formatter [1..] solutions
      where solutions = map beautyIndex strings
            formatter i b = "Case #" ++ show i ++ ": " ++ show b

beautyIndex :: String -> Int
beautyIndex = calculateIndex . occurrences . letterGroups

letterGroups = group . sort . filter (`elem` ['a'..'z']) . map toLower
occurrences = reverse . sort . map length
calculateIndex = sum . zipWith (*) [26,25..1]
