import Data.Char
import Data.List

main = interact beautifulStringsSolver

beautifulStringsSolver :: String -> String
beautifulStringsSolver = unlines . solveCases . drop 1 . lines

solveCases :: [String] -> [String]
solveCases cases = zipWith formatter [1..] solutions
      where solutions = map solveBS cases
            formatter i b = "Case #" ++ show i ++ ": " ++ show b

solveBS :: String -> Int
solveBS = sum . zipWith (*) [26,25..1] . reverse . sort . map length . letterGroups 

letterGroups = group . sort . filter (`elem` ['a'..'z']) . map toLower