import Data.List

main = interact $ unlines . balancedSmileysSolver . drop 1 . lines

balancedSmileysSolver :: [String] -> [String]
balancedSmileysSolver sentences = zipWith caseString [1..] solutions
      where solutions = map isBalanced sentences
            caseString i b = "Case #" ++ show i ++ ": " ++ format b
            format b = if b then "YES" else "NO" 

isBalanced = any balanced . parenthesesSeq

parenthesesSeq = parentSeq [""]
parentSeq states (':':')':remain) = parentSeq (addToSeq ["", ")"] states) remain
parentSeq states (':':'(':remain) = parentSeq (addToSeq ["", "("] states) remain
parentSeq states (')':remain)     = parentSeq (addToSeq [")"] states) remain
parentSeq states ('(':remain)     = parentSeq (addToSeq ["("] states) remain
parentSeq states (_:remain)       = parentSeq states remain
parentSeq states []               = states

addToSeq newOnes states = [s++n | n <- newOnes, s <- states]

balanced = balanced' 0
balanced' opens ('(':ps) = balanced' (opens+1) ps
balanced' opens (')':ps)
         | opens == 0     = False
         | otherwise      = balanced' (opens-1) ps
balanced' opens []       = opens == 0
