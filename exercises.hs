
hello = putStrLn "hello world"

fib n | n <= 2    = 1
      | otherwise = fib (n - 1) + fib (n - 2)
 
tfib n | n <= 2    = 1
       | otherwise = go 2 1 1
  where go x i0 i1 | x >= n    = i1
                   | otherwise = go (x + 1) i1 (i0 + i1)

data Color = Red | Green | Blue | Violet
           deriving (Show, Eq, Enum)

targetColor :: Color
targetColor = Blue

isTarget :: Color -> Bool
isTarget c | c == targetColor = True
isTarget anythingElse         = False

isHello "hello" = True
isHello anything = False


data Move = Rock | Paper | Scissors deriving (Eq, Read, Show, Enum, Bounded)

data Outcome = Lose | Tie | Win deriving (Show, Eq, Ord)

-- | @outcome our_move their_move@
outcome :: Move -> Move -> Outcome
outcome Rock Scissors        = Win
outcome Paper Rock           = Win
outcome Scissors Paper       = Win
outcome us them | us == them = Tie
                | otherwise  = Lose

