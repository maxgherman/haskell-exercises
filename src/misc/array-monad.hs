runTurn :: Int -> [Int]
runTurn x = [x - 1, x, x + 1]


runGame :: Int -> [Int]
runGame x = do
  m1 <- runTurn x
  m2 <- runTurn m1
  m3 <- runTurn m2
  m4 <- runTurn m3
  m5 <- runTurn m4
  return m5