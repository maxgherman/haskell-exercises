data Board = Board deriving Show

nextPositions :: Board -> [Board]
nextPositions b = [b]

nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN b n p
  | n < 0 = []
  | n == 0 = filter p [b]
  | otherwise = [result |
        positions <- nextPositions b,
        result <- nextPositionsN positions (n - 1) p
    ]



pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x
    | x <= 0 = []
    | otherwise = do {
        c <- [1..x];
        b <- [1..x];
        a <- [1..(b-1)];
        if a^2 + b^2 == c^2 then [1] else [];
        return (a, b, c)
    }
