antiDiagCoords :: Int -> (Int,Int) -> [((Int,Int),Char)]
antiDiagCoords size queenCoord = makeDiag startCoord where
    makeDiag (row, col)
        | row > size || col > size = []
        | (row, col) == queenCoord = makeDiag (row+1, col+1)
        | otherwise = ((row, col), '#'):makeDiag (row+1, col+1)
    startCoord = uncurry go queenCoord
    go row' col' = if row' == 1 || col' == 1 then (row', col') else go (row' - 1) (col' - 1)
