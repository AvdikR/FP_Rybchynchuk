rangeOp :: (a -> a -> a) -> [a] -> (Int, Int) -> a
rangeOp op arr (start, end) = foldl1 op $ take (end - start) (drop start arr)

processRanges :: (a -> a -> a) -> [a] -> [(Int, Int)] -> [a]
processRanges op arr = map (rangeOp op arr)

main :: IO ()
main = do
    putStrLn "Lab 1 | Vadym Rybchynchuk PP-41"
    -- Вводимо масив
    putStrLn "Enter array elements separated by spaces: "
    inputArr <- getLine
    let arr = map read (words inputArr) :: [Int]

    -- Вводимо кількість діапазонів
    putStrLn "Enter number of ranges: "
    rangeCount <- readLn :: IO Int

    -- Вводимо межі діапазонів
    ranges <- mapM (\_ -> do
        putStrLn "Enter range (start <space> end): "
        rangeInput <- getLine
        let [start, end] = map read (words rangeInput) :: [Int]
        return (start, end)
        ) [1..rangeCount]

    -- Обираємо операцію
    putStrLn "\nSelect operation: (S for sum, P for product): "
    operation <- getLine

    let result = case operation of
            "S"    -> processRanges (+) arr ranges -- обчислення суми
            "P"    -> processRanges (*) arr ranges -- обчислення добутку
            --"Min"  -> processRanges min arr ranges
            --"Max"  -> processRanges max arr ranges
            _      -> error "Invalid operation"

    -- Результати
    putStrLn "\nResults:"
    print result