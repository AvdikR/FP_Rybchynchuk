module Main (main) where

-- Функція, що обчислює добуток п'яти натуральних чисел
multiplyFive :: Num a => a -> a -> a -> a -> a -> a
multiplyFive a b c d e = a * b * c * d * e

-- Часткове використання функції multiplyFive з чотирма аргументами
multiplyFour :: Num a => a -> a -> a -> a -> a
multiplyFour = multiplyFive 4

-- Часткове використання функції multiplyFive з трьома аргументами
multiplyThree :: Num a => a -> a -> a -> a
multiplyThree = multiplyFive 2 3 

-- Часткове використання функції multiplyFive з двома аргументами
multiplyTwo :: Num a => a -> a -> a
multiplyTwo = multiplyFive 2 3 4

-- Часткове використання функції multiplyFive з одним аргументом
multiplyOne :: Num a => a -> a
multiplyOne = multiplyFive 1 7 4 9

main :: IO ()
main = do
    putStrLn "Lab 4 | Vadym Rybchynchuk PP-41"
    -- Введення числа для multiplyOne
    putStrLn "Enter a number for multiplyOne:"
    numOne <- readLn :: IO Int
    print $ multiplyOne numOne 

    -- Введення двох чисел для multiplyTwo
    putStrLn "\nEnter two numbers for multiplyTwo:"
    numTwo1 <- readLn :: IO Int
    numTwo2 <- readLn :: IO Int
    print $ multiplyTwo numTwo1 numTwo2 

    -- Введення трьох чисел для multiplyThree
    putStrLn "\nEnter three numbers for multiplyThree:"
    numThree1 <- readLn :: IO Int
    numThree2 <- readLn :: IO Int
    numThree3 <- readLn :: IO Int
    print $ multiplyThree numThree1 numThree2 numThree3 

    -- Введення чотирьох чисел для multiplyFour
    putStrLn "\nEnter four numbers for multiplyFour:"
    numFour1 <- readLn :: IO Int
    numFour2 <- readLn :: IO Int
    numFour3 <- readLn :: IO Int
    numFour4 <- readLn :: IO Int
    print $ multiplyFour numFour1 numFour2 numFour3 numFour4 