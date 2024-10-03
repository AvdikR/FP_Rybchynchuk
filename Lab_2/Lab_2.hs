module Main (main) where

-- Синоніми типів
type TheInt = Int
type TheBool = Bool
type TheString = String
type TheFloat = Float
type TheDouble = Double

-- Синонім типу для структури Character
type Human = Character TheString TheInt TheFloat

-- Віднімання двох чисел
diff :: TheInt -> TheInt -> TheInt
diff x y = x - y

data Character a b c = Character {
    name :: a,
    level :: b,
    progress :: c
} deriving (Show)

-- Функція для виведення полів Character
printHumanInfo :: (Show a, Show b, Show c) => Character a b c -> IO ()
printHumanInfo (Character {name=a, level=b, progress=c}) = do
    putStrLn $ "Name: " ++ show a  -- Зміна show a на a, оскільки a вже рядок
    putStrLn $ "Level: " ++ show b
    putStrLn $ "Progress: " ++ show c

-- Виведення результатів
main :: IO ()
main = do
    putStrLn "Lab 2 | Vadym Rybchynchuk PP-41"
    putStrLn "Difference:"
    print (diff 100 25)
    
    -- Створення об'єкта з використанням синоніму типу Human
    let human = Character {name="Red", level=25, progress=61.82} :: Human
    putStrLn "\nHuman Information:"
    printHumanInfo human