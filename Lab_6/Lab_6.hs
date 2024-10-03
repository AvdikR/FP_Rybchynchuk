module Main (main) where

-- Визначаємо Y-комбінатор
yCombinator :: (a -> a) -> a
yCombinator f = f (yCombinator f)

-- Функція для факторіала, яка буде використовуватися Y-комбінатором
factorial :: (Int -> Int) -> Int -> Int
factorial f n = if n == 0 then 1 else n * f (n - 1)

-- Нерекурсивна версія факторіала за допомогою Y-комбінатора
nonRecursiveFactorial :: Int -> Int
nonRecursiveFactorial = yCombinator factorial

main :: IO ()
main = do
    putStrLn "Lab 6 | Vadym Rybchynchuk PP-41"
    -- Виведемо результат обчислення факторіала
    putStrLn $ "Factorial of 7: " ++ show (nonRecursiveFactorial 3)

    -- Команда для виклику runghc Lab_6

    