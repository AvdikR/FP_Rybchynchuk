module Main (main) where

main :: IO ()
main = do
    putStrLn "Lab 5 | Vadym Rybchynchuk PP-41"
    putStrLn "Think of a number between 0 and 99."
    guessNumber 0 99

-- Функція для вгадування числа
guessNumber :: Int -> Int -> IO ()
guessNumber min max
    | min == max = putStrLn $ "Your number is " ++ show min ++ "!"  -- Якщо залишилося одне можливе значення, виводимо результат
    | otherwise = do
        let mid = (min + max) `div` 2  -- Обчислюємо середнє значення діапазону
        putStrLn $ "Is your number less than " ++ show mid ++ "? (Yes/No)"
        answer <- getLine
        case answer of
            "Yes" -> guessNumber min (mid - 1)  -- Якщо відповідь "Так", звужуємо діапазон вниз
            "No"  -> do
                putStrLn $ "Is your number greater than " ++ show mid ++ "? (Yes/No)"
                answerGreater <- getLine
                case answerGreater of
                    "Yes" -> guessNumber (mid + 1) max  -- Якщо число більше, звужуємо діапазон вгору
                    "No"  -> putStrLn $ "Your number is " ++ show mid ++ "!"  -- Якщо не більше і не менше, число знайдено
                    _     -> do
                        putStrLn "Please answer Yes or No."
                        guessNumber min max  -- Повторюємо питання, якщо введено некоректну відповідь
            _ -> do
                putStrLn "Please answer Yes or No."
                guessNumber min max  -- Якщо введено некоректну відповідь, повторюємо питання