module Main (main) where

import Data.IORef

-- Функція для оновлення лічильника
updateCounter :: IORef Int -> IO ()
updateCounter reference = do
  modifyIORef' reference (+1)

-- Основна функція
main :: IO ()
main = do
  putStrLn "Lab 3 | Vadym Rybchynchuk PP-41"
  -- Ініціалізація лічильника
  ref <- newIORef (0 :: Int)

  -- Виклики функції, яка збільшує лічильник
  updateCounter ref
  updateCounter ref
  updateCounter ref
  updateCounter ref

  -- Читання поточного значення лічильника
  count <- readIORef ref

  -- Виведення результату
  putStrLn $ "(Part 1) The function has been called " ++ show count ++ " times"

  updateCounter ref
  updateCounter ref

  count <-readIORef ref

  -- Виведення результату
  putStrLn $ "(Part 2) The function has been called " ++ show count ++ " times"