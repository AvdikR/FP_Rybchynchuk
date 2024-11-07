module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (withFile, IOMode(WriteMode), hSetEncoding, utf8, stdout, hSetBuffering, BufferMode(LineBuffering))

type Node = Int -- вузли
type Label = String
type Edge = (Node, Node, Label) -- ребра

-- Модифікований Y-комбінатор для Haskell
yCombinator :: (a -> a) -> a
yCombinator f = f (yCombinator f)

-- Функція факторіала
factorial :: (Int -> Int) -> Int -> Int
factorial rec 0 = 1
factorial rec n = n * rec (n - 1)

-- Обчислення факторіала через Y-комбінатор
computeFactorial :: Int -> Int
computeFactorial n = yCombinator (\f -> \x -> factorial f x) n

-- Побудова графа факторіала з етапами обчислення
factorialGraph :: Int -> [(Node, Label)] -> [Edge] -> ([(Node, Label)], [Edge])
factorialGraph 0 nodes edges = (nodes, edges)
factorialGraph n nodes edges =
    let nextNode = length nodes + 1
        newNodeLabel = show n ++ " * factorial(" ++ show (n - 1) ++ ")"
        newEdge = (nextNode - 1, nextNode, " recurse")
        (updatedNodes, updatedEdges) = factorialGraph (n - 1) ((nextNode, newNodeLabel) : nodes) (newEdge : edges)
    in (updatedNodes, updatedEdges)

-- Етапи обчислення факторіала
initialNodes :: Int -> [(Node, Label)]
initialNodes n = [(1, "Factorial(" ++ show n ++ ")")]

-- Функція для експорту графа у форматі DOT
exportGraph :: [(Node, Label)] -> [Edge] -> Text
exportGraph nodes edges = 
    T.concat [T.pack "digraph FactorialGraph {\n", nodesFormat, edgesFormat, T.pack "\n}\n"]
  where
    nodesFormat = T.unlines [T.pack (show node) <> T.pack " [label=\"" <> T.pack label <> T.pack "\"];" | (node, label) <- nodes]
    edgesFormat = T.unlines [T.concat [T.pack (show from), T.pack " -> ", T.pack (show to), T.pack " [label=\"" <> T.pack label <> T.pack "\"]"] | (from, to, label) <- edges]

-- Функція для збереження графа у файл з UTF-8 кодуванням
saveGraph :: FilePath -> [(Node, Label)] -> [Edge] -> IO ()
saveGraph filePath nodes edges = 
    withFile filePath WriteMode $ \handle -> do
        hSetEncoding handle utf8
        TIO.hPutStr handle (exportGraph nodes edges)

-- Основна функція для побудови та збереження графів обчислення та редукції
main :: IO ()
main = do
    putStrLn "Lab 7 | Vadym Rybchynchuk PP-41"
    hSetEncoding stdout utf8
    hSetBuffering stdout LineBuffering

    let number = 4  -- Змініть це значення, щоб знайти факторіал іншого числа
    let result = computeFactorial number
    putStrLn $ "Factorial of " ++ show number ++ " is: " ++ show result

    -- Створення графа обчислення факторіала
    let (factorialGraphNodes, factorialGraphEdges) = factorialGraph number (initialNodes number) []
    saveGraph "factorial_initial.dot" factorialGraphNodes factorialGraphEdges

    -- Створення графа редукції
    let reductionGraphNodes = 
            [ (1, "Y (factorial)"), 
              (2, "λf.λn.if n == 0 then 1 else n * f (n-1)"), -- первірка чи значення факторіалу дорівнює 0
              (3, "apply Y to factorial"),  -- застосування Y-комбінатор до функції факторіала, щоб почати процес обчислення.
              (4, "n = " ++ show number), -- встановлення значення n
              (5, "if " ++ show number ++ " == 0 then 1 else " ++ show number ++ " * factorial(" ++ show (number - 1) ++ ")"), -- первірка чи вказане значення дорівнює 0
              (6, "factorial(" ++ show (number - 1) ++ ")"), -- виклик factorial для n-1.
              (7, "if " ++ show (number - 1) ++ " == 0 then 1 else " ++ show (number - 1) ++ " * factorial(" ++ show (number - 2) ++ ")"),
              (8, "factorial(" ++ show (number - 2) ++ ")"),
              (9, "result: " ++ show result)
            ]
    
    let reductionGraphEdges = 
            [ (1, 2, "Y definition"), 
              (2, 3, "expand Y"),
              (3, 4, "set n"),
              (4, 5, "evaluate if-else"),
              (5, 6, "calculate recursive step"),
              (6, 7, "expand next step"),
              (7, 8, "calculate recursive step"),
              (8, 9, "final result")
            ]
    saveGraph "factorial_reduction.dot" reductionGraphNodes reductionGraphEdges

    putStrLn "Graphs for the factorial calculation and reduction are saved in files: factorial_initial.dot, factorial_reduction.dot"