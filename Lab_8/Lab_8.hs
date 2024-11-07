module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (withFile, IOMode(WriteMode), hSetEncoding, utf8, stdout, hSetBuffering, BufferMode(LineBuffering))

-- Тип для виразів
data Expr = GCD Expr Expr | Num Int

-- Показувати вирази у зручному форматі
instance Show Expr where
    show (GCD a b) = "GCD (" ++ show a ++ ") (" ++ show b ++ ")"
    show (Num n) = show n

-- Функція для обчислення GCD (найбільший спільний дільник)
eval :: Expr -> Int
eval (GCD a b) = evalGCD (eval a) (eval b)
eval (Num n) = n

-- Функція для обчислення GCD з редукцією
evalGCD :: Int -> Int -> Int
evalGCD m 0 = m
evalGCD m n = evalGCD n (m `mod` n)

-- Структура для графа
data GraphNode = Node Int String
data GraphEdge = Edge Int Int String

-- Функція для експорту графа у форматі DOT
exportGraph :: [GraphNode] -> [GraphEdge] -> Text
exportGraph nodes edges = 
    T.concat [T.pack "digraph GCDGraph {\n", nodesFormat, edgesFormat, T.pack "\n}\n"]
  where
    nodesFormat = T.unlines [T.pack (show nodeId) <> T.pack " [label=\"" <> T.pack label <> T.pack "\"];" | (Node nodeId label) <- nodes]
    edgesFormat = T.unlines [T.concat [T.pack (show from), T.pack " -> ", T.pack (show to), T.pack " [label=\"" <> T.pack label <> T.pack "\"]"] | (Edge from to label) <- edges]

-- Граф для GCD 9 10
gcdGraph :: [GraphNode]
gcdGraph =
    [ Node 1 "gcd(9, 10)"
    , Node 2 "gcd(10, 9 mod 10)" -- обчислення остачі на ділення 9 на 10 = 9
    , Node 3 "gcd(10, 9)" -- заміна (перевтановка)
    , Node 4 "gcd(9, 10 mod 9)" -- обчислення остачі на ділення 10 на 9 = 1
    , Node 5 "gcd(9, 1)" -- обчислення остачі на ділення 9 на 1
    , Node 6 "gcd(1, 0)" -- визначення найбільшого значення
    , Node 7 "1"
    ]

gcdEdges :: [GraphEdge]
gcdEdges =
    [ Edge 1 2 " reducing" -- редукція
    , Edge 2 3 " reversion" -- перестановка
    , Edge 3 4 " reducing" -- редукція (обчислення)
    , Edge 4 5 " gcd(9, 1)" -- обчислення остачі від ділення
    , Edge 5 6 " reducing" -- редукція
    , Edge 6 7 " max" -- кінцевий результат (знаходимо найбільше значення)
    ]

-- Основна функція
main :: IO ()
main = do
    putStrLn "Lab 8 | Vadym Rybchynchuk PP-41"
    hSetEncoding stdout utf8
    hSetBuffering stdout LineBuffering
    
    let expr = GCD (Num 9) (Num 10)
    putStrLn ("Before reduction: " ++ show expr)

    let reducedExpr = eval expr
    putStrLn ("After reduction: " ++ show reducedExpr)

    -- Зберегти граф
    let graph = exportGraph gcdGraph gcdEdges
    withFile "gcd_graph.dot" WriteMode $ \handle -> do
        TIO.hPutStr handle graph
    putStrLn "Graph for GCD is saved in gcd_graph.dot"