module Main where 
import Data.Char(digitToInt)

-- minha ideia nessa questao foi gerar todos os subconjuntos possiveis da lista, o que gera uma lista de listas
-- somar cada subconjunto e adicionar numa unica lista que contem a soma deles e entao verificar se essa 
-- soma eh igual ao numero solicitado

main :: IO()
main = do inp <- getLine
          sm <- getLine
          print $ checkSum (somaMatriz (sublistas inp)) (read sm :: Int)
          
sublistas :: [Char] -> [[Char]]
sublistas []      =  [[]]
sublistas (x:xs)  =  [ x:ys | ys <- sublistas xs ] ++ sublistas xs

checkSum ::[Int] -> Int -> Int
checkSum [] n = 0
checkSum (x:xs) n = if x == n
                    then 1 + checkSum xs n
                    else checkSum xs n

somaLista :: [Char] -> Int
somaLista [] = 0
somaLista (x:xs) = if x == '.' || x == ' ' then 0 else digitToInt x + somaLista xs

somaMatriz :: [[Char]] -> [Int]
somaMatriz [] = []
somaMatriz (x:xs) = somaLista x : somaMatriz xs