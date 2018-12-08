module Main where 
import Data.List (elemIndices)

main :: IO()
main = do s <- getLine
          n <- getLine
          putStrLn $ show $ indexLista s n
          return()

indexLista :: [Char] -> [Char] -> [Int]
indexLista [] _ = []
indexLista (xs) (ch:cs) = elemIndices ch xs 