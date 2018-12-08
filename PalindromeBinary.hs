module Main where 
main :: IO()
main = do n <- getLine
          checkPalin (read n :: Int)
          return()


checkPalin :: Int -> IO()
checkPalin x = if (toBin x) == (reverse (toBin x)) 
               then putStrLn "True" 
               else putStrLn "False"


toBin :: Int -> [Int]
toBin 0 = [0]
toBin n = reverse $ helper n where helper 0 = []
                                   helper n 
                                    | mod n 2 == 1 = 1 : helper (n `div` 2)
                                    | mod n 2 == 0 = 0 : helper (n `div` 2)