module Main where
import System.Environment

argToInt index args =
  (read $ args !! index) :: Int
  
main :: IO ()
main = do args <- getArgs
          putStrLn $ "Sum: " ++ (show $ (argToInt 0 args)  + (argToInt 1 args))
