module Main where

import Reader

main :: IO ()
main = do
    s <- runWithCtx computation
    putStrLn s
