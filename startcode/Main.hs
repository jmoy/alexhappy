module Main where
import Parser
import Control.Monad (forM_)

main::IO()
main = do
  s <- getContents
  let endowments = parse s
  forM_ endowments $
    \(name,num) -> putStrLn $ name++" got "++show num++" cats."
