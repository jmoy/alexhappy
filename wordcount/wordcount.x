{
module Main(main) where
}

%wrapper "basic"

$letter = [a-zA-Z]
$nonletter = [~$letter\n]

tokens :-
  $nonletter+     ;
  $letter+        {id}

{
main::IO ()
main = do
  s <- getContents
  let toks = alexScanTokens s
  mapM_ putStrLn toks
}
