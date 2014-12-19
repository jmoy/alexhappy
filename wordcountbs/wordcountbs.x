{
module Main(main) where
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.Text.Lazy.IO as TIO
}

%wrapper "basic-bytestring"

$letter = [a-zA-Z]
$nonletter = [~$letter\n]

tokens :-
  $nonletter+     ;
  $letter+        {TE.decodeUtf8}

{
main::IO ()
main = do
  s <- fmap TE.encodeUtf8 TIO.getContents
  let toks = alexScanTokens s
  mapM_ TIO.putStrLn toks
}
