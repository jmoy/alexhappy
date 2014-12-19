{
module Lexer (alexScan,AlexReturn(..)) where
import qualified Data.Text.Lazy as T
import Input
}

$letter = [a-zA-Z]
$nonletter = [~$letter\n]

tokens :-
  $nonletter+     ;
  $letter+        {makeToken}

{
}
