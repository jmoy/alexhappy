{
module Parser (parse) where

import LexParseCommon
import Lexer
}

%name parserAct
%tokentype {Token}
%error {parseError}
%monad {P}
%lexer {lexer} {TEOF}

%token
  cat           {TCat}
  string        {TString $$}

%%
Program : Endowments {$1}

Endowments:               {[]}
          |string Catlist Endowments {($1,$2):$3}

Catlist : cat {1}
        | Catlist cat {$1+1}

{
parseError _ = do
  lno <- getLineNo
  error $ "Parse error on line "++show lno

parse::String->[(String,Int)]
parse s = evalP parserAct s
}

