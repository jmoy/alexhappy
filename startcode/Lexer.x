{
module Lexer (lexer)  where

import LexParseCommon
import Control.Monad.State
}

tokens :-

<0>         $white+           ;
<0>         cat               {plainTok TCat}

<0>         \"              {beginString}
<stringSC>  \"              {endString}
<stringSC>  .                 {appendString}
<stringSC>  \\[nt\"]        {escapeString}

<0,commentSC>            "/*"              {beginComment}
<commentSC> "*/"              {endComment}
<commentSC> [.\n]            ;
{
type LexAction = Int->String->P (Maybe Token)

plainTok::Token->LexAction
plainTok t _ _ = return (Just t)

textTok::(String->Token)->LexAction
textTok cons _ s = return $ Just (cons s)

beginString::LexAction
beginString _ _ = do
  s <- get
  put s{lexSC = stringSC}
  return Nothing

appendString::LexAction
appendString _ (c:_) = do
  s <- get
  put s{stringBuf = c:(stringBuf s)}
  return Nothing

escapeString::LexAction
escapeString _ (_:c:_) = do
  let unesc =
        case c of
          'n' -> '\n'
          't' -> '\t'
          '"' -> '"'
  s <- get
  put s{stringBuf = unesc:(stringBuf s)}
  return Nothing
        
endString::LexAction
endString _ _ = do
  s <- get
  let buf = stringBuf s
  put s{lexSC = 0, stringBuf = ""}
  return $ Just $ TString (reverse buf)
  
beginComment::LexAction
beginComment _ _ = do
  s <- get
  put s {lexSC = commentSC,
         commentDepth = (commentDepth s)+1}
  return Nothing

endComment _ _ = do
  s <- get
  let cd = commentDepth s
  let sc' = if cd==1 then 0 else commentSC
  put s {lexSC=sc',commentDepth=cd-1}
  return Nothing
  
readToken::P Token
readToken = do
  s <- get
  case alexScan (input s) (lexSC s) of
    AlexEOF -> return TEOF
    AlexError inp' -> error $ "Lexical error on line "++(show $ ailineno inp')      
    AlexSkip inp' _ -> do    
      put s{input = inp'}
      readToken
    AlexToken inp' n act -> do 
      let (AlexInput{airest=buf}) = input s
      put s{input = inp'}
      res <- act n (take n buf)
      case res of
        Nothing -> readToken
        Just t -> return t

lexer::(Token->P a)->P a
lexer cont = do
  tok <- readToken
  cont tok
}
