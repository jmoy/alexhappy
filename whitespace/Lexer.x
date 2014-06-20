{
module Main (main) where
import Control.Monad.State
import Control.Monad
import Data.Word
import Codec.Binary.UTF8.String (encode)
}

tokens :-
       \n$white*               {startWhite}
       $white+		       ;
       [a-z0-9_]+               {name}


{
data Token 
     = TIndent
     | TDedent
     | TNewline
     | TName String
     | TEOF
     deriving (Eq,Show)

-- The functions that must be provided to Alex's basic interface
-- The input: last character, unused bytes, remaining string
data AlexInput = AlexInput Char [Word8] String
     deriving Show
alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (AlexInput c (b:bs) s) = Just (b,AlexInput c bs s)
alexGetByte (AlexInput c [] [])    = Nothing
alexGetByte (AlexInput _ [] (c:s)) = case encode [c] of
                             	   	(b:bs) -> Just (b, AlexInput c bs s)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AlexInput c _ _) = c

-- Our state

data ParseState = 
     ParseState {input::AlexInput,
                 indent_stack::[Int],
                 pending_tokens::[Token]}
                 deriving Show

initialState::String -> ParseState
initialState s = ParseState {   input = AlexInput '\n' [] s,
                                indent_stack = [1],
                                pending_tokens = []
                                }

-- Our Parser monad
type P a = State ParseState a

evalP::P a -> String -> a
evalP m s= evalState m (initialState s)

-- Set input

startWhite::Int->String->P Token
startWhite n _ = do
	   s<-get
           let is@(cur:_) = indent_stack s
           when (n>cur) $ do
              put s{indent_stack = n:is,pending_tokens = [TIndent]}
           when (n<cur)  $ do
              let (pre,post@(top:_)) = span (> n) is
              if top == n then
                 put s{indent_stack = post,
                                    pending_tokens = map (const TDedent) pre}
              else
                 error "Indents don't match"
           return TNewline

name::n->String->P Token
name _ s = return (TName s)

-- Action to read a token
readToken::P Token
readToken = do
          s <- get
          case pending_tokens s of
               t:ts -> do
			put s{pending_tokens = ts}
			return t  
               [] ->  case alexScan (input s) 0 of
                       AlexEOF -> do
                                    rval <- startWhite 1 ""
                                    put s{pending_tokens=(pending_tokens s)++[TEOF]}
                                    return rval
                       AlexError _ -> error "!Lexical error"
                       AlexSkip inp' _ -> do    
                          put s{input = inp'}
                          readToken
                       AlexToken inp' n act -> do 
                          let (AlexInput _ _ buf) = input s
                          put s{input = inp'}
                          act n (take n buf)

readtoks::P [Token]
readtoks = do
            t<-readToken
            case t of
              TEOF -> return [t]
              _ -> do 
                rest<- readtoks
                return (t:rest)

tokenize::String->[Token]
tokenize s = 
        evalP readtoks s 

main::IO()
main = do
     input <- getContents
     print (tokenize input)
     
}    
