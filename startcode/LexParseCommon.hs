module LexParseCommon where
       
import Control.Monad.State
import Control.Monad
import Data.Word
import Codec.Binary.UTF8.String (encode)

data Token 
     = TCat
     | TEOF
     | TString String
     deriving (Eq,Show)

-- The functions that must be provided to Alex's basic interface
-- The input: last character, unused bytes, remaining string
data AlexInput
  = AlexInput {
    aiprev::Char,
    aibytes::[Word8],
    airest::String,
    ailineno::Int}
  deriving Show
           
alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte ai
  = case (aibytes ai) of
    (b:bs) -> Just (b,ai{aibytes=bs})
    [] -> case (airest ai) of
      [] -> Nothing
      (c:cs) -> let n = (ailineno ai)
                    n' = if c=='\n' then n+1 else n
                    (b:bs) = encode [c] in
                Just (b,AlexInput {aiprev=c,
                                   aibytes=bs,
                                   airest=cs,
                                   ailineno=n'})
                
                    
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AlexInput {aiprev=c}) = c

data ParseState = 
     ParseState {input::AlexInput,
                 lexSC::Int,       --Lexer start code
                 commentDepth::Int,--Comment depth
                 stringBuf::String --Temporary storage for strings
                }
     deriving Show

initialState::String -> ParseState
initialState s = ParseState {   input = AlexInput {aiprev='\n',
                                                   aibytes=[],
                                                   airest=s,
                                                   ailineno = 1},
                                lexSC = 0,
                                commentDepth = 0,
                                stringBuf = ""
                                }

-- Our Parser monad
type P a = State ParseState a

getLineNo::P Int
getLineNo = do
  s <- get
  return . ailineno . input $ s

evalP::P a -> String -> a
evalP m s= evalState m (initialState s)

