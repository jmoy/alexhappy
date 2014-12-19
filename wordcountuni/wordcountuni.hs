module Main(main) where
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Input
import Lexer

tokenize::T.Text->[T.Text]
tokenize str = go $ strToInp str
  where go inp =
          case alexScan inp 0 of
                AlexEOF -> []
                AlexError _ -> error "lexical error"
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act (T.take
                                               (fromIntegral len)
                                               (inpToStr inp))
                                          : go inp'

main::IO ()
main = do
  s <- TIO.getContents
  let toks = tokenize s
  mapM_ TIO.putStrLn toks
