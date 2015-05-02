{-# LANGUAGE BangPatterns #-}
module Main(main) where
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.Lazy.Encoding as TE
import Input
import Lexer

tokenize::B.ByteString->[B.ByteString]
tokenize str = go str

go !inp =
  case alexScan inp 0 of
    AlexEOF -> []
    AlexError _ -> error "lexical error"
    AlexSkip  inp' len     -> go inp'
    AlexToken inp' len act ->
      let n = B.length inp - B.length inp'
          !bs = B.take (fromIntegral len) inp
          !tok = act bs in
      tok:go inp'

main::IO ()
main = do
  s <- B.getContents
  let toks = tokenize s
  mapM_ B.putStrLn toks
