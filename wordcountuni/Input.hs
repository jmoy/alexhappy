module Input where
import Codec.Binary.UTF8.String as U8
import Data.Word
import qualified Data.Text.Lazy as T


type AlexInput = ([Word8],   -- rest of the bytes for the current char
                  T.Text)    -- rest of the input string

strToInp::T.Text->AlexInput
strToInp s = ([],s)

inpToStr::AlexInput->T.Text
inpToStr (_,s) = s

makeToken::T.Text -> T.Text
makeToken = id

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte ((b:bs),s)  = Just (b, (bs, s))
alexGetByte ([],s) =
  case T.uncons s of
    Nothing -> Nothing
    Just (c,s') -> case U8.encodeChar c of
      (b:bs) -> Just (b, (bs, s'))

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar _ = undefined

