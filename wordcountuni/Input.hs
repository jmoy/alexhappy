module Input where
import Codec.Binary.UTF8.String as U8
import Data.Word
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.ByteString.Lazy as B

type AlexInput = B.ByteString

strToInp::B.ByteString->AlexInput
strToInp  = id

inpToStr::AlexInput->B.ByteString
inpToStr  = id

makeToken::B.ByteString -> B.ByteString
makeToken = B.copy

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte  = B.uncons

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar _ = undefined

