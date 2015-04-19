module Util where {
import Data.Word;
import Data.Binary.Put(runPut,putWord32le);
import qualified Data.ByteString.Lazy as Lazy;
import Text.Printf;
u32le :: Word32 -> [Word8];
u32le = Lazy.unpack . runPut . putWord32le ;

hex_byte :: Word8 -> String;
hex_byte = printf "%02x";

}
