{-# LANGUAGE LambdaCase #-}
module Util where {
import Data.List;
import Control.Exception(assert);
import Data.Word;
import Data.Binary.Put(runPut,putWord32le);
import qualified Data.ByteString.Lazy as Lazy;
import Text.Printf;
u32le :: Word32 -> [Word8];
u32le = Lazy.unpack . runPut . putWord32le ;

hex_byte :: Word8 -> String;
hex_byte = printf "%02x";

newtype Rotation = Rotation Int deriving (Show);
type W = Word32;

list_rotate :: Integer -> [a] -> [a];
list_rotate n l = if n>=0
then genericDrop n l ++ genericTake n l
else list_rotate (genericLength l + n) l;

to_matrix :: Integer -> [a] -> [[a]];
to_matrix n = unfoldr (\case {[] -> Nothing; l -> let {r = genericSplitAt n l} in assert (n == (genericLength $ fst r)) $ Just r});

whex :: W -> String;
whex x = printf "%08x" x;

int_matrix :: [[Integer]];
int_matrix = to_matrix 4 [0..15];

}
