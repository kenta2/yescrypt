{-# LANGUAGE LambdaCase #-}
module Util where {
import Data.List;
-- import Control.Exception(assert);
import qualified UserError as User;
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
list_rotate _ [] = [];
list_rotate n l = let {
m = mod n (genericLength l); -- NB mod not rem is important for negative
} in genericDrop m l ++ genericTake m l;

newtype Matrix_width = Matrix_width Integer deriving (Show);

to_matrix :: Matrix_width -> [a] -> [[a]];
to_matrix (Matrix_width n) = unfoldr (\case {[] -> Nothing; l -> let {r = genericSplitAt n l} in User.assert "to_matrix" (n == (genericLength $ fst r)) $ Just r});

whex :: W -> String;
whex x = printf "%08x" x;

int_matrix :: [[Integer]];
int_matrix = mat4 [0..15];

diagonal_phrase :: String;
diagonal_phrase = "expand 32-byte k";

mat4 :: [a] -> [[a]];
mat4 = to_matrix (Matrix_width 4);

salsa20_diagonal :: [W];
salsa20_diagonal = u8_to_32_little $ map fromEnum diagonal_phrase;

code4bytes :: [W] -> W;
code4bytes = foldr (\b old -> old * 256 +b) 0;

u8_to_32_little :: Integral a => [a] -> [W];
u8_to_32_little = map code4bytes . mat4 . map fromIntegral;

block_bytes :: [W] -> [Word8];
block_bytes = concatMap u32le;

and_add :: (Num a) => ([a] -> [a]) -> [a] -> [a];
and_add f x = zipWith (+) x $ f x;

}
