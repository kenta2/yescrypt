{-# LANGUAGE ScopedTypeVariables, LambdaCase, GeneralizedNewtypeDeriving #-}
module Main where {
import Data.List;
import Data.Bits(xor,(.&.));
import qualified Data.ByteString.Lazy as Lazy;
import qualified Data.Binary as Binary;
import Data.Binary(Binary);
import Data.Binary.Get(runGet);
import qualified Data.Binary.Get as Binary.Get;
import Control.Exception(assert);
import Data.Vector(Vector);
import qualified Data.Vector as Vector;
import Data.Word;
import qualified Data.ByteString as ByteString;
import Data.ByteString(ByteString);
import Data.ByteString.Base16 as Base16;
-- import Crypto.PBKDF.ByteString(sha256PBKDF2);
-- import qualified Crypto.Hash.SHA256 as SHA256;

to_hex :: ByteString -> String;
to_hex =  map (toEnum . fromEnum) . ByteString.unpack . Base16.encode;

newtype Pwx_simple = Pwx_simple Integer deriving (Show);
newtype Pwx_gather = Pwx_gather Integer deriving (Show);
newtype Pwx_rounds = Pwx_rounds Integer deriving (Show);
newtype Swidth = Swidth Integer deriving (Show);
newtype Btype = Btype Word64 deriving (Show);
unB :: Btype -> Word64;
unB (Btype x) = x;

newtype Stype = Stype Word64;
type Sbox = Vector [Stype];

-- for(i=start;i<=end;i++){acc=f(acc,i)}
counting_fold :: (Enum a, Eq a) => a -> a -> acc -> (acc -> a -> acc) -> acc;
counting_fold start end acc0 f = if start == end then acc0
else counting_fold (succ start) end (f acc0 start) f;

pwxform :: Pwx_simple -> Pwx_gather -> Pwx_rounds -> Swidth -> Sbox -> [[Btype]] -> [[Btype]];
pwxform simple gather (Pwx_rounds rounds) swidth sbox b = (flip genericIndex) rounds $ (flip iterate) b $ pwx1 simple gather swidth sbox;

pwx1 :: Pwx_simple -> Pwx_gather -> Swidth -> Sbox -> [[Btype]] -> [[Btype]];
pwx1 simple (Pwx_gather gather) swidth sbox b = assert (gather == genericLength b)
$ map (pwx2 simple swidth sbox) b;

-- need to figure out relationship between sizeof Sbox and Pwx_simple and smask

pwx2 :: Pwx_simple -> Swidth -> Sbox -> [Btype] -> [Btype];
pwx2 simple (Swidth swidth) sbox b = let {
 sim8 :: Integer;
 sim8 = case simple of {Pwx_simple x -> x *8};
 smask :: Word32;
 smask = safeFromIntegral $ (2^swidth-1)*sim8;
 p :: [Word32];
 p = map (\bj0 -> case divMod ((.&.) bj0 smask) (safeFromIntegral sim8) of {
  (q,0) -> q;
  _ -> error "p did not divide cleanly";}) $ split64 $ unB $ head b;  -- the first element of each row is special, used to look up which sbox to use.
 s :: [[Stype]];
 s = map (vector_genericIndex sbox) p;
} in zipWith3 f3 b (s!!0) (s!!1);

vector_genericIndex :: (Integral index) => Vector a -> index -> a;
vector_genericIndex v i = v Vector.! (safeFromIntegral i);

type Word_vec = Vector Word64;

to_word_vec :: ByteString -> Word_vec;
to_word_vec b = assert (mod (ByteString.length b) 8 ==0)
$ Vector.fromList $ runGet get_many_until_empty $ Lazy.fromStrict b;

-- A seemingly useful function for Binary.Get but unfortunately of
-- limited use, because it does not parse lazily, i.e. does not return
-- a lazy list; it must consume the entire input before returning the
-- first item.
get_many_until_empty :: forall a. Binary a => Binary.Get [a];
get_many_until_empty = Binary.Get.isEmpty >>= \case {
True -> return [];
False -> do {
x <- Binary.get;
rest :: [a] <- get_many_until_empty;
return $ x:rest;
}};

raise_dimension :: Integer -> Vector a -> Vector (Vector a);
raise_dimension n v = assert (mod (fromIntegral $ Vector.length v) n ==0)
$ Vector.unfoldr (\small -> if Vector.length small ==0 then Nothing else Just $ Vector.splitAt (fromIntegral n) small) v;

-- this curious primitive is at the heart of pwxform
qr_multiply :: Integer -> Integer -> Integer;
qr_multiply modulus x = let {
(q,r) = divMod x modulus
} in q*r;

qr_multiply_word64 :: Word64 -> Word64;
qr_multiply_word64 = safeFromIntegral . qr_multiply two32 . fromIntegral;

f3 :: Btype -> Stype -> Stype -> Btype;
f3 (Btype b) (Stype s0) (Stype s1) = Btype $ xor (qr_multiply_word64 b + s0) s1;

split64 :: Word64 -> [Word32];
split64 x = case divMod x $ safeFromIntegral two32 of {
(q,r) -> [safeFromIntegral r,safeFromIntegral q];
};

two32 :: Integer;
two32 = 2^(32::Integer);

safeFromIntegral :: forall a b . (Integral a, Integral b, Bounded b) => a -> b;
safeFromIntegral x = let { ix :: Integer ; ix = fromIntegral x} in if (ix < (fromIntegral (minBound::b))) || (ix > (fromIntegral (maxBound::b)))
then error "safeFromIntegral: out of range"
else fromIntegral x;

main :: IO (); main = undefined;
} --end
