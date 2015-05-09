{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
module Salsa20 where {
import SeqIterate;
import Util;
import Data.Typeable(Typeable,cast);
import Data.Bits(rotate,xor,Bits);
import Data.List hiding (length, replicate,(!!));
import Control.Exception(assert);
import Data.Word;
import Prelude hiding(length, replicate,(!!));

-- import Debug.Trace;

round_func :: (Bits a, Num a) => Rotation -> a -> a -> a -> a;
round_func (Rotation k) a b c = xor c $ rotate (b + a) k;

newtype Arity = Arity Integer deriving (Show);
unArity :: Arity -> Integer;
unArity (Arity n) = n;

{-
a b c 7 c:=1
b 1 d 9 d:=2
1 2 e 13 e:=3
2 3 f 18 f:=4
3 4 g ?
-}
-- arity=2 for salsa20, the number of elements above the current position it depends on.
-- it always depends on the current position, so actual arity of r is arity+1;
do_column :: forall a b . Arity -> (b -> [a] -> a) -> [b] -> [a] -> [a];
do_column (Arity arity) f shifts l0 = let
{ l :: [a]
; l = cycle l0
; answer :: [a]
-- cycle shifts is for generalization of salsa20 on bigger matrices, yet still keeping the 4 rotation amounts.  Just repeat the rotations as necessary: far from clear this is a safe thing to do.
; answer = zipWith f (cycle shifts) $ transpose $ (genericDrop arity l:) $ genericTake arity $ tails $ genericTake arity l ++ answer;
} in answer;

salsa20_arity :: Arity;
salsa20_arity = Arity 2;

quarter_round :: (Num a, Typeable a, Bits a) => [a] -> [a];
quarter_round input = list_rotate (negate $ unArity salsa20_arity) $ take_same_length input $ do_column salsa20_arity r_as_list (map Rotation [7,9,13,18]) input;

shift_columns :: [[a]] -> [[a]];
shift_columns = zipWith list_rotate (enumFrom $ negate 1);

unshift_columns :: [[a]] -> [[a]];
unshift_columns = zipWith list_rotate $ enumFromThen 1 0;

r_as_list :: forall a . (Typeable a, Bits a, Num a) => Rotation -> [a] -> a;
-- this is how to trace a polymorphic function
r_as_list k (l@[c,a,b]) = no_trace (case cast l of {
Nothing -> "not word";
Just (ww::[W]) -> "(" ++ show k ++ ",[" ++ (unwords $ map whex ww) ++ "])"}) $ round_func k a b c;
r_as_list _ _ = error "wrong arity";

-- map whex $ fourfunc 2 r_as_list (map Rotation [7,9,13,18]) [0x18171615,0x61707865::W,0x100f0e0d,0x7]
-- ["d3c83331","71572c6d","f3e4deb6","4dfdec95"]
-- agrees with salsafamily paper

no_trace :: String -> a -> a;
no_trace = flip const;

take_same_length :: [a] -> [b] -> [b];
take_same_length [] _ = [];
take_same_length (_:r1) (h:r2) = h:take_same_length r1 r2;
take_same_length _ _ = error "take_same_length: second list too short";

example_key :: [W];
example_key = map code4bytes $ to_matrix (Matrix_width 4) $ enumFromTo 1 32;

start_string :: [W];
start_string = let {
 d :: Integer -> [W];
 d i = [genericIndex salsa20_diagonal i]
} in d 0 ++ take 4 example_key ++ d 1 ++ [0x01040103,0x06020905,7,0] ++ d 2 ++ drop 4 example_key ++ d 3;

-- we transpose first because we prefer to work with rows rather than columns
one_round :: (Typeable a, Num a, Bits a) => [[a]] -> [[a]];
one_round = unshift_columns . map quarter_round . shift_columns . transpose;

newtype Rounds = Rounds Integer deriving (Show);

core :: (Typeable a, Num a, Bits a, NFData a) => Rounds -> [[a]] -> [[a]];
core (Rounds n) = ((flip genericIndex) n) . (seqIterate one_round);

salsa20_test :: Rounds -> [W] -> IO();
salsa20_test num_rounds s = mapM_ putStrLn $ map (unwords . map whex) $ core num_rounds $ to_matrix (Matrix_width 4) s;

{-
ckkk
kcnn
nnck
kkkc
-}
hsalsa_setup :: [Word8] -> [Word8] -> [W];
hsalsa_setup key nonce = key_iv_setup (u8_to_32_little key) (u8_to_32_little nonce);

hsalsa_subkey :: [[W]] -> [W];
hsalsa_subkey x = map (genericIndex $ concat x) [0::Integer,5,10,15,6,7,8,9];

hsalsa :: [Word8] -> [Word8] -> [W];
hsalsa key = hsalsa_subkey . core (Rounds 20) . to_matrix (Matrix_width 4) . hsalsa_setup key;

key_iv_setup :: [W] -> [W] -> [W];
key_iv_setup key iv =
let {
(left :: [W], right :: [W]) = splitAt 4 $ key;
 d :: Integer -> [W];
 d i = [genericIndex salsa20_diagonal i];}
in assert ((256::Integer) == 32* genericLength key)
$ assert ((4::Integer) == genericLength right)
$ assert ((4::Integer) == genericLength left)
$ assert ((128::Integer) == 32* genericLength iv)
$ d 0
++ left
++ d 1
++ iv
++ d 2
++ right
++ d 3;

and_add :: (Num a) => ([a] -> [a]) -> [a] -> [a];
and_add f x = zipWith (+) x $ f x;

with_add :: (Typeable a, Num a, Bits a, NFData a) => Rounds -> [a] -> [a];
with_add rounds = and_add $ concat . core rounds . to_matrix (Matrix_width 4);

encode_counter :: [W] -> [W] -> Integer -> [W];
encode_counter key iv counter = assert (counter>=0)
$ assert (counter < 2^(64::Integer))
$ key_iv_setup key $ iv ++ let { (q,r) = divMod counter $ 2^(32::Integer) } in map fromIntegral [r,q];

salsa20w :: Rounds -> [W] -> [W] -> [[W]];
salsa20w rounds key iv = map (with_add rounds . encode_counter key iv) [0..];

xsalsa_w :: Rounds -> [Word8] -> [Word8] -> [[W]];
xsalsa_w rounds key iv = let
{ (iv1,iv2) = splitAt 16 iv; }
in salsa20w rounds (hsalsa key iv1) (u8_to_32_little iv2);

block_bytes :: [W] -> [Word8];
block_bytes = concatMap u32le;

xsalsa :: [Word8] -> [Word8] -> [Word8];
xsalsa key iv = concatMap block_bytes $ xsalsa_w (Rounds 20) key iv;


}
