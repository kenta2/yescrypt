{-# LANGUAGE ScopedTypeVariables, LambdaCase, AllowAmbiguousTypes #-}
module Salsa20 where {
import Control.Exception(assert);
import Data.Typeable(Typeable,cast);
import Data.Word;
import Data.Bits(rotate,xor,Bits);
import Data.Array.ST(STArray);
import Data.Array.IArray;
import Data.List;


-- import Debug.Trace;
import Text.Printf;

newtype Rotation = Rotation Int deriving (Show);

type W = Word32;

round_func :: (Bits a, Num a) => Rotation -> a -> a -> a -> a;
round_func (Rotation k) a b c = xor c $ rotate (b + a) k;

type Coord = (Integer,Integer);

type Concrete s = STArray s Coord W;

newtype Arity = Arity Integer deriving (Show);
unArity :: Arity -> Integer;
unArity (Arity n) = n;

flip_tuple :: (a,b) -> (b,a);
flip_tuple (x,y) = (y,x);

transpose_array :: forall a i1 i2 e . (Ix i1, Ix i2, IArray a e) => a (i1,i2) e -> a (i2,i1) e;
transpose_array old = let
{ bounds_new :: ((i2,i1),(i2,i1))
; bounds_new = (flip_tuple $ fst $ bounds old, flip_tuple $ snd $ bounds old)
; f :: (i2, i1) -> e
; f x = old ! (flip_tuple x); -- ! is already bounds checked
} in array bounds_new $ zip_map f $ range bounds_new;

zip_map :: (a -> b) -> [a] -> [(a,b)];
zip_map f l = zip l $ map f l;

list_rotate :: Integer -> [a] -> [a];
list_rotate n l = if n>=0
then genericDrop n l ++ genericTake n l
else list_rotate (genericLength l + n) l;

{-
a b c 7 c:=1
b 1 d 9 d:=2
1 2 e 13 e:=3
2 3 f 18 f:=4
3 4 g ?
-}
-- arity=2 for salsa20, the number of elements above the current position it depends on.
-- it always depends on the current position, so actual arity of r is arity+1;
fourfunc :: forall a b . Arity -> (b -> [a] -> a) -> [b] -> [a] -> [a];
fourfunc (Arity arity) f shifts l0 = let
{ l :: [a]
; l = cycle l0
; answer :: [a]
-- cycle shifts is for generalization of salsa20 on bigger matrices, yet still keeping the 4 rotation amounts.  Just repeat the rotations as necessary: far from clear this is a safe thing to do.
; answer = zipWith f (cycle shifts) $ transpose $ (genericDrop arity l:) $ genericTake arity $ tails $ genericTake arity l ++ answer;
} in answer;

salsa20_arity :: Arity;
salsa20_arity = Arity 2;

column :: (Num a, Typeable a, Bits a) => [a] -> [a];
column input = list_rotate (negate $ unArity salsa20_arity) $ take_same_length input $ fourfunc salsa20_arity r_as_list (map Rotation [7,9,13,18]) input;

order1 :: [Integer];
order1 = [12,0,4,8];

shift_columns :: [[a]] -> [[a]];
-- shift_columns [] = error "empty shift_columns";
-- shift_columns (x:rest) = list_rotate (pred $ genericLength x) x : zipWith list_rotate (enumFrom 0) rest;
shift_columns = zipWith list_rotate (enumFrom $ negate 1);

unshift_columns :: [[a]] -> [[a]];
unshift_columns = zipWith list_rotate $ enumFromThen 1 0;

half :: (Num a, Typeable a, Bits a) => [[a]] -> [[a]];
half = unshift_columns . map column . shift_columns . transpose;

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

whex :: W -> String;
whex x = printf "%x" x;

take_same_length :: [a] -> [b] -> [b];
take_same_length [] _ = [];
take_same_length (_:r1) (h:r2) = h:take_same_length r1 r2;
take_same_length _ _ = error "take_same_length: second list too short";

others :: [a] -> [(a,[a])];
others [] = error "empty";
others [x] = [(x,[])];
others (h:rest) = (h,rest):do {
(p,q) <- others rest;
return (p, h:q);
};

to_matrix :: Integer -> [a] -> [[a]];
to_matrix n = unfoldr (\l -> case l of {[] -> Nothing; _ -> let {r = genericSplitAt n l} in assert (n == (genericLength $ fst r)) $ Just r});

salsa20_diagonal :: [W];
salsa20_diagonal = [0x61707865, 0x3320646e, 0x79622d32, 0x6b206574];

int_matrix :: [[Integer]];
int_matrix = to_matrix 4 [0..15];

code4bytes :: [W] -> W;
code4bytes = foldr (\b old -> old * 256 +b) 0;

example_key :: [W];
example_key = map code4bytes $ to_matrix 4 $ enumFromTo 1 32;

start_string :: [W];
start_string = let {
d i = [salsa20_diagonal !! i]
} in d 0 ++ take 4 example_key ++ d 1 ++ [0x01040103,0x06020905,7,0] ++ d 2 ++ drop 4 example_key ++ d 3;

one_round :: (Typeable a, Num a, Bits a) => [[a]] -> [[a]];
one_round = unshift_columns . map column . shift_columns .transpose;

n_rounds :: (Typeable a, Num a, Bits a) => Integer -> [[a]] -> [[a]];
n_rounds n = ((flip genericIndex) n) . (iterate one_round);

salsa20_test :: Integer -> IO();
salsa20_test num_rounds = mapM_ putStrLn $ map (unwords . map whex) $ n_rounds num_rounds $ to_matrix 4 start_string;

}
