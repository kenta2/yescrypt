{-# LANGUAGE ScopedTypeVariables, LambdaCase, AllowAmbiguousTypes #-}
module Salsa20 where {
import Control.Exception(assert);
import Data.Typeable(Typeable,cast);
import Data.Word;
import Data.Bits(rotate,xor,Bits);
import Data.Array.ST(STArray);
import Data.Array.IArray;
import Data.List;
import AlgebraicSalsa20;

-- import Debug.Trace;
import Text.Printf;

newtype Rotation = Rotation Int deriving (Show);

type W = Word32;

round_func :: (Bits a, Num a) => Rotation -> a -> a -> a -> a;
round_func (Rotation k) a b c = xor c $ rotate (a + b) k;

type Coord = (Integer,Integer);

type Concrete s = STArray s Coord W;


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
list_rotate n l = genericDrop n l ++ genericTake n l;

{-
a b c 7 c:=1
b 1 d 9 d:=2
1 2 e 13 e:=3
2 3 f 18 f:=4
3 4 g ?
-}
-- arity=2 for salsa20, the number of elements above the current position it depends on.
-- it always depends on the current position, so actual arity of r is arity+1;
fourfunc :: forall a b . Integer -> (b -> [a] -> a) -> [b] -> [a] -> [a];
fourfunc arity f shifts l0 = let
{ l :: [a]
; l = cycle l0
; answer :: [a]
; answer = zipWith f shifts $ transpose $ (genericDrop arity l:) $ genericTake arity $ tails $ genericTake arity l ++ answer;
} in answer;

column :: (Num a, Typeable a, Bits a) => [a] -> [a];
column = fourfunc 2 r_as_list (map Rotation [7,9,13,18]);

order :: [Integer];
order = [4,8,12,0];
atest :: [(Algebraic (Integer,Bool),Integer)];
atest = zip (column $ do { n <- (list_rotate 2 order); return $ Atom (n,False)}) order;

shift_columns :: [[a]] -> [[a]];
-- shift_columns [] = error "empty shift_columns";
-- shift_columns (x:rest) = list_rotate (pred $ genericLength x) x : zipWith list_rotate (enumFrom 0) rest;
shift_columns = zipWith list_rotate (enumFrom 1);

test_shift_columns :: [[Algebraic Integer]];
test_shift_columns = shift_columns $ transpose $ to_matrix 4 $ map Atom [0..15];

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


atest_out :: [(Algebraic (Integer,Bool),Integer)];
atest_out = simplify_in_order atest;

others :: [a] -> [(a,[a])];
others [] = error "empty";
others [x] = [(x,[])];
others (h:rest) = (h,rest):do {
(p,q) <- others rest;
return (p, h:q);
};

to_matrix :: Integer -> [a] -> [[a]];
to_matrix n = unfoldr (\l -> case l of {[] -> Nothing; _ -> let {r = genericSplitAt n l} in assert (n == (genericLength $ fst r)) $ Just r});



}
