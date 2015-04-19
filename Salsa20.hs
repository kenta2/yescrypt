{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
module Salsa20 where {
import SeqIterate;
import Util;
import Data.Typeable(Typeable,cast);
import Data.Bits(rotate,xor,Bits);
import Data.List;

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

diagonal_phrase :: String;
diagonal_phrase = "expand 32-byte k";

salsa20_diagonal :: [W];
salsa20_diagonal = map code4bytes $ to_matrix (Matrix_width 4) $ map (fromIntegral . fromEnum) diagonal_phrase;

code4bytes :: [W] -> W;
code4bytes = foldr (\b old -> old * 256 +b) 0;

example_key :: [W];
example_key = map code4bytes $ to_matrix (Matrix_width 4) $ enumFromTo 1 32;

start_string :: [W];
start_string = let {
d i = [salsa20_diagonal !! i]
} in d 0 ++ take 4 example_key ++ d 1 ++ [0x01040103,0x06020905,7,0] ++ d 2 ++ drop 4 example_key ++ d 3;

-- we transpose first because we prefer to work with rows rather than columns
one_round :: (Typeable a, Num a, Bits a) => [[a]] -> [[a]];
one_round = unshift_columns . map quarter_round . shift_columns . transpose;

core :: (Typeable a, Num a, Bits a, NFData a) => Double_rounds -> [[a]] -> [[a]];
core (Double_rounds n) = ((flip genericIndex) n) . (seqIterate one_round);

salsa20_test :: Double_rounds -> IO();
salsa20_test num_rounds = mapM_ putStrLn $ map (unwords . map whex) $ core num_rounds $ to_matrix (Matrix_width 4) start_string;

}
