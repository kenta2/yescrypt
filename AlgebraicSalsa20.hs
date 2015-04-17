{-# LANGUAGE ScopedTypeVariables #-}
-- enough rope to evaluate Salsa20 algebraically
module AlgebraicSalsa20 where {
import Data.Bits(Bits(..));
import Data.List(sortBy,transpose);
import Data.Ord(comparing);
import Salsa20;
data Algebraic a = Atom a | Xor (Algebraic a) (Algebraic a) | Rotate (Algebraic a) Int | Add (Algebraic a) (Algebraic a) deriving (Show,Eq);

instance Num (Algebraic a) where {
(+) x y = Add x y;
(*) = error "Algebraic *";
abs = error "Algebraic abs";
signum = error "Algebraic signum";
negate = error "Algebraic negate";
fromInteger = error "Algebraic fromInteger";
};

instance (Eq a) => Bits (Algebraic a) where {
xor x y = Xor x y;
rotate x k = Rotate x k;
(.&.) = error "Algebraic &";
(.|.) = error "Algebraic |";
complement = error "Algebraic complement";
shift = error "Algebraic shift";
bitSize = error "Algebraic bitSize";
bitSizeMaybe = error "Algebraic bitSizeMaybe";
isSigned = error "Algebraic isSigned";
testBit = error "Algebraic testBit";
bit = error "Algebraic bit";
popCount = error "Algebraic popCount";
};

simplify :: (Eq a) => (a,Algebraic a) -> (Algebraic a) -> (Algebraic a);
simplify v@(n,small) large = if small == large then Atom n
else case large of {
Atom _ -> large;
Xor x y -> Xor (simplify v x) (simplify v y);
Rotate x i -> Rotate (simplify v x) i;
Add x y -> Add (simplify v x) (simplify v y);
};

size :: Algebraic a -> Integer;
size (Atom _) = 1;
size (Xor x y) = 1 + size x + size y;
size (Add x y) = 1 + size x + size y;
size (Rotate x _) = 1 + size x;

simplify_in_order :: [(Algebraic (Integer,Bool),Integer)] -> [(Algebraic (Integer,Bool),Integer)];
simplify_in_order [] = [];
simplify_in_order ((e,n):rest) = (e,n):(simplify_in_order $ map_fst (simplify ((n,True), e)) rest);

map_fst :: (a -> b) -> [(a,c)] -> [(b,c)];
map_fst f l = do {
x <- l;
return (f $ fst x, snd x);
};

do_by_size :: forall a b sortable . (Ord sortable) => ([a] -> [b]) -> (a -> sortable) -> [a] -> [b];
do_by_size f getsize l = let {
 sz_and :: [(a, Integer)];
 sz_and = sortBy (comparing $ getsize . fst) $ zip l $ enumFrom 0;
} in map snd $ sortBy (comparing fst) $ zip (map snd sz_and) (f $ map fst sz_and);

simplify_by_size :: [(Algebraic (Integer,Bool), Integer)] -> [(Algebraic (Integer,Bool),Integer)];
simplify_by_size = do_by_size simplify_in_order (size . fst);

algebraic_show :: (Algebraic (Integer,a)) -> String;
algebraic_show (Atom (i,_)) = "x[" ++ show i ++ "]";
algebraic_show (Add x y) = "(" ++ algebraic_show x ++ " + " ++ algebraic_show y ++ ")";
algebraic_show (Xor x y) = algebraic_show x ++ " ^ " ++ algebraic_show y;
algebraic_show (Rotate x k) = "(" ++ algebraic_show x ++ " <<< " ++ show k ++ ")";

atest :: [Integer] -> [(Algebraic (Integer,Bool),Integer)];
atest order = zip (column $ do { n <- order; return $ Atom (n,False)}) order;
-- list_rotate 2 is because the output lags by 2 (arity) from the input.

test_shift_columns :: [[Algebraic Integer]];
test_shift_columns = shift_columns $ transpose $ to_matrix 4 $ map Atom [0..15];

atest_out :: [Integer] -> [(Algebraic (Integer,Bool),Integer)];
atest_out = simplify_in_order . (list_rotate $ unArity salsa20_arity) . atest;

output_c :: ((Algebraic (Integer, a)), Integer) -> String;
output_c (e,i) = "x[" ++ show i ++ "] := " ++ algebraic_show e;

c_code_out :: [String];
c_code_out = map output_c $ concatMap atest_out $ (shift_columns $ transpose int_matrix) ++ shift_columns int_matrix;

}
