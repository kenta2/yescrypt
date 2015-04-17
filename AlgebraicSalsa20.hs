{-# LANGUAGE ScopedTypeVariables #-}
-- enough rope to evaluate Salsa20 algebraically
module AlgebraicSalsa20 where {
import Data.Bits(Bits(..));
import Data.List(sortBy);
import Data.Ord(comparing);
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

}
