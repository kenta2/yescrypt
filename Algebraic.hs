{-# LANGUAGE ScopedTypeVariables #-}
-- enough rope to evaluate Salsa20 algebraically
module Algebraic where {
import Data.Bits(Bits(..));
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


}
