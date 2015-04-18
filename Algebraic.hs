{-# LANGUAGE ScopedTypeVariables #-}
-- enough rope to evaluate Salsa20 algebraically
module Algebraic where {
import Data.Maybe(fromJust);
import Data.Bits(Bits(..));
data Algebraic a = Atom a | Xor (Algebraic a) (Algebraic a) | Rotate Int (Algebraic a) | Add (Algebraic a) (Algebraic a) deriving (Show,Eq);

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
rotate x k = Rotate k x;
(.&.) = error "Algebraic &";
(.|.) = error "Algebraic |";
complement = error "Algebraic complement";
shift = error "Algebraic shift";
bitSize = fromJust . bitSizeMaybe;
bitSizeMaybe = Just . fromIntegral . size;
isSigned = error "Algebraic isSigned";
testBit = error "Algebraic testBit";
bit = error "Algebraic bit";
popCount = error "Algebraic popCount";
};

size :: Algebraic a -> Integer;
size (Atom _) = 1;
size (Xor x y) = 1 + size x + size y;
size (Add x y) = 1 + size x + size y;
size (Rotate _ x) = 1 + size x;

simplify :: (Eq a) => (a,Algebraic a) -> (Algebraic a) -> (Algebraic a);
simplify v@(n,small) large = if small == large then Atom n
else case large of {
Atom _ -> large;
Xor x y -> Xor (simplify v x) (simplify v y);
Rotate i x -> Rotate i (simplify v x);
Add x y -> Add (simplify v x) (simplify v y);
};


}
