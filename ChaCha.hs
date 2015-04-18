{-# LANGUAGE ScopedTypeVariables #-}
module ChaCha where {
import Data.Bits(Bits,xor,rotate);
import Salsa20;
chacha :: (Bits a, Num a) => Rotation -> a -> a -> a -> (a,a);
chacha (Rotation k) b c d = let {
c2 = c + d;
b2 = xor b c2;
} in (rotate b2 k, c2);

{- and
Zab->Za (writing Z last)
bcd->bc (writing b last)
def->de (writing d last)
fgh->fg (writing f last)

TUVWXYZabcd
bcdefgh
alternatively backward
Zab -> Za
XYZ -> XY fgh -> fg
VWX -> VW def -> de

-}

do_column :: forall a shift_t . Arity -> (shift_t -> [a] -> [a]) -> [shift_t] -> [a] -> [a];
do_column (Arity arity) f shifts l0 = let
{ l :: [a]
; l = cycle l0
; answer :: [a]
; answer = undefined
} in undefined;


}
