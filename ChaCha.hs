{-# LANGUAGE ScopedTypeVariables #-}
module ChaCha where {
import Algebraic;
import Data.Bits(Bits,xor,rotate);
import Salsa20(Rotation(..),list_rotate);
chacha :: (Bits a, Num a) => Rotation -> [a] -> [a];
chacha (Rotation k) (b:c:d:rest) = let {
c2 = c + d;
b2 = xor b c2;
} in rotate b2 k:c2:d:rest;
chacha _ _ = error "need at least 3 elements";

-- someday, extend to greater than 4, but for now, take the easy route of 4xN matrices.
do_column :: forall a shift_t . (shift_t -> [a] -> [a]) -> [shift_t] -> [a] -> [a];
do_column f shifts = case shifts of {
[] -> id;
s1:s2:srest -> do_column f srest . list_rotate (negate 1) . f s2 . list_rotate 2 . f s1 . list_rotate (negate 1);
[s1] -> -- strange odd number of shifts
 list_rotate 1 . f s1 . list_rotate (negate 1);
};

quarter_round :: (Bits a, Num a) => [a] -> [a];
quarter_round = do_column chacha $ map Rotation [16,12,8,7];

chacha_algebraic :: [Algebraic Char];
chacha_algebraic = quarter_round $ map Atom "abcd";

chacha_algebraic_sizes :: IO();
chacha_algebraic_sizes = mapM_ (print.size) chacha_algebraic;

}
