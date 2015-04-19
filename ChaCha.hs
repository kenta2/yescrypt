{-# LANGUAGE ScopedTypeVariables #-}
module ChaCha where {
import Util;
import Control.Monad.Identity;
import Algebraic;
import Data.Bits(Bits,xor,rotate);
import Data.List;
import Data.Word;
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

shift_rows :: [[a]] -> [[a]];
shift_rows = zipWith list_rotate $ enumFrom 0;

unshift_rows :: [[a]] -> [[a]];
unshift_rows = zipWith list_rotate $ map negate $ enumFrom 0;

-- we assume the matrix is already transposed;
-- one_round :: ([a] -> [a]) -> [[a]] -> [[a]];
--one_round f = transpose . unshift_rows . transpose . map f . transpose . shift_rows . transpose . map f;
one_roundM :: forall a m . Monad m => ([a] -> m [a]) -> [[a]] -> m [[a]];
one_roundM f l = mapM f l >>= mapM f . transpose . shift_rows . transpose >>= return . transpose . unshift_rows . transpose;

show_one_round :: IO ();
show_one_round = do {
_ <- one_roundM (\l -> do {print l;return l}) $ transpose $ int_matrix;
return ();
};

one_round :: (Bits a, Num a) => [[a]] -> [[a]];
one_round = runIdentity . (one_roundM $ return . quarter_round);

core :: (Bits a, Num a) => Integer -> [[a]] -> [[a]];
core n = transpose . (flip genericIndex) n . iterate one_round . transpose;

test_input :: [W];
test_input = [0x456723c6,0x98694873,0xdc515cff,0x944a58ec,0x1f297ccd,0x58bad7ab,0x41f21efb,0xa9e3e146,0x007c62c2,0x085427f8,0x231be9e8,0xcde7438d,0x0f76255a,0xf92e7263,0xc233d79f,0xc4c9079a];

core_plus :: (Bits a, Num a) => Integer -> [[a]] -> [[a]];
core_plus n input = zipWith (zipWith (+)) input $ core n input;

-- name is sic, copying reference code , despite it being ChaCha
salsa20_wordtobyte :: [W] -> [Word8];
salsa20_wordtobyte = concatMap u32le . concat . core_plus 4 . to_matrix 4;

chacha_example :: IO ();
chacha_example = do{
 mapM_ putStrLn $ map unwords $ to_matrix 4 $ map whex test_input;
 mapM_ putStrLn $ map unwords $ to_matrix 16 $ map hex_byte $ salsa20_wordtobyte test_input;
};

}
