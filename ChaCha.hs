{-# LANGUAGE ScopedTypeVariables #-}
module ChaCha where {
import SeqIterate;
import Util;
import Control.Monad.Identity;
import Algebraic;
import Data.Bits(Bits,xor,rotate);
import Data.List;
import Data.Word;
import Control.Exception(assert);

newtype Double_rounds = Double_rounds Integer deriving (Show);

round_function :: (Bits a, Num a) => Rotation -> [a] -> [a];
round_function (Rotation k) (b:c:d:rest) = let {
c2 = c + d;
b2 = xor b c2;
} in rotate b2 k:c2:d:rest;
round_function _ _ = error "need at least 3 elements";

-- someday, extend to greater than 4, but for now, take the easy route of 4xN matrices.
do_column :: forall a shift_t . (shift_t -> [a] -> [a]) -> [shift_t] -> [a] -> [a];
do_column f shifts = case shifts of {
[] -> id;
s1:s2:srest -> do_column f srest . list_rotate (negate 1) . f s2 . list_rotate 2 . f s1 . list_rotate (negate 1);
[s1] -> -- strange odd number of shifts
 list_rotate 1 . f s1 . list_rotate (negate 1);
};

rotations :: [Rotation];
rotations = map Rotation [16,12,8,7];

quarter_round :: (Bits a, Num a) => [a] -> [a];
quarter_round = do_column round_function rotations;

chacha_algebraic :: [Algebraic Char];
chacha_algebraic = quarter_round $ map Atom "abcd";

chacha_algebraic_sizes :: IO();
chacha_algebraic_sizes = mapM_ (print.size) chacha_algebraic;

shift_rows :: [[a]] -> [[a]];
shift_rows = transpose . (zipWith list_rotate $ enumFrom 0) . transpose;

unshift_rows :: [[a]] -> [[a]];
unshift_rows = transpose . (zipWith list_rotate $ map negate $ enumFrom 0) . transpose;

-- we assume the matrix is already transposed;
-- one_round :: ([a] -> [a]) -> [[a]] -> [[a]];
--one_round f = transpose . unshift_rows . transpose . map f . transpose . shift_rows . transpose . map f;
double_roundM :: forall a m . Monad m => ([a] -> m [a]) -> [[a]] -> m [[a]];
double_roundM f l = mapM f l >>= mapM f . shift_rows >>= return . unshift_rows ;

-- output matches up with the QUARTERROUND macro calls in C reference implementation
show_double_round :: IO ();
show_double_round = show_double_round_extended $ Matrix_width 4;

-- extending ChaCha to any 4xW matrix
show_double_round_extended :: Matrix_width -> IO();
show_double_round_extended w@(Matrix_width n) = void $ double_roundM (\l -> do {print l;return l}) $ transpose $ to_matrix w $ genericTake (4*n) $ enumFrom (0::Integer);

double_round :: (Bits a, Num a) => [[a]] -> [[a]];
double_round = runIdentity . (double_roundM $ return . quarter_round);

core :: (NFData a, Bits a, Num a) => Double_rounds -> [[a]] -> [[a]];
core (Double_rounds n) = transpose . (flip genericIndex) n . seqIterate double_round . transpose;

test_input :: [W];
test_input = key_iv_setup [1..32] [3,1,4,1,5,9,2,6] 7;

core_plus :: (Bits a, Num a, NFData a) => Double_rounds -> [[a]] -> [[a]];
core_plus n input = zipWith (zipWith (+)) input $ core n input;

-- name is sic, copying reference code , despite it being ChaCha
salsa20_wordtobyte :: Double_rounds -> [W] -> [Word8];
salsa20_wordtobyte rounds = concatMap u32le . concat . core_plus rounds . to_matrix (Matrix_width 4);

-- This implementation is about 3000 times slower than the reference C implementation (which itself is slower than optimized C implementations).
example :: Double_rounds -> IO ();
example rounds = do{
 -- mapM_ putStrLn $ map unwords $ to_matrix 4 $ map whex test_input;
 mapM_ putStrLn $ map unwords $ to_matrix (Matrix_width 16) $ map hex_byte $ salsa20_wordtobyte rounds test_input;
};

vector_with_header :: String -> [W] -> IO ();
vector_with_header h v = do {
putStrLn h;
mapM_ (putStrLn.whex) v;
putStrLn "";
};

matrix_with_header :: String -> [[W]] -> IO ();
matrix_with_header h m = do {
putStrLn h;
mapM_ putStrLn $ map (unwords.map whex) m;
putStrLn [];
};

matrix_with_headerM :: [[W]] -> String -> ([[W]] -> [[W]]) -> IO ();
matrix_with_headerM m h f = do {
putStrLn h;
mapM_ putStrLn $ map (unwords.map whex) $ transpose $ f $ transpose m;
putStrLn [];
};

test_vectors :: IO ();
test_vectors = do {
let {
 v :: [W];
 v = head $ transpose m;
 m :: [[W]];
 m = to_matrix (Matrix_width 4) test_input;
};
 vector_with_header "original column" v;
 vector_with_header "after first line of round function"
 $ do_column round_function (take 1 rotations) v;

 vector_with_header "after second line of round function"
 $ do_column round_function (take 2 rotations) v;

 vector_with_header "after third line of round function"
 $ do_column round_function (take 3 rotations) v;

 vector_with_header "after all 4 lines of round function, i.e., quarter round"
 $ quarter_round v;

 matrix_with_headerM m "original matrix" id;
 matrix_with_headerM m "one round (4 quarter rounds on columns)" $ map quarter_round;

 matrix_with_headerM m "after shift rows" $ shift_rows . map quarter_round;

 matrix_with_headerM m "after another 4 quarter rounds on columns" $ map quarter_round . shift_rows . map quarter_round;

 matrix_with_header "unshifting rows (concludes 1 double round)" $ core (Double_rounds 1) m;
 matrix_with_header "after 8 rounds (4 double rounds)" $ core (Double_rounds 4) m;
 matrix_with_header "Adding the original input to the output of 8 rounds" $ core_plus (Double_rounds 4) m;

 putStrLn "reading the above as bytes, little endian";
 example $ Double_rounds 4;

 let { n = 10000 };
 putStrLn $ "\nsame as above but with " ++ show (2*n) ++ " rounds (" ++ show n ++ " double rounds)";
 example $ Double_rounds n;
};

key_iv_setup :: [Word8] -> [Word8] -> Word64 -> [W];
key_iv_setup key iv counter = assert (256 == 8*length key)
$ assert (64 == 8* length iv)
$ let { (q,r) = divMod counter $ 2^(32::Integer) }
in salsa20_diagonal ++ u8_to_32_little key ++ [fromIntegral r, fromIntegral q] ++ u8_to_32_little iv;
}
