{-# LANGUAGE ScopedTypeVariables #-}
-- enough rope to evaluate Salsa20 algebraically
module Main(main) where {
import Data.Bits(Bits(..));
import Data.List(transpose);
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

simplify_in_order :: [Assign] -> [Assign];
simplify_in_order [] = [];
simplify_in_order (f@(Assign v1 e1):rest) = f:(simplify_in_order
$ map (\(Assign v e) -> Assign v $ simplify ((v1,Updated), e1) e) rest);

algebraic_show :: (Algebraic (Vnum,a)) -> String;
algebraic_show (Atom ((Vnum i),_)) = "x[" ++ show i ++ "]";
algebraic_show (Add x y) = "(" ++ algebraic_show x ++ " + " ++ algebraic_show y ++ ")";
algebraic_show (Xor x y) = algebraic_show x ++ " ^ " ++ algebraic_show y;
algebraic_show (Rotate x k) = "(" ++ algebraic_show x ++ " <<< " ++ show k ++ ")";

run_algebraic :: [Vnum] -> [Assign];
run_algebraic order = zipWith Assign order (column $ do { n <- order; return $ Atom (n,Original)});
-- list_rotate 2 is because the output lags by 2 (arity) from the input.

simplify_run :: [Assign] -> [Assign];
simplify_run = simplify_in_order . (list_rotate $ unArity salsa20_arity);

output_c :: Assign -> String;
output_c (Assign (Vnum i) e) = "x[" ++ show i ++ "] := " ++ algebraic_show e;

c_code_out :: [String];
c_code_out = map output_c $ concatMap (simplify_run . run_algebraic . map Vnum) $ (shift_columns $ transpose int_matrix) ++ shift_columns int_matrix;

-- the original value of a variable, or the updated value.
-- Fortunately, we do not update a variable more than once.
data Epoch = Original | Updated deriving (Show, Eq);

type Expr = Algebraic (Vnum, Epoch);
newtype Vnum = Vnum Integer deriving (Show,Eq);

data Assign = Assign Vnum Expr;

main :: IO();
main = mapM_ putStrLn c_code_out;
}
