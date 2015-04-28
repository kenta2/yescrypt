{-# LANGUAGE LambdaCase #-}
-- computes values of OEIS A027417
module Main where {
import Data.List;
--import Data.Set(Set);
--import qualified Data.Set as Set;
-- Data.Set runs out of memory at 14
import qualified Data.IntSet as Set;
-- IntSet runs out at 16
-- 15 takes 24 minutes.
import System.Environment(getArgs);
import Control.Exception(assert);
to_pow_two :: Integer -> [Integer];
to_pow_two n = genericTake ((2::Integer)^n) $ enumFrom 0;

all_multiples :: Integer -> [Int];
all_multiples n = do {
x <- to_pow_two n;
y <- enumFromTo 0 x;
let {z = x*y};
assert (z < fromIntegral (maxBound :: Set.Key)) $ return ();
return $ fromIntegral z;
};

main :: IO ();
main = getArgs >>= \case {
[n] -> print $ Set.size $ Set.fromList $ all_multiples $ read n;
_ -> error "n";
};

{- 2 7 26 90 340 1238 4647 17578 67592 259768 1004348 3902357 15202050
59410557 232483840 -}

} --end
