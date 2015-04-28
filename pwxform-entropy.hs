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
import Data.IntSet(IntSet);
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

max_size :: Int;
max_size = 10000000;
size_limited_insert :: IntSet -> Set.Key -> IntSet;
size_limited_insert s k = if Set.size s < max_size
then Set.insert k s
else if k < Set.findMax s
     then if Set.member k s
          then s
          else Set.insert k $ Set.deleteMax s;
     else s;

analyze_range :: Integer -> Set.Key -> [(Int,Set.Key)];
analyze_range n vmin = let {
 done :: IntSet;
 done = foldl' size_limited_insert Set.empty $ filter (>=vmin) $ all_multiples n;
 size = Set.size done;
 vmax = Set.findMax done;
} in (size,vmax):(
 if size == max_size
 then analyze_range n $ succ vmax
 else []);

main :: IO ();
main = getArgs >>= \case {
["range",n] -> mapM_ print $ analyze_range (read n) 0;
["original",n] -> print $ Set.size $ Set.fromList $ all_multiples $ read n;
_ -> error "n";
};

{- 2 7 26 90 340 1238 4647 17578 67592 259768 1004348 3902357 15202050
59410557 232483840 -}

} --end
