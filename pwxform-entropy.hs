{-# LANGUAGE LambdaCase #-}
module Main where {
import Data.List;
--import Data.Set(Set);
import qualified Data.Set as Set;
import System.Environment(getArgs);
to_pow_two :: Integer -> [Integer];
to_pow_two n = genericTake ((2::Integer)^n) $ enumFrom 0;

all_multiples :: Integer -> [Integer];
all_multiples n = do {
x <- to_pow_two n;
y <- to_pow_two n;
return $ x*y;
};

main :: IO ();
main = getArgs >>= \case {
[n] -> print $ Set.size $ Set.fromList $ all_multiples $ read n;
_ -> error "n";
};

} --end
