module Main where {
import Salsa20;
import qualified Data.ByteString.Lazy as Lazy;
main :: IO();
-- sadly normal String putStr fails here due to unicode.
main = Lazy.putStr $ Lazy.pack $ xsalsa (replicate 32 0) (replicate 24 0);
-- Approximately 310 million years to consume all 2^64 blocks until
-- the block counter overflows.

-- 1200 times slower than the C version.
}
