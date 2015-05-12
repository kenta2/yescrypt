module Main where {
import Salsa20;
import qualified Data.ByteString.Lazy as Lazy;
main :: IO();
-- sadly normal String putStr fails here due to unicode.
main = do {
fi <- Lazy.getContents;
let { (key, salt) = Lazy.splitAt 32 fi };
{-^ We expact exactly 32 + 24 bytes of input -}
Lazy.putStr $ Lazy.pack $ xsalsa (Lazy.unpack key) (Lazy.unpack salt);
-- Approximately 310 million years to consume all 2^64 blocks until
-- the block counter overflows.
};

-- 1200 times slower than the C version.
}
