module Main where {
import Salsa20;
import qualified Data.ByteString.Lazy as Lazy;
main :: IO();
-- sadly normal String putStr fails here due to unicode.
main = Lazy.putStr $ Lazy.pack $ xsalsa (replicate 32 0) (replicate 24 0);
}
