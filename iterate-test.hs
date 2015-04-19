{-# LANGUAGE LambdaCase #-}
module Main where {
import SeqIterate;
import Data.List;
import System.Environment(getArgs);
main :: IO ();
main = getArgs >>= \case {
      [n] -> print $ (flip genericIndex) ((read n)::Integer) $ iterate id (42::Integer);
["seq",n] -> print $ (flip genericIndex) ((read n)::Integer) $ seqIterate id (42::Integer)};
}
