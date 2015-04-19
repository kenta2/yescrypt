{-# LANGUAGE LambdaCase #-}
module Main where {
import qualified ChaCha;
import Util;
import System.Environment(getArgs);
main :: IO();
main = getArgs >>= \case {[n] -> ChaCha.example $ Double_rounds $ read n };
}
