module SeqIterate where {
import Control.DeepSeq;
seqIterate :: NFData a => (a -> a) -> a -> [a];
seqIterate f x = deepseq x $ x : seqIterate f (f x);
}
