module UserError(assert) where {
assert :: String -> Bool -> a -> a;
assert s x y = if x then y else error $ "UserError.assert failed: " ++ s;
-- assert _ _ y = y;
}
