module Query where

ret v s = [(v,s)]
err = []

chr c []     = err
chr c (x:xs) = if (c==x) then ret c xs else err

alt p q s = case ps of
  [] -> q s
  _  -> ps
 where ps = p s

thn p q s = concat (map (\(r,s') -> q r s') (p s))

mny p = (p `thn` (\v -> mny p `thn` (\vs -> ret (v:vs)))) `alt` (ret [])

v1 = (chr 'a') "abc"
v2 = ((chr 'a') `alt` (chr 'b'))"abc"
v3 = ((chr 'a') `thn` (\v -> chr 'b'))"abc"
v4 = mny (chr 'a') "aaab"
