import Prelude hiding ((++), map)

[] ++ bs = bs
(a:as) ++ bs = a:(as ++ bs)

map _ [] = []
map f (x:xs) = (f x):(map f xs)

perms [] = [[]]
perms (x:xs) = _perms [] x xs
  where _perms as b [] = map (b:) (perms as)
        _perms as b (c:ds) =
          (map (b:) (perms (as ++ (c:ds)))) ++
          (_perms (as ++ [b]) c ds)
