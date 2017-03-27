split ls =
  case ls of
    x:xs ->
      case xs of
        x':xs' ->
          case split xs' of
            (left, right) -> (x:left, x':right)
        [] -> (ls, [])
    [] -> ([], [])

merge la lb =
  case la of
    a:as ->
      case lb of
        b:bs -> if a < b
                  then a:(merge as lb)
                  else b:(merge la bs)
        [] -> la
    [] -> lb

mergesort ls =
  case ls of
    x:xs ->
      case xs of
        x':xs' ->
          case split ls of
            (left, right) -> merge (mergesort left) (mergesort right)
        [] -> ls
    [] -> []

ulam n | n `mod` 2 == 0 = n `div` 2
       | otherwise = (3 * n) + 1

ulamChain n | n <= 1 = []
            | otherwise = n:(ulamChain (ulam n))
