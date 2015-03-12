fun quicksort [x] = [x]
  | quicksort [] = []
  | quicksort (x :: xs) =
  let
    fun filter f [] = []
      | filter f (x::xs) = if f x then x::(filter f xs) else filter f xs
    val lessThenX = filter (fn y => y < x) xs
    val bigThenX = filter (fn y => y >= x) xs
  in
    (quicksort lessThenX) @ [x] @ (quicksort bigThenX)
  end
