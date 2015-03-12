fun mergesort [x] = [x]
  | mergesort [] = []
  | mergesort lis =
  let
    val half = length lis div 2
    fun drop 0 a = a
      | drop n (x::xs) = drop (n-1) xs
    fun take 0 _ = []
      | take n (x::xs) = x::(take (n-1) xs)
    val sndHalf = drop half lis
    val fstHalf = take half lis
    fun merge [] a = a
      | merge a [] = a
      | merge (x::xs) (y::ys) = if x > y then y::(merge (x::xs) ys) else
        x::(merge xs (y::ys))
  in
    merge (mergesort fstHalf) (mergesort sndHalf)
  end
