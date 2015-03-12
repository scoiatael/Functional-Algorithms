fun suffixes [] = [[]]
  | suffixes (x::xs) =
  let
    val suffixes_of_xs = suffixes xs
    fun add_x_to_last (y::ys) = (x::y)::y::ys
  in
    add_x_to_last suffixes_of_xs
  end

fun suffixes2 [] = [[]]
  | suffixes2 (t as (x::xs)) = t :: (suffixes xs)
