fun findLowestBiggerThan x [] = []
  | findLowestBiggerThan x (y::xs) =
  let
    fun substituteIfAny a [b] = if a < b then [a] else [b]
      | substituteIfAny a [] = [a]
    val lowestBiggerFrom_xs = findLowestBiggerThan x xs
  in
    if or (x >= y)
    then
      lowestBiggerFrom_xs
    else
      substituteIfAny y lowestBiggerFrom_xs
  end

fun findLowest [x] = x
  | findLowest (x::xs) =
  let
    val lowest = findLowest xs
  in
    if x < lowest
    then x
    else lowest
  end

fun sortBiggerThen x xs =
let
  val lowestBiggerFrom_xs = findLowestBiggerThan x xs
  fun countX [] = 0
    | countX (y::ys) = (countX ys) + (if x = y then 1 else 0)
  fun appendXTimes 0 ys = ys
    | appendXTimes n ys = x::(appendXTimes (n-1) ys)
  fun insertX times [] = appendXTimes times []
    | insertX times [y] = appendXTimes times (sortBiggerThen y xs)
  val numberX = countX xs
in
  insertX numberX lowestBiggerFrom_xs
end


fun sort [] = []
  | sort xs =
  let
    val lowest = findLowest xs
  in
    sortBiggerThen lowest xs
  end
