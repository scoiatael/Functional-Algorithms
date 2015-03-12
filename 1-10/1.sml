fun sublist (x::xs) =
let
  fun addx (ys::yss) = (x::ys) :: addx yss
    | addx [] = []
  val xss = sublist xs
in
  xss @ addx xss
end
    | sublist [] = [[]]

fun betSublist (x::xs) =
let
  fun addxAcc (ys::yss) a = (x::ys) :: addxAcc yss a
    | addxAcc [] a = a
  val xss = betSublist xs
in
  addxAcc xss xss
end
    | betSublist [] = [[]]
