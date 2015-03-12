datatype tree = & of int * (tree * tree) | %
infix &

fun flatten (x&(t1,t2)) = flatten t1 @ [x] @ flatten t2
  | flatten % = []

fun betFlatten tre =
let
  fun flattenAcc (x&(t1,t2)) acc = flattenAcc t1 ( x :: (flattenAcc t2 acc ) )
    | flattenAcc % acc = acc
in
  flattenAcc tre []
end

fun test_f f =
let
  val simpleTree = 7&(3&(1&(%,%),5&(%,%)), 11&(9&(%,%), 13&(%,%)))
in
  f simpleTree
end
