datatype tree = & of int * (tree * tree) | %
infix &

fun cbt (0, _) = %
  | cbt (n,e) =
  let
    val lower = cbt ((n-1),e)
  in
    e&(lower,lower)
  end

fun size % = 0
  | size ( e&(a,b) ) = 1 + (size a) + (size b)

datatype MaybeTree = Just of tree | Nothing

fun find_tree_with_size (_, []) = Nothing
  | find_tree_with_size (p, x::xs) =
  if size x = p
  then Just x
  else
    find_tree_with_size (p, xs)

fun exists Nothing = false
  | exists (Just _) = true

fun fromMaybe (Just a) = a
fun fst (a, _) = a

fun bt (n,e) =
let
  fun hbt (k, e, li) =
    let
      val elem = find_tree_with_size (k, li)
    in
      if exists elem
      then (fromMaybe elem, li)
      else if k = 0 then (%, % :: li) else
        let
          val lower_left_size = (k-1) div 2
          val lower_right_size = (k) div 2
          val (lower_left_tree, li') = hbt (lower_left_size, e, li)
          val (lower_right_tree, li'') = hbt (lower_right_size, e, li')
          val new_tree = e&(lower_left_tree, lower_right_tree)
        in
          ( new_tree, new_tree :: li'')
        end
    end
in fst ( hbt (n, e, []) )
end

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
