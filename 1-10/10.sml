datatype tree = & of int * (tree * tree) | %
infix &

datatype triValue = Found | NotFound | NotSure

fun member_helper (x, %) = NotSure
  | member_helper (x, y&(t1,t2)) =
let
  fun from_left Found = Found
    | from_left NotSure = NotSure
    | from_left NotFound = NotFound
  fun from_right Found = Found
    | from_right NotSure = if x = y then Found else NotFound
    | from_right NotFound = NotFound
  val lower =
    if x < y
    then
      from_left ( member_helper (x,t1) )
    else
      from_right ( member_helper (x,t2) )
in lower
end

fun member (x, t) =
let
  fun trivalue_to_bool Found = true
    | trivalue_to_bool NotFound = false
    | trivalue_to_bool NotSure = false
  val lower = member_helper (x, t)
in trivalue_to_bool lower
end

fun insert (x, t) = if member (x, t) then t else
let
  fun insert_helper (x, z as y&(t1,t2)) =
    if x < y
    then
      y&(insert_helper (x,t1), t2)
    else
      y&(t1, insert_helper (x,t2))
    | insert_helper (x, %) = x&(%,%)
in
  insert_helper (x,t)
end

val sample_tree = 4&(2&(1&(%,%), 3&(%,%)), 6&(5&(%,%), 7&(%,%)))

fun make_tree_with f xs = foldl f % xs

datatype changed_tree = Old of tree | New of tree


fun insert2_helper (x, %) = New (x&(%,%))
  | insert2_helper (x, t as y&(t1,t2)) =
let
  fun from_left (Old z) = (Old (y&(z,t2)))
    | from_left (New z) = (New (y&(z,t2)))
  fun from_right (Old z) = (Old (y&(t1,z)))
    | from_right (New z) = if x = y then (Old t) else (Old (y&(t1,z)))
  val lower =
    if x < y
    then
      from_left ( insert2_helper (x,t1) )
    else
      from_right ( insert2_helper (x,t2) )
in lower
end


fun insert2 (x,t) =
let
  fun drop_wrapper (Old t) = t
    | drop_wrapper (New t) = t
in
  drop_wrapper (insert2_helper (x,t))
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
