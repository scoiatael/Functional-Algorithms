datatype tree = E | T of (tree * int * tree)

fun bigger (pivot, E) = E
  | bigger (pivot, T (a, x, b)) =
    if x <= pivot then bigger (pivot, b)
    else case a of
        E => T ( E, x, b)
      | T (a1, y, a2) =>
        if y <= pivot then T (bigger (pivot, a2), x, b)
        else T (bigger (pivot, a1), y, T (a2, x, b))

fun smaller (pivot, E) = E
  | smaller (pivot, T (a, x, b)) =
    if x > pivot then smaller (pivot, a)
    else case b of
        E => T (a, x, E)
      | T (b1, y, b2) =>
        if y > pivot then T (a, x, smaller (pivot, b1))
        else T (T (a, x, b1), y, smaller (pivot, b2))

fun insert a E = T (E, a, E)
  | insert a t =
  let
    val sm = smaller (a,t)
    val bg = bigger (a,t)
  in T (sm, a, bg)
  end

val t  = insert 6 E
val t2 = insert 15 t
val t3 = insert 10 t2
val t4 = insert 1 t3
val t5 = insert 6 t4
val t6 = insert 1 t5

fun p x = x + 10
fun map (f, E) = E
  | map (f, T(a,x,b)) = T ( map (f,a), f x, map (f,b))

val some_tree2 = T ( (T (E, 1, E)) , 2, (T (E, 8, E) ))
val some_tree3 = T (some_tree2, 9, map (p,some_tree2))

fun flatten E = []
  | flatten (T(a,x,b)) = flatten a @ [x] @ flatten b

fun split (T (a, x, b)) = (a,b)
fun less x = #1 ( split x)
fun bigg x = #2 ( split x)

fun partition (pivot, E) = (E, E)
  | partition (pivot, t as T (a, x, b)) =
    if x <= pivot then
      case b of
        E => (t, E)
      | T(b1, y, b2) =>
          if y <= pivot then
            let val (small, big) = partition (pivot, b2)
            in (T (T (a, x, b1), y, small), big) end
          else
            let val (small, big) = partition (pivot, b1)
            in (T (a, x, small), T (big, y, b2)) end
    else
    case a of
      E => (E, t)
    | T(a1, y, a2) =>
      if y <= pivot then
        let val (small, big) = partition (pivot, a2)
        in (T (a1, y, small), T (big, x, b)) end
      else
        let val (small, big) = partition (pivot, a1)
        in (small, T (big, y, T (a2 , x, b))) end

fun insert' a E = T (E, a, E)
  | insert' a t =
  let
    val (sm, bg) = partition (a, t)
  in T (sm, a, bg)
  end

val t'  = insert' 6 E
val t2' = insert' 15 t
val t3' = insert' 10 t2
val t4' = insert' 1 t3
val t5' = insert' 6 t4
val t6' = insert' 1 t5
