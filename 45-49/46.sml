Control.lazysml := true;
open Lazy;

exception EMPTY

signature ORDERED =
sig
  type T

  val eq : T * T -> bool
  val lt : T * T -> bool
  val leq : T * T -> bool
end


structure Elem : ORDERED =
struct
  type T = int

  fun eq (x,y) = x=y
  fun lt (x,y) = x<y
  fun leq (x,y) = x<=y
end

datatype Tree = NODE of int * Elem.T * Tree list
type Heap = Tree list susp

val empty = $[]
fun isEmpty ($ts) = null ts

fun rank (NODE (r, x, c)) = r
fun root (NODE (r, x, c)) = x
fun link (t1 as NODE (r, x1, c1), t2 as NODE (_, x2 , c2)) =
  if Elem.leq (x1, x2) then NODE (r+1, x1, t2 :: c1)
  else NODE (r+1, x2, t1 :: c2)
fun insTree (t, []) = [t]
  | insTree (t, ts as t':: ts') =
    if rank t < rank t' then t:: ts else insTree (link (t, t'), ts')
fun mrg (ts1, []) = ts1
  | mrg([], ts2) = ts2
  | mrg (ts1 as t1 :: ts1', ts2 as t2 :: ts2') =
  if rank t1 < rank t2 then t1 :: mrg (ts1', ts2)
  else if rank t2 < rank t1 then t2 :: mrg (ts1, ts2' )
  else insTree (link (t1, t2), mrg (ts1', ts2' ))

fun lazy insert (x, $(ts)) = $(insTree (NODE (0, x, []), ts))
fun merge ($(ts1), $(ts2)) = $(mrg (ts1, ts2))

fun removeMinTree [] = raise EMPTY
  | removeMinTree [t] = (t, [])
  | removeMinTree (t :: ts) =
    let val (t', ts') = removeMinTree ts
    in if Elem.leq (root t, root t') then (t, ts) else (t', t :: ts' ) end
fun findMin ($ts) = let val (t, _) = removeMinTree ts in root t end
fun deleteMin ($ts) =
  let val (NODE (_, x, ts1), ts2) = removeMinTree ts
  in $(mrg (rev ts1, ts2)) end
