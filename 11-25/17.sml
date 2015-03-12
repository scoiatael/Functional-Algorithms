datatype Empty = EMPTY
exception EMPTY
signature ORDERED =
sig
  type T

  val eq : T * T -> bool
  val lt : T * T -> bool
  val leq : T * T -> bool
end

signature HEAP =
sig
  structure Elem: ORDERED

  type Heap

  val empty     : Heap
  val isEmpty   : Heap -> bool

  val insert    : Elem.T * Heap -> Heap
  val merge     : Heap * Heap -> Heap
  val findMin   : Heap -> Elem.T
  val deleteMin : Heap -> Heap
end

functor BinomialHeap (Element: ORDERED) : HEAP =
struct
  structure Elem = Element

  datatype Tree = Node of Elem.T * Tree list
  type Heap = (int * Tree) list

  fun mkHeap ls =
  let
    fun zipNumbers [] _ = []
      | zipNumbers (x::xs) a = (a, x) :: (zipNumbers xs (a-1))
  in
    zipNumbers ls (length ls)
  end

  val empty = []
  fun isEmpty te = null te

  fun root (Node (e, _)) = e
  fun min (a,b) = if Elem.leq (a,b) then a else b
  fun snd (_,b) = b

  fun link (t1 as Node (x1, c1), t2 as Node (x2, c2 )) =
    if Elem.leq (x1, x2) then Node (x1, t2 :: c1)
    else Node (x2, t1 :: c2 )

  fun insTree (t, r, []) = [(r,t)]
    | insTree (t, r, ts as (r', t'):: ts') =
      if r < r'
      then (r,t) :: ts
      else insTree (link (t, t'), r+1, ts')
  fun insert (x, ts) = insTree (Node (x, []), 0, ts)
  fun merge (ts1, []) = ts1
    | merge ([], ts2 ) = ts2
    | merge (ts1 as (r1, t1) :: ts1', ts2 as (r2, t2) :: ts2' ) =
      if r1 < r2 then (r1, t1) :: merge (ts1', ts2 )
      else if r2 < r1 then (r2, t2) :: merge (ts1, ts2' )
      else insTree (link (t1, t2 ), r1+1, merge (ts1', ts2' ))

  fun removeMinTree [] = raise EMPTY
    | removeMinTree [(_,t)] = (t, [])
    | removeMinTree ((r,t):: ts) =
      let val (t', ts' ) = removeMinTree ts
      in if Elem.leq (root t, root t')
        then (t, ts)
        else (t', (r,t)::ts' )
      end
  fun findMin [] = raise EMPTY
    | findMin ((r,t)::xs) = foldl min (root t) (map root (map snd xs) )
  fun deleteMin ts =
  let
    val (Node (x, ts1), ts2 ) = removeMinTree ts
  in merge (mkHeap (rev ts1), ts2 )
  end
end

functor ExplicitMin (H : HEAP) : HEAP =
struct
  structure Elem = H.Elem
  datatype Heap = E | NE of Elem.T * H.Heap

  val empty = E
  fun isEmpty E = true
    | isEmpty _ = false

  fun min (a,b) = if Elem.leq (a,b) then a else b

  fun insert (e, E) = NE (e, H.insert (e, H.empty))
    | insert (e, NE (e', h)) = NE ( min (e,e'), H.insert (e,h))
  fun merge (E, E) = E
    | merge (E, t) = t
    | merge (t, E) = t
    | merge (NE (e, h), NE (e',h')) =
      NE ( min (e,e'), H.merge (h,h'))
  fun findMin E = raise EMPTY
    | findMin (NE (e, _)) = e
  fun deleteMin E = raise EMPTY
    | deleteMin (NE (_, h) ) =
    let
      val h' = H.deleteMin h
    in
      if H.isEmpty h' then E else NE (H.findMin h', h')
    end
end

structure IntOrder : ORDERED = struct
  type T = int

  fun eq (x,y) = x = y
  fun lt (x,y) = x < y
  fun leq (x,y) = x <= y
end

structure MemIntHeap = ExplicitMin( BinomialHeap( IntOrder ) )

val example_heap = MemIntHeap.empty

val example_heap2 = foldl MemIntHeap.insert example_heap [1,7,1,1,3,4]
