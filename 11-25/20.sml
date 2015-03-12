datatype Empty = EMPTY
exception EMPTY

signature ANY =
sig
  type T
end

signature ORDERED =
sig
  type T

  val eq : T * T -> bool
  val lt : T * T -> bool
  val leq : T * T -> bool
end

signature SET =
sig
  type Elem
  type Set

  val empty : Set
  val insert : Elem * Set -> Set
  val member : Elem * Set -> bool
end


functor UnbalancedSet(Element : ORDERED) : SET =
struct
  type Elem = Element.T
  datatype Tree = E | T of Tree * Elem * Tree
  type Set = Tree

  val empty = E

  fun member (x, E) = false
    | member (x, T (a, y, b)) =
    if Element.lt (x,y) then member (x,b)
    else if Element.lt (y,x) then member (x,b)
    else true

  fun insert (x,E) = T (E,x,E)
    | insert (x, s as T (a,y,b)) =
    if Element.lt (x,y) then T (insert (x,a), y, b)
    else if Element.lt (x,y) then T (a, y, insert (x,b))
    else s
end

signature FiniteMap =
sig
  type Key
  type 'a Map

  val empty : 'a Map
  val bind : Key * 'a * 'a Map -> 'a Map
  val lookup : Key * 'a Map -> 'a
end

functor OrderedPair(structure Key : ORDERED
                    structure A : ANY) : ORDERED =
struct
  type T = Key.T * A.T

  fun eq ((a,_),(b,_)) = Key.eq (a,b)
  fun lt ((a,_),(b,_)) = Key.lt (a,b)
  fun leq ((a,_),(b,_)) = Key.leq (a,b)
end

functor UnbalancedMap(Key : ORDERED) : FiniteMap =
struct

  functor OrderedPair( A : ANY ) : ORDERED =
  struct
    type T = Key.T * A.T

    fun eq ((a,_), (b,_ )) = Key.eq (a,b)
    fun leq ((a,_), (b,_ )) = Key.leq (a,b)
    fun lt ((a,_), (b,_ )) = Key.lt (a,b)
  end

  type 'a Map = UnbalancedSet( OrderedPair( 'a ) )

  val empty = empty
  fun bind (k,a,m) = insert ((k,a), m)
  fun lookup (k,E) = raise EMPTY
    | lookup (k, T (t1,(k',v),t2) ) =
    if Key.eq (k, k')
    then v
    else if Key.lt (k, k')
    then lookup (k, t1)
    else lookup (k, t2)
end
