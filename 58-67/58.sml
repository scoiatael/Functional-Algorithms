signature HALFDEQUE =
sig
  type 'a Queue
  val empty : 'a Queue
  val isEmpty : 'a Queue -> bool

  val cons : 'a * 'a Queue -> 'a Queue
  val head : 'a Queue -> 'a
  val tail : 'a Queue -> 'a Queue

  val snoc : 'a Queue * 'a -> 'a Queue
end

signature QUEUE =
sig
  type 'a Queue
  val empty : 'a Queue
  val isEmpty : 'a Queue -> bool

  val cons : 'a * 'a Queue -> 'a Queue
  val head : 'a Queue -> 'a
  val tail : 'a Queue -> 'a Queue
end

functor AddSnoc(Q : QUEUE) : HALFDEQUE =
struct
  type 'a Queue = 'a list * 'a Q.Queue

  val empty = ([], Q.empty)
  fun isEmpty ([], q) = Q.isEmpty q
    | isEmpty _ = false

  fun cons (a, (ls,q)) = (ls, Q.cons (a,q))
  fun head (a::_, _) = a
    | head ([], q) = Q.head q
  fun tail ([], q) = ([], Q.tail q)
    | tail (_::xs, q) = (xs, q)

  fun snoc ((ls, q), a) = (a::ls, q)
end

