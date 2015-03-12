open SMLofNJ.Susp

datatype dir = L | R
signature BININFDEPTHTREE =
sig
  type 'a tree
  val constTree : 'a -> 'a tree
  val tMap : ('a -> 'b) -> 'a tree -> 'b tree
  val subTree : dir list -> 'a tree -> 'a tree
  val mirror : 'a tree -> 'a tree
  val unfold : ('a -> 'a * 'a * 'a) -> 'a -> 'a tree
end

structure BinInfDepthTree : BININFDEPTHTREE =
struct
  datatype 'a tree = Fork of 'a * 'a treePair susp | Leaf
  withtype 'a treePair = 'a tree * 'a tree

  fun constTree a =
  let
    val this = ref Leaf
    val _ = this := Fork(a, delay( fn _ => (!this, !this)))
  in !this
  end

  fun tMap f Leaf = Leaf
    | tMap f (Fork(a, t)) = Fork(f a,
    delay(fn _ =>
    let
      val (left, right) = force t
    in (tMap f left, tMap f right)
    end ))

  fun subTree [] a = a
    | subTree _ Leaf = Leaf
    | subTree (x::xs) (Fork(_, t)) =
    let
      val (left, right) = force t
    in case x of L => subTree xs left
          | R => subTree xs right
    end

  fun mirror Leaf = Leaf
    | mirror (Fork(a, t)) = Fork(a, delay( fn _ =>
        let
          val (left, right) = force t
        in
          (mirror right, mirror left)
        end ))

  fun unfold f arg =
  let val (head, arg_left, arg_right) = f arg
  in Fork(head, delay( fn _ => (unfold f arg_left, unfold f arg_right)))
  end
end

val int_tree =
  let
    fun dblThat x = (x, 2*x, 2*x+1)
  in BinInfDepthTree.unfold dblThat 1
  end
