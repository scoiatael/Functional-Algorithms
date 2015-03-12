Control.lazysml := true;
open Lazy;

datatype lazy 'a stream = Cons of 'a * 'a stream | Nil

fun lazy append ((xs),ys) = case xs of Nil => ys
                                | Cons (x, xs') => Cons (x, append (xs', ys))

val lazy lis1 = Cons (1, Cons (2, Cons (3, Nil)))

fun reverse xs =
let
  fun loop (xs,ys) = case xs of
                          Nil => ys
                        | Cons (x,xs') => loop (xs', Cons (x,ys))
in
 loop (xs, Nil)
end

fun toList xs = case xs of
                     Nil => []
                   | Cons (x,xs') => x::(toList xs')

val empty = (0, Nil, 0, Nil)
fun isEmpty (lenf, _, _, _) = (lenf = 0)
fun check (q as (lenf, f, lenr, r)) =
  if 2*lenf >= lenr then q else (lenf + lenr, append(f, reverse r), 0, Nil)
fun snoc ((lenf, f, lenr, r), x) = check (lenf, f, lenr+1, (Cons (x, r)))
fun head (lenf, Cons (x, f'), lenr, r) = x
fun tail (lenf, Cons (x, f'), lenr, r) = check (lenf-1, f', lenr, r)
fun qToList q = if isEmpty q then [] else (head q)::(qToList (tail q))

fun copyTextFile (infile) =
  let
    val ins = TextIO.openIn infile
    fun helper (copt: char option, q) =
      case copt of
           NONE => (TextIO.closeIn ins; q)
         | SOME(c) => helper(TextIO.input1 ins, snoc (q, c))
  in
    qToList( helper((TextIO.input1 ins, empty)) )
  end

val li = copyTextFile "test1.txt"
