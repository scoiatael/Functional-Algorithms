fun append ([], xs) = xs
  | append ((x::xs), ys) = x::(append (xs, ys))

fun reverse ys =
let fun loop ([], ys) = ys
  | loop ((x::xs), ys) = loop (xs, x::ys)
in loop (ys, [])
end

val empty = (0, [], 0, [])
fun isEmpty (lenf, _, _, _) = (lenf = 0)
fun check (q as (lenf, f, lenr, r)) =
  if lenf > 0 then q else (lenf + lenr, append(f, reverse r), 0, [])
fun snoc ((lenf, f, lenr, r), x) = check (lenf, f, lenr+1, ((x:: r)))
fun head (lenf, (x:: f'), lenr, r) = x
fun tail (lenf, (x:: f'), lenr, r) = check (lenf-1, f', lenr, r)

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
