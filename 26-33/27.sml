open SMLofNJ.Susp

signature STREAM =
sig
  type 'a stream
  val sHd : 'a stream -> 'a
  val sTl : 'a stream -> 'a stream
  val ++ : 'a stream * 'a stream -> 'a stream
  val constStream : 'a -> 'a stream
  val mkStream : (int -> 'a) -> 'a stream
  val from : int -> int stream
  val sMap : ('a -> 'b) -> 'a stream -> 'b stream
  val sDrop : int * 'a stream -> 'a stream
  val sTake : int * 'a stream -> 'a list
  val zip : 'a stream -> 'b stream -> ('a * 'b) stream
  val unzip : ('a * 'b) stream -> 'a stream * 'b stream
  val splice : 'a stream -> 'b stream -> ('a * 'b) stream
  val filter : ('a -> 'b option) -> 'a stream -> 'b stream
  val find : ('a -> bool) -> 'a stream -> 'a stream
  val remove : ('a -> bool) -> 'a stream -> 'a stream
  val findOne : ('a -> bool) -> 'a stream -> 'a option
  val flatten : 'a stream stream -> 'a stream
  val nth : int -> 'a stream -> 'a
  val prefix : ('a -> bool) -> 'a stream -> 'a stream
  val suffix : ('a -> bool) -> 'a stream -> 'a stream
  val split : int -> 'a stream -> 'a stream * 'a stream
  val splitp : ('a -> bool) -> 'a stream -> 'a stream * 'a stream
end

structure SuspStream : STREAM =
struct
  datatype 'a stream = CONS of 'a * 'a stream susp | NIL
  exception SHd and STl and Nth
  fun sHd NIL = raise SHd
    | sHd (CONS(x, t)) = x
  fun sTl NIL = raise STl
    | sTl (CONS(x, t)) = force t
  fun nth 0 NIL = raise Nth
    | nth 0 (CONS(x,_)) = x
    | nth n (CONS(_, t)) = nth (n-1) (force t)
  fun prefix f NIL = NIL
    | prefix f (CONS(x, t)) =
    let
      val t' = delay( fn _ => prefix f (force t))
    in (if f x then CONS(x, t') else (force t'))
    end
  fun suffix f str = let
    fun suffix' NIL = (true, NIL)
      | suffix' (CONS(x, t)) =
      let
        val (cont, t') = suffix' (force t)
        val cont' = cont andalso (f x)
      in (cont', if cont' then CONS(x, delay( fn _ => t')) else t')
      end
  in #2 (suffix' str)
  end
  fun splitp f NIL = (NIL, NIL)
    | splitp f (CONS(x, t)) =
    let
      val (ts, fs) = splitp f (force t)
    in if f x then (CONS(x, delay( fn _ => ts)), fs) else (ts, CONS(x, delay(
    fn _ => fs)))
    end
  fun from n = (CONS(n, delay( fn _ => from (n+1))))
  fun sMap f NIL = NIL
    | sMap f (CONS(x, xs)) = CONS(f x, delay( fn _ => sMap f (force xs)))
  fun ++ ((NIL), f) = f
    | ++ ((CONS (x, s)), f) = CONS (x, delay( fn _ => ++( force s,f)))
  fun flatten NIL = NIL
    | flatten (CONS(x, xs)) = ++(x, flatten (force xs))
  fun toList NIL = []
    | toList (CONS(x,s)) = x::(toList (force s))
  fun sTake' (0,s) = NIL
    | sTake' (n, NIL) = NIL
    | sTake' (n, CONS(x, s)) = (CONS(x, delay( fn _ => sTake' (n-1 , force
    s))))
  fun sTake a = toList (sTake' a)
  fun sDrop (0, s) = s
    | sDrop (n, NIL) = NIL
    | sDrop (n, CONS(x, s)) = sDrop (n-1 , force s)
  fun unzip NIL = (NIL,NIL)
    | unzip (s as CONS((_,_),_)) = (sMap #1 s, sMap #2 s)
  fun split n str = (sTake' (n,str), sDrop (n,str))
  fun mkStream f = sMap f (from 0)
  fun constStream a = mkStream (fn _ => a)
  fun zip astr bstr = flatten (sMap (fn a => sMap (fn b => (a,b)) bstr) astr)
  fun splice astr bstr = flatten (sMap (fn a => zip (constStream a) bstr) astr)
  fun filter f str = sMap valOf (#1 (splitp isSome (sMap f str)))
  fun find f astr = filter (fn a => if f a then SOME a else NONE) astr
  fun remove f astr = filter (fn a => if f a then NONE else SOME a) astr
  fun findOne f astr = (SOME (sHd (find f astr))) handle SHd => NONE
end
