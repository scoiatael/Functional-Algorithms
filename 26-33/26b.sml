val _ = Control.lazysml := true
open Lazy

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
  val sDrop : int -> 'a stream -> 'a stream
  val sTake : int -> 'a stream -> 'a list
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
structure LazyStream : STREAM =
struct
  datatype 'a StreamCell = NIL | CONS of 'a * 'a Stream
  withtype 'a Stream = 'a StreamCell susp
  exception SHd and STl and Nth
  fun lazy sHd ($NIL) = raise SHd
    | sHd ($CONS(x, t)) = x
  fun sTl $NIL = raise STl
    | sTl $CONS(x, t) = t
  fun lazy nth 0 $NIL = raise Nth
    | nth 0 $CONS(x,_) = x
    | nth n $CONS(_, t) = nth (n-1) t
  fun lazy prefix f $NIL = $NIL
    | prefix f $CONS(x, t) =
    let
      val t' = prefix f t
    in (if f x then $CONS(x, t') else t')
    end
  fun lazy suffix f str = let
    fun lazy suffix' $NIL = (true, $NIL)
      | suffix' $CONS(x, t) =
      let
        val (cont, t') = suffix' t
        val cont' = cont andalso (f x)
      in (cont', if cont' then $CONS(x, t') else t')
      end
  in #2 (suffix' str)
  end
  fun lazy splitp f $NIL = ($NIL, $NIL)
    | splitp f $CONS(x, t) =
    let
      val (ts, fs) = splitp f t
    in if f x then ($CONS(x, ts), fs) else (ts, $CONS(x,fs))
    end
  fun lazy from n = $CONS(n, from (n+1))
  fun lazy sMap f $NIL = $NIL
    | sMap f $CONS(x, xs) = $CONS(f x, sMap f xs)
  fun lazy flatten $NIL = $NIL
    | flatten $CONS(x, xs) = ++(x, flatten xs)
  fun lazy ++ (($NIL), f) = f
    | ++ (($CONS (x, s)), f) = $CONS (x, ++(s,f))
  fun lazy sTake (0,s) = $NIL
    | sTake (n, $NIL) = $NIL
    | sTake (n, $CONS(x, s)) = $CONS(x, sTake (n-1 , s))
  fun lazy sDrop (0, s) = s
    | sDrop (n, $NIL) = $NIL
    | sDrop (n, $CONS(x, s)) = sDrop (n-1 , s)
  fun lazy unzip s = (sMap #1 s, sMap #2 s)
  fun lazy split n str = (sTake n str, sDrop n str)
  fun lazy mkStream f = sMap f (from 0)
  fun lazy constStream a = mkStream (fn _ => a)
  fun lazy zip astr bstr = flatten (sMap (fn a => sMap (fn b => (a,b)) bstr) astr)
  fun lazy splice astr bstr = flatten (sMap (fn a => zip (constStream a) bstr) astr)
  fun lazy filter f str = sMap valOf (#1 (splitp isSome (sMap f str)))
  fun lazy find f astr = filter (fn a => if f a then SOME a else NONE) astr
  fun lazy remove f astr = filter (fn a => if f a then NONE else SOME a) astr
  fun lazy findOne f astr = (SOME (sHd (find f astr))) handle SHd => NONE
end
