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
end

structure LazyStream : STREAM =
struct
  datatype 'a StreamCell = NIL | CONS of 'a * 'a Stream
  withtype 'a Stream = 'a StreamCell susp
  exception SHd and STl and Nth
  fun lazy sHd ($NIL) = raise SHd
    | sHd ($CONS(x, t)) = x
  fun lazy sTl $NIL = raise STl
    | sTl $CONS(x, t) = t
  fun lazy from n = $CONS(n, from (n+1))
  fun lazy sMap f $NIL = $NIL
    | sMap f $CONS(x, xs) = $CONS(f x, sMap f xs)
  fun lazy ++ (($NIL), f) = f
    | ++ (($CONS (x, s)), f) = $CONS (x, ++(s,f))
  fun lazy unzip s = (sMap #1 s, sMap #2 s)
  fun lazy split n str = (sTake n str, sDrop n str)
  fun lazy mkStream f = sMap f (from 0)
  fun lazy constStream a = mkStream (fn _ => a)
end
