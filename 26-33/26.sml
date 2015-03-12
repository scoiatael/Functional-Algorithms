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

structure SuspStream : STREAM =
struct
  datatype 'a streamCell = Cons of 'a * 'a stream | Nil
  withtype 'a stream = 'a streamCell susp
  exception SHd and STl and Nth
  fun sHd str = case force str of Nil => raise SHd
                   | Cons (a,_) => a
  fun sTl str = case force str of Nil => raise STl
                   | Cons (_, astr) => astr
  fun toList str = case force str of Nil => []
                      | Cons (a, astr) => a :: (toList astr)
  fun nth n str = case force str of Nil => raise Nth
    | Cons (a, astr) => if n = 0 then a else nth (n-1) astr
  fun prefix f str = case force str of Nil => delay( fn _ => Nil)
                        | Cons (a, astr) => case f a of true => delay( fn _ =>
                            Cons (a, prefix f astr))
                                               | false => delay( fn _ => Nil)
  fun suffix f str = let
    fun suffix' s = case force s of Nil => (true, delay(fn _ => Nil))
                       | Cons (a, astr) => let
                         val fa =  f a
                         val (cont, astr') = suffix' astr in
                           (cont andalso fa, if (cont andalso fa)
                                                 then delay(fn _ =>
                                                 Cons (a, astr'))
                                                 else astr')
                                           end
  in #2 (suffix' str)
  end
  fun split 0 astr = (delay( fn _ => Nil ), astr)
    | split n astr = let
      val (fs,ls) = split (n-1) (sTl astr)
                     in (delay(fn _ => Cons (sHd astr, fs)), ls)
                     end
  fun splitp f str = case force str of Nil => (delay (fn _ => Nil), delay (fn _ => Nil))
                        | Cons (a, astr) => let
                          val (ts, fs) = splitp f astr
                                            in case f a of true => ( delay( fn _
                                            => Cons (a, ts)), fs)
                                                  | false => (ts, delay( fn _
                                                  => Cons (a, fs)))
                                            end
  fun ++ (astr,bstr) = case force astr of Nil => bstr
                        | Cons (a, astr') => delay( fn _ => Cons(a, ++(astr',
                        bstr)))
  fun from n = delay( fn _ => Cons( n, from (n+1) ))
  fun sMap f str = case force str of Nil => delay(fn _ => Nil)
                       | Cons (a, astr) => delay(fn _ => Cons( f a, sMap f astr))

  fun flatten str = case force str of Nil => delay( fn _ => Nil )
                      | Cons (a, astr) => ++(a,(flatten astr))
  fun unzip str = case force str of Nil => (delay ( fn _ => Nil), delay ( fn _ => Nil))
                     | Cons ((a,b), str') =>
                     let
                       val (astr, bstr) = unzip str'
                     in (delay ( fn _ => Cons(a, astr)), delay ( fn _ =>
                     Cons(b, bstr)))
                     end
  fun mkStream f = sMap f (from 0)
  fun constStream a = mkStream (fn _ => a)
  fun sDrop n str = #2 (split n str)
  fun sTake n str = toList (#1 (split n str))
  fun zip astr bstr = flatten (sMap (fn a => sMap (fn b => (a,b)) bstr) astr)
  fun splice astr bstr = flatten (sMap (fn a => zip (constStream a) bstr) astr)
  fun filter f str = sMap valOf (#1 (splitp isSome (sMap f str)))
  fun find f astr = filter (fn a => if f a then SOME a else NONE) astr
  fun remove f astr = filter (fn a => if f a then NONE else SOME a) astr
  fun findOne f astr = (SOME (sHd (find f astr))) handle SHd => NONE
end

