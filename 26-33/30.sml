structure Susp =
struct
  datatype 'a value = NotYet of (unit -> 'a) | Just of 'a
  withtype 'a susp = 'a value ref
  fun force r = case !r of (Just a) => a
    | (NotYet f) =>
        let
          val v = (f ())
          val _ = r := Just v
        in v
        end
  fun delay f = ref (NotYet f)
end

structure InStream =
struct
  open Susp
  datatype inStream = Stream of (TextIO.elem option * inStream) susp | EOS

  fun makeStdIn () = Stream(delay( fn _ =>
    (TextIO.input1 TextIO.stdIn, makeStdIn ())))

  val stdIn = makeStdIn ()

  fun read (Stream str) = force str
end
