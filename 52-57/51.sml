open Lazy;

datatype 'a Stream_ = Cons of 'a * 'a Stream | Nil
  withtype 'a Stream = 'a Stream_ susp

exception EMPTY

signature QUEUE =
sig
  type 'a Queue

  val empty : 'a Queue
  val isEmpty : 'a Queue -> bool
  val snoc : 'a Queue * 'a -> 'a Queue
  val head : 'a Queue -> 'a
  val tail : 'a Queue -> 'a Queue
end

(* Problem:
  Wykonanie snocx, gdy lenr = lenf powoduje
  q qs (lenf, f, lenr, r) => q' as (lenf', f', 0, []), gdzie
    f  = $Cons(x, xs)
    f' = $Cons(x, rotate(...))

  w efekcie po wykonaniu ciągu operacji gdzie snocx₁...snocxₖ wymuszają rotacje
    otrzymujemy strukturę:
    fᵏ = $Cons(x, rotate₁(...(rotateₖ(xs)))
    gdzie konieczne jest wymuszenie rotacji by uzyskać dostęp do głowy

  Udowodnić, iż takie wymuszenie ma czas O(log n) w dowolnym przypadku.
  *)

structure BankersQueue : QUEUE =
struct
  type 'a Queue = int * 'a Stream * int * 'a list

  val empty = (0, $Nil , 0, [])
  fun isEmpty (lenf, _, _, _) = (lenf = 0)

    (* Φ  ( rotate(x, y, a) ) = 1 + Φ(x) + Φ(a)  | Φ(f) to ilosc rotacji w f : α Stream
        | ( Cons(_, x)      ) = Φ(x)
        | ( Nil*            ) = 0                    *)

(* Teza: dla ∀ q as (lenf, f, lenr, r) | Φ(f) = O(log |q|) *)

  fun rotate ($(Nil), [y], a) = $(Cons(y, a))
    | rotate ($(Cons(x, xs)), y::ys, a) = $(Cons(x, rotate(xs, ys, $(Cons(y, a)))))
    (* wymuszenie rotate: Φ(q') ≤ Φ(q)               *)


  fun check (q as (lenf, f, lenr, r)) =
    if lenr <= lenf then q else (lenf+lenr, rotate(f,r, $(Nil)), 0, [])
    (* Φ(q') = Φ(q)
        lub Φ(q') = Φ(q) + 1 ale wowczas |q'| = 2|q|  *)

  fun snoc ((lenf, f, lenr, r), x) = check (lenf, f, lenr+1, x::r)
    (* snocx q = check q' = q''
       Φ(q)' = Φ(q)
       a check q' zachowa niezmiennik                *)

  fun head (lenf, $(Nil), lenr, r) = raise EMPTY
  | head (lenf, $(Cons (x, f)), lenr, r) = x

  fun tail (lenf, $(Nil), lenr, r) = raise EMPTY
  | tail (lenf, $(Cons (x, f)), lenr, r) = check (lenf-1, f, lenr, r)
    (* tail q = check q' = q''
        Φ(q') = Φ(q)
       a check q' zachowa niezmiennik                *)

    (* Dodatkowe twierdzenie: obliczenie glowy E₁ E₂ .. Eₖ(...),
        gdzie E₁ : α Stream to f (front) w pewnej kolejce q : α Queue
      wymaga tylko k krokow *)

    (* Indukcja po k:
        case Eₖ of
          rotate(Eₖ₋₁, _, _) => 1 + k - 1
          Cons(_, Eₖ₋₁) => 1
       Baza:
          Nil => 1
      *)

    (* Ponieważ istnieje tylko O(log |q|) rotate'ów w całej strukturze,
        a obliczenie wyrażenia zawierającego k rotate'ów trwa co najwyżej k,
          to obliczenie głowy w dowolnym momencie nie może trwać więcej niż k.

        Stąd head: O(1),
          tail, snoc: ~2log n (zakładając obliczenie głowy przed i po)
      *)

end
