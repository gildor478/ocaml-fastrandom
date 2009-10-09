
(** Fast random number generator compatible with Random
    @author Sylvain Le Gall
  *)

module State :
sig 
  type t

  val make: ?buffer:int -> int array -> t

  val bits: t -> int

  val int: t -> int -> int
end
;;

val bits: unit -> int

val int: int -> int
