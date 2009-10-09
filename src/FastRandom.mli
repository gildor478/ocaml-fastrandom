
(** Fast random number generator compatible with Random
    @author Sylvain Le Gall
  *)

module State :
sig 
  type t

  val make: ?buffer:int -> int array -> t

  val make_self_init: ?buffer:int -> unit -> t

  val bits: t -> int

  val int: t -> int -> int
end
;;

(** See Random.init. *)
val init : int -> unit

(** See Random.self_init *)
val self_init: unit -> unit

(** See Random.bits *)
val bits: unit -> int

(** See Random.int *)
val int: int -> int
