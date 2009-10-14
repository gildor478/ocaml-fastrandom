
(** Fast random number generator compatible with Random
    @author Sylvain Le Gall
  *)

module State :
sig 
  type t

  (** See {!FastRandom.init} *)
  val make: ?buffer:int -> int array -> t

  (** See {!FastRandom.self_init} *)
  val make_self_init: ?buffer:int -> unit -> t

  (** See {!FastRandom.bits} *)
  val bits: t -> int

  (** See {!FastRandom.int} *)
  val int: t -> int -> int

  (** See {!FastRandom.skip} *)
  val skip: t -> int -> unit
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

(** [skip n] Skip the [n] [FastRandom.bits] call.
  *)
val skip: int -> unit
