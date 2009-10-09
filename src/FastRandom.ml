
(* Copyright 2009 Sylvain Le Gall *)
(* Copyright 1996 INRIA *)

(** Fast random number generator compatible with Random
    @author Sylvain Le Gall, (initial: Damien Doligez)

    See OCaml standard library Random module for details.
  *)

type buffer_t = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t

external random_seed: unit -> int = "caml_sys_random_seed";;

module Low =
struct 
  type t 

  external init: unit -> unit = "caml_fastrandom_init"

  external create: int array -> t = "caml_fastrandom_create" 

  external copy: t -> t = "caml_fastrandom_copy"

  external bits: t -> int = "caml_fastrandom_bits" "noalloc"

  external refill: t -> buffer_t -> unit = "caml_fastrandom_refill" "noalloc"

  let () = 
    init ()
end
;;

module State = 
struct
  open Bigarray

  type t = 
      { 
        bits: buffer_t; 
        len:  int;

        mutable low: Low.t;
        mutable idx: int;
      }

  let new_state ?(buffer=1024) () = 
    let buffer = 
      max 1 buffer
    in
      {
        low  = Low.create (Array.make 55 0);
        bits = Array1.create int c_layout buffer;
        idx  = buffer; (* Invalid so initialized at first call *)
        len  = buffer;
      }

  let full_init s seed =
    let st = 
      Array.make 55 0
    in
    let combine accu x = 
      Digest.string (accu ^ string_of_int x) 
    in
    let extract d =
      (Char.code d.[0] + (Char.code d.[1] lsl 8) + (Char.code d.[2] lsl 16))
      lxor (Char.code d.[3] lsl 22)
    in
    let l = 
      Array.length seed 
    in
    let () =
      for i = 0 to 54 do
        st.(i) <- i;
      done
    in
    let accu = 
      ref "x" 
    in
      for i = 0 to 54 + max 55 l do
        let j = i mod 55 in
        let k = i mod l in
        accu := combine !accu seed.(k);
        st.(j) <- st.(j) lxor extract !accu;
      done;
      s.low <- Low.create st;
      s.idx <- s.len

  let make ?buffer seed =
    let res =
      new_state ?buffer ()
    in
      full_init res seed;
      res

  let make_self_init ?buffer () = 
    make ?buffer [|random_seed ()|]

  (* Returns 30 random bits as an integer 0 <= x < 1073741824 *)
  let rec bits s =
    let () = 
      if s.idx >= s.len then
        (
          Low.refill s.low s.bits;
          s.idx <- 0
        )
    in
    let newval = 
      Array1.get s.bits s.idx
    in
      s.idx <- s.idx + 1;
      newval

  let rec intaux s n =
    let r = bits s in
    let v = r mod n in
    if r - v > 0x3FFFFFFF - n + 1 then intaux s n else v

  let int s bound =
    if bound > 0x3FFFFFFF || bound <= 0
    then invalid_arg "Random.int"
    else intaux s bound

end
;;

let default = 
  State.make 
    [|
      509760043; 399328820; 99941072; 112282318; 611886020; 516451399;
      626288598; 337482183; 748548471; 808894867; 657927153; 386437385;
      42355480; 977713532; 311548488; 13857891; 307938721; 93724463;
      1041159001; 444711218; 1040610926; 233671814; 664494626; 1071756703;
      188709089; 420289414; 969883075; 513442196; 275039308; 918830973;
      598627151; 134083417; 823987070; 619204222; 81893604; 871834315;
      398384680; 475117924; 520153386; 324637501; 38588599; 435158812;
      168033706; 585877294; 328347186; 293179100; 671391820; 846150845;
      283985689; 502873302; 718642511; 938465128; 962756406; 107944131;
      192910970
    |]
;;

let bits () = State.bits default;;
let int bound = State.int default bound;;
let init seed = State.full_init default [|seed|];;
let self_init () = init (random_seed ());;
