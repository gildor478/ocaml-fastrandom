
(* Copyright 2009 Sylvain Le Gall *)
(* Copyright 1996 INRIA *)

(** Fast random number generator compatible with Random
    @author Sylvain Le Gall, (initial: Damien Doligez)

    See OCaml standard library Random module for details.
  *)

external random_seed: unit -> int = "caml_sys_random_seed";;

module Low =
struct 
  type t 

  external init: unit -> unit = "caml_fastrandom_init"

  external create: unit -> t = "caml_fastrandom_create" 

  external reset: t -> int array -> unit = "caml_fastrandom_reset"

  external copy: t -> t = "caml_fastrandom_copy"

  external bits: t -> int = "caml_fastrandom_bits" "noalloc"

  external refill: t -> int array -> int = "caml_fastrandom_refill" "noalloc"

  external skip: t -> int -> unit = "caml_fastrandom_skip" "noalloc"

  let () = 
    init ()
  
  let full_init t seed =
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
      reset t st

end
;;

module State = 
struct
  open Bigarray

  type t = 
      {
        low:    Low.t;
        bits:   int array;
        len:    int;
        mutable idx: int;
      }

  let new_state ?(buffer=1024) () = 
    let buffer_len = 
      max 0 buffer
    in
      {
        low  = Low.create ();
        bits = Array.create buffer_len 0;
        idx  = buffer_len; (* Invalid so initialized at first call *)
        len  = buffer_len;
      }

  let full_init t seed =
    Low.full_init t.low seed;
    (* Invalidate buffer *)
    t.idx <- t.len

  let make ?buffer seed =
    let t =
      new_state ?buffer ()
    in
      Low.full_init t.low seed;
      t

  let make_self_init ?buffer () = 
    make ?buffer [|random_seed ()|]

  (* Returns 30 random bits as an integer 0 <= x < 1073741824 *)
  let rec bits t =
    if t.idx >= t.len then
      (
        t.idx <- 0;
        Low.refill t.low t.bits
      )
    else
      (
        let newval = 
          Array.unsafe_get t.bits t.idx
        in
          t.idx <- t.idx + 1;
          newval
      )

  let rec intaux t n =
    let r =
      bits t 
    in
    let v = 
      r mod n 
    in
      if r - v > 0x3FFFFFFF - n + 1 then 
        intaux t n 
      else 
        v

  let int t bound =
    if bound > 0x3FFFFFFF || bound <= 0 then 
      invalid_arg "Random.int"
    else 
      intaux t bound

  let skip t n =
    t.idx <- t.idx + n;
    if t.idx > t.len then
      (
        (* We are beyond end of current buffer *)
        Low.skip t.low (t.idx - t.len);
        t.idx <- t.len
      )

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
let skip n = State.skip default n;;
