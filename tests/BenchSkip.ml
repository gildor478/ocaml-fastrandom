
(** Benchmark random number skipping 
    @author Sylvain Le Gall
  *)

open Benchmark;;

let skip_count =
  128
;;

let count = 
  (1024 * 1024) / skip_count
;;

let ignore_int : int -> unit = ignore
;;

let seed = 
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

let s =
  FastRandom.State.make ~buffer:0 seed
;;

let res = 
  throughputN 
    3
    (
      [
       "FastRandom.skip_no_buffer", 
       (fun () ->
          for i = 1 to count do
            FastRandom.State.skip s skip_count;
            ignore_int (FastRandom.State.bits s)
          done),
       ();

       "FastRandom.skip", 
       (fun () ->
          for i = 1 to count do
            FastRandom.State.skip s skip_count;
            ignore_int (FastRandom.State.bits s)
          done),
       ();

       "FastRandom.no_skip",
       (fun () ->
          for i = 1 to count do
            for j = 1 to skip_count do
              ignore_int (FastRandom.State.bits s)
            done;
            ignore_int (FastRandom.State.bits s)
          done),
       ();

       "Random.skip",
       (fun () ->
          for i = 1 to count do
            for j = 1 to skip_count do
              ignore_int (Random.bits ())
            done;
            ignore_int (Random.bits ())
          done),
       ();
      ]
    )
;;

print_newline();;
tabulate res;;

