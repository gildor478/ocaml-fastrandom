
(** Tests for FastRandom
    @author Sylvain Le Gall
  *)

open OUnit;;

let _ = 

  let ignore_int : int -> unit =
     ignore 
  in

  let string_of_seed seed = 
    "[|"^
    (String.concat "; " 
       (Array.to_list 
          (Array.map 
             string_of_int 
             seed)))
    ^"|]"
  in

  let bracket_state seed f =
    bracket
      (fun () ->
         Random.State.make seed,
         FastRandom.State.make seed)
      f
      ignore
  in

  let test_generate_of_vector seed = 
    (string_of_seed seed) >::
    bracket_state seed
    (fun (exp_gen, gen) ->
       for i = 0 to 8192 do 
         assert_equal 
           ~printer:(fun n -> Printf.sprintf "%d (rank %d)" n i)
           (Random.State.bits exp_gen)
           (FastRandom.State.bits gen)
       done)
  in

  let test_skip_of_vector seed =
    (string_of_seed seed) >::
    bracket_state seed
    (fun (exp_gen, gen) ->
       let my_skip n = 
         for i = 1 to n do
           ignore_int (Random.State.bits exp_gen)
         done
       in
       let n = 
         1 
       in
         for i = 0 to 16384 do 
           my_skip n;
           FastRandom.State.skip gen n;
           assert_equal 
             ~printer:(fun n -> Printf.sprintf "%d (rank %d)" n i)
             (Random.State.bits exp_gen)
             (FastRandom.State.bits gen);
         done)
  in

  let random_seeds = 
    Random.init 123456;
    Array.to_list
      (Array.init
         20
         (fun i -> [|Random.bits (); Random.bits ()|]))
  in
    run_test_tt_main 
      ("fastrandom" >:::
       [
         "generate" >::: 
         (List.map test_generate_of_vector 
            random_seeds);

         "skip" >::: 
         (List.map test_skip_of_vector
            random_seeds);
       ])
;;

