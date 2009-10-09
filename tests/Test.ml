
(** Tests for FastRandom
    @author Sylvain Le Gall
  *)

open OUnit;;

let _ = 
  let test_of_vector keys = 
    ("[|"^
     (String.concat "; " 
        (Array.to_list 
           (Array.map 
              string_of_int 
              keys)))
     ^"|]") >::
    (fun () ->
       let exp_gen = 
         Random.State.make keys
       in
       let gen = 
         FastRandom.State.make keys
       in
         for i = 0 to 1024 * 1024 do 
           assert_equal 
             ~printer:(fun n -> Printf.sprintf "%d (rank %d)" n i)
             (Random.State.bits exp_gen)
             (FastRandom.State.bits gen)
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
       (List.map test_of_vector random_seeds))
;;

