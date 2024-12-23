module IntBag = Bt_bag.Make (struct
    type t = int

    let compare = compare
  end)

let prop_reverse lst = lst = lst

open QCheck

let test =
  QCheck.Test.make ~count:1000 ~name:"reverse" (list small_int) (fun x -> prop_reverse x)
;;

QCheck_runner.run_tests ~verbose:true [ test ]
