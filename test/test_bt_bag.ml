open Alcotest

let test_add_case () =
  let module IntBag = Bt_bag.Make(struct
      type t = int
      let compare = compare
    end) in
  let bag = IntBag.empty in
  let bag = IntBag.add bag 1 in
  let bag = IntBag.add bag 2 in
  let bag = IntBag.add bag 1 in
  let bag = IntBag.add bag 3 in
  let bag = IntBag.add bag 2 in
  let result = IntBag.elements bag in
  let expected = [1; 1; 2; 2; 3] in
  check (list int) "same elements" expected result

let test_is_empty_case () =
  let module IntBag = Bt_bag.Make(struct
      type t = int
      let compare = compare
    end) in
  let bag = IntBag.empty in
  check bool "empty bag" true (IntBag.is_empty bag);
  let bag = IntBag.add bag 1 in
  check bool "non-empty bag" false (IntBag.is_empty bag)

let () =
  run "bt_bag tests" [
    (
      "test_add",
      [test_case "test_add" `Quick test_add_case]
    );
    (
      "test_is_empty",
      [test_case "test_is_empty" `Quick test_is_empty_case]
    );
  ]
