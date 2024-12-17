open Alcotest

let test_add_case () =
  let module IntBag = Bt_bag.Make(struct
      type t = int
      let compare = compare
    end) in
  let bag = IntBag.empty in
  let bag = IntBag.add 1 bag in
  let bag = IntBag.add 2 bag in
  let bag = IntBag.add 1 bag in
  let bag = IntBag.add 3 bag in
  let bag = IntBag.add 2 bag in
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
  let bag = IntBag.add 1 bag in
  check bool "non-empty bag" false (IntBag.is_empty bag)

  let test_find_case () =
    let module IntBag = Bt_bag.Make(struct
        type t = int
        let compare = compare
      end) in
    let bag = IntBag.empty in
    let bag = IntBag.add 1 bag in
    let bag = IntBag.add 2 bag in
    let bag = IntBag.add 1 bag in
    let bag = IntBag.add 3 bag in
    let bag = IntBag.add 2 bag in
    check int "find 1" 2 (IntBag.find 1 bag);
    check int "find 2" 2 (IntBag.find 2 bag);
    check int "find 3" 1 (IntBag.find 3 bag);
    check_raises "find 4" Not_found (fun () -> ignore (IntBag.find 4 bag))

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
    (
      "test_find",
      [test_case "test_find" `Quick test_find_case]
    )
  ]
