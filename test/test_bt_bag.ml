open Alcotest

module IntBag = Bt_bag.Make (struct
    type t = int

    let compare = compare
  end)

module CharBag = Bt_bag.Make (struct
    type t = char

    let compare = compare
  end)

let test_add_case () =
  let bag = IntBag.empty in
  let bag = IntBag.add 1 bag in
  let bag = IntBag.add 2 bag in
  let bag = IntBag.add 1 bag in
  let bag = IntBag.add 3 bag in
  let bag = IntBag.add 2 bag in
  let result = IntBag.elements bag in
  let expected = [ 1; 1; 2; 2; 3 ] in
  check (list int) "same elements in add" expected result
;;

let test_is_empty_case () =
  let bag = IntBag.empty in
  check bool "empty bag" true (IntBag.is_empty bag);
  let bag = IntBag.add 1 bag in
  check bool "non-empty bag" false (IntBag.is_empty bag)
;;

let test_find_case () =
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
;;

let test_find_opt_case () =
  let bag = IntBag.empty in
  let bag = IntBag.add 1 bag in
  let bag = IntBag.add 2 bag in
  let bag = IntBag.add 1 bag in
  let bag = IntBag.add 3 bag in
  let bag = IntBag.add 2 bag in
  check (option int) "find 1" (Some 2) (IntBag.find_opt 1 bag);
  check (option int) "find 2" (Some 2) (IntBag.find_opt 2 bag);
  check (option int) "find 3" (Some 1) (IntBag.find_opt 3 bag);
  check (option int) "find 4" None (IntBag.find_opt 4 bag)
;;

let test_merge () =
  let bag1 = IntBag.empty in
  let bag1 = IntBag.add 1 bag1 in
  let bag1 = IntBag.add 2 bag1 in
  let bag1 = IntBag.add 1 bag1 in
  let bag1 = IntBag.add 3 bag1 in
  let bag1 = IntBag.add 2 bag1 in
  let bag2 = IntBag.empty in
  let bag2 = IntBag.add 1 bag2 in
  let bag2 = IntBag.add 2 bag2 in
  let bag2 = IntBag.add 2 bag2 in
  let bag2 = IntBag.add 3 bag2 in
  let bag2 = IntBag.add 3 bag2 in
  let bag = IntBag.merge bag1 bag2 in
  let result = IntBag.elements bag in
  let expected = [ 1; 1; 1; 2; 2; 2; 2; 3; 3; 3 ] in
  check (list int) "same elements in merge" expected result
;;

let test_remove () =
  let bag = IntBag.empty in
  let bag = IntBag.add 1 bag in
  let bag = IntBag.add 2 bag in
  let bag = IntBag.add 1 bag in
  let bag = IntBag.add 3 bag in
  let bag = IntBag.add 2 bag in
  let bag = IntBag.remove 1 bag in
  let result = IntBag.elements bag in
  let expected = [ 1; 2; 2; 3 ] in
  check (list int) "same elements in remove" expected result
;;

let test_map () =
  let bag = IntBag.empty in
  let bag = IntBag.add 1 bag in
  let bag = IntBag.add 2 bag in
  let bag = IntBag.add 1 bag in
  let bag = IntBag.add 3 bag in
  let bag = IntBag.add 2 bag in
  let bag = IntBag.map (fun x -> x + 1) bag in
  let result = IntBag.elements bag in
  let expected = [ 2; 2; 3; 3; 4 ] in
  check (list int) "same elements in map" expected result
;;

let test_filter () =
  let bag = IntBag.empty in
  let bag = IntBag.add 1 bag in
  let bag = IntBag.add 2 bag in
  let bag = IntBag.add 1 bag in
  let bag = IntBag.add 3 bag in
  let bag = IntBag.add 2 bag in
  let bag = IntBag.filter (fun x -> x mod 2 = 0) bag in
  let result = IntBag.elements bag in
  let expected = [ 2; 2 ] in
  check (list int) "same elements in filter" expected result
;;

let test_fold_right () =
  let bag = IntBag.empty in
  let bag = IntBag.add 1 bag in
  let bag = IntBag.add 2 bag in
  let bag = IntBag.add 1 bag in
  let bag = IntBag.add 3 bag in
  let bag = IntBag.add 2 bag in
  let result = IntBag.fold_right (fun key count acc -> acc + (key * count)) bag 0 in
  let expected = (1 * 2) + (2 * 2) + (3 * 1) in
  check int "same elements in fold_right" expected result
;;

let test_fold_right_zero_elements () =
  let bag = IntBag.empty in
  let result = IntBag.fold_right (fun key count acc -> acc + (key * count)) bag 0 in
  let expected = 0 in
  check int "same elements in fold right with no elements" expected result
;;

let test_fold_left_two_elements () =
  let bag = IntBag.empty in
  let bag = IntBag.add 1 bag in
  let bag = IntBag.add 2 bag in
  let result = IntBag.fold_left (fun acc key count -> acc - (key * count)) 100 bag in
  let expected = 100 - 1 - 2 in
  check
    int
    "same elements in fold_left with two elements with substraction function"
    expected
    result
;;

let test_fold_left_three_elements () =
  let bag = IntBag.empty in
  let bag = IntBag.add 1 bag in
  let bag = IntBag.add 2 bag in
  let bag = IntBag.add 3 bag in
  let result = IntBag.fold_left (fun acc key count -> acc - (key * count)) 100 bag in
  let expected = 100 - 1 - 2 - 3 in
  check
    int
    "same elements in fold_left with three elements with substraction function"
    expected
    result
;;

let test_fold_right_two_elements () =
  let bag = IntBag.empty in
  let bag = IntBag.add 1 bag in
  let bag = IntBag.add 2 bag in
  let result = IntBag.fold_right (fun acc key count -> acc - (key * count)) bag 100 in
  let expected = 1 - (2 - 100) in
  check
    int
    "same elements in fold_right with two elements with substraction function"
    expected
    result
;;

let test_fold_right_three_elements () =
  let bag = IntBag.empty in
  let bag = IntBag.add 1 bag in
  let bag = IntBag.add 2 bag in
  let bag = IntBag.add 3 bag in
  let result = IntBag.fold_right (fun acc key count -> acc - (key * count)) bag 100 in
  let expected = 1 - (2 - (3 - 100)) in
  check
    int
    "same elements in fold_right with three elements with substraction function"
    expected
    result
;;

let test_fold_left () =
  let bag = IntBag.empty in
  let bag = IntBag.add 1 bag in
  let bag = IntBag.add 2 bag in
  let bag = IntBag.add 1 bag in
  let bag = IntBag.add 3 bag in
  let bag = IntBag.add 2 bag in
  let result = IntBag.fold_left (fun acc key count -> acc + (key * count)) 0 bag in
  let expected = (1 * 2) + (2 * 2) + (3 * 1) in
  check int "same elements" expected result
;;

let test_fold_left_right_difference_case () =
  let bag = IntBag.empty in
  let bag = IntBag.add 5 bag in
  let bag = IntBag.add 10 bag in
  let bag = IntBag.add 15 bag in
  let result_left = IntBag.fold_left (fun acc key count -> acc - (key * count)) 0 bag in
  let result_right = IntBag.fold_right (fun acc key count -> acc - (key * count)) bag 0 in
  let expected_left = 0 - 5 - 10 - 15 in
  let expected_right = 5 - (10 - (15 - 0)) in
  check int "same left" expected_left result_left;
  check int "same right" expected_right result_right
;;

let test_of_list () =
  let bag = IntBag.of_list [ 1; 2; 1; 3; 2 ] in
  let result = IntBag.elements bag in
  let expected = [ 1; 1; 2; 2; 3 ] in
  check (list int) "same elements in of_list" expected result

let () =
  run
    "bt_bag tests"
    [ "test_add", [ test_case "test_add" `Quick test_add_case ]
    ; "test_is_empty", [ test_case "test_is_empty" `Quick test_is_empty_case ]
    ; ( "test_find"
      , [ test_case "test_find" `Quick test_find_case
        ; test_case "test_find_opt" `Quick test_find_opt_case
        ] )
    ; "test_merge", [ test_case "test_merge" `Quick test_merge ]
    ; "test_remove", [ test_case "test_remove" `Quick test_remove ]
    ; "test_map", [ test_case "test_map" `Quick test_map ]
    ; "test_filter", [ test_case "test_filter" `Quick test_filter ]
    ; ( "test_fold"
      , [ test_case "test_fold_right" `Quick test_fold_right
        ; test_case "test_fold_right_zero_elements" `Quick test_fold_right_zero_elements
        ; test_case "test_fold_right_two_elements" `Quick test_fold_right_two_elements
        ; test_case "test_fold_right_three_elements" `Quick test_fold_right_three_elements
        ; test_case "test_fold_left_two_elements" `Quick test_fold_left_two_elements
        ; test_case "test_fold_left_three_elements" `Quick test_fold_left_three_elements
        ; test_case "test_fold_left" `Quick test_fold_left
        ; test_case
            "test_fold_left_right_difference"
            `Quick
            test_fold_left_right_difference_case
        ] )
    ; "test_of_list", [ test_case "test_of_list" `Quick test_of_list ]
    ]
;;
