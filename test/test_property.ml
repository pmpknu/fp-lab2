module IntBag = Bt_bag.Make (struct
    type t = int

    let compare = compare
  end)

let count = 1000
let generate_list = QCheck.(list int)
let generate_bag = QCheck.map IntBag.of_list generate_list

open IntBag

let is_equal_test =
  QCheck.Test.make ~count ~name:"is_equal correctness" generate_bag (fun bag ->
    bag = bag && IntBag.empty = IntBag.empty)
;;

let merge_empty_is_neutral =
  QCheck.Test.make
    ~count
    ~name:"forall bags: bag merge Empty = Empty merge bag = bag"
    generate_bag
    (fun bag ->
       let empty = IntBag.empty in
       let merged1 = IntBag.merge bag empty in
       let merged2 = IntBag.merge empty bag in
       if merged1 === bag && merged2 === bag
       then true
       else (
         let debugprint s b =
           Printf.printf
             s
             (String.concat ", " (List.map string_of_int (IntBag.elements b)))
         in
         debugprint "Failing bag elements    : %s\n" bag;
         debugprint "Failing MERGED1 elements: %s\n" merged1;
         debugprint "Failing MERGED2 elements: %s\n" merged2;
         false))
;;

let merge_is_associative =
  QCheck.Test.make
    ~count
    ~name:"forall bags: (a merge b) merge c = a merge (b merge c)"
    (QCheck.triple generate_bag generate_bag generate_bag)
    (fun (a, b, c) -> a >>= (b >>= c) === (a >>= b >>= c))
;;

let () =
  let is_equal_properties = List.map QCheck_alcotest.to_alcotest [ is_equal_test ] in
  let monoid_properties =
    List.map QCheck_alcotest.to_alcotest [ merge_empty_is_neutral; merge_is_associative ]
  in
  Alcotest.run
    "quickcheck"
    [ "monoid_properties", monoid_properties; "is_equal_properties", is_equal_properties ]
;;
