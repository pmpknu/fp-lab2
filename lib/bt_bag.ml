module type Bag = sig
    type key
    type 'a bag
    val empty : 'a bag
    val is_empty : 'a bag -> bool

    val add : 'a bag -> key -> 'a bag
    (* add bag x adds x to the bag, returning an element that can later be removed from the bag. add runs in constant time *)
end

module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module Make (Ord : Set.OrderedType) : Bag with type key = Ord.t = struct
    type key = Ord.t

    type 'a bag = Empty | Node of 'a bag * key * int * 'a bag

    let empty = Empty
    let is_empty = function
        | Empty -> true
        | _ -> false

    let rec add bag x = 
      match bag with
      | Empty -> Node (Empty, x, 1, Empty)
      | Node (left, key, count, right) ->
          if Ord.compare x key = 0 then
            Node (left, key, count + 1, right)
          else if Ord.compare x key < 0 then
            Node (add left x, key, count, right)
          else
            Node (left, key, count, add right x)
end
