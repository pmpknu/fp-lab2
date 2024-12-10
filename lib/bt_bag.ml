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

    type 'a bag = Leaf | Node of 'a bag * key * int * 'a bag

    let empty = Leaf
    let is_empty = function
        | Leaf -> true
        | _ -> false

    let rec add bag x = 
      match bag with
      | Leaf -> Node (Leaf, x, 1, Leaf)
      | Node (left, key, count, right) ->
          if x = key then
            Node (left, key, count + 1, right)
          else if Ord.compare x key < 0 then
            Node (add left x, key, count, right)
          else
            Node (left, key, count, add right x)
end
