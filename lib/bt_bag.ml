module type Bag = sig
    type key
    type 'a bag
    val empty : 'a bag
    val is_empty : 'a bag -> bool
end

module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module Make (Ord : Set.OrderedType) : Bag with type key = Ord.t = struct
    type key = Ord.t
    let compare = Ord.compare

    type 'a bag = Leaf | Node of 'a bag * key * int * 'a bag

    let empty = Leaf
    let is_empty = function
        | Leaf -> true
        | _ -> false
end
