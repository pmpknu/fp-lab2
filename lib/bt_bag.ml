module type Bag = sig
    type key
    type 'a btree
    val empty : 'a btree
    val is_empty : 'a btree -> bool

    val add : 'a btree -> key -> 'a btree

    val elements : 'a btree -> key list

end

module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module Make (Ord : Set.OrderedType) : Bag with type key = Ord.t = struct
    type key = Ord.t

    type 'a btree = Empty | Node of {left:'a btree; value:key; count:int; right:'a btree}

    let empty = Empty
    let is_empty = function
        | Empty -> true
        | _ -> false

    let rec add bag x = 
      match bag with
      | Empty -> Node {left = Empty; value = x; count = 1; right = Empty}
      | Node {left; value = key; count; right} ->
          if Ord.compare x key = 0 then
            Node {left; value = key; count = count + 1; right}
          else if Ord.compare x key < 0 then
            Node {left = add left x; value = key; count; right}
          else
            Node {left; value = key; count; right = add right x}

    let rec elements = function
      | Empty -> []
      | Node {left; value; count; right} ->
          (elements left) @ (List.init count (fun _ -> value)) @ (elements right)
end
