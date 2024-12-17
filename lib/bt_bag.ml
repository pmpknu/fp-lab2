module type Bag = sig
    type key
    type 'a btree
    val empty : 'a btree
    val is_empty : 'a btree -> bool

    val add : key -> 'a btree -> 'a btree

    val elements : 'a btree -> key list

    val find : key -> 'a btree -> int

    val find_opt : key -> 'a btree -> int option
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

    let rec add x bag = 
      match bag with
      | Empty -> Node {left = Empty; value = x; count = 1; right = Empty}
      | Node {left; value = key; count; right} ->
          if Ord.compare x key = 0 then
            Node {left; value = key; count = count + 1; right}
          else if Ord.compare x key < 0 then
            Node {left = add x left; value = key; count; right}
          else
            Node {left; value = key; count; right = add x right}

    let rec elements = function
      | Empty -> []
      | Node {left; value; count; right} ->
          (elements left) @ (List.init count (fun _ -> value)) @ (elements right)
  
    let rec find x = function
        Empty -> raise Not_found
      | Node {left; value; count; right} ->
          match Ord.compare x value with
          | 0 -> count
          | n when n < 0 -> find x left
          | _ -> find x right

    let rec find_opt x = function
        Empty -> None
      | Node {left; value; count; right} ->
          match Ord.compare x value with
          | 0 -> Some count
          | n when n < 0 -> find_opt x left
          | _ -> find_opt x right
end
