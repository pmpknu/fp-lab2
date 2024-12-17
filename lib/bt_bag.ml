module type Bag = sig
    type key
    type 'a btree
    val empty : 'a btree
    val is_empty : 'a btree -> bool

    val add : key -> 'a btree -> 'a btree

    val elements : 'a btree -> key list

    val find : key -> 'a btree -> int

    val find_opt : key -> 'a btree -> int option

    val merge : 'a btree -> 'a btree -> 'a btree

    val remove : key -> 'a btree -> 'a btree

    val map : (key -> key) -> 'a btree -> 'a btree

    val filter : (key -> bool) -> 'a btree -> 'a btree

    val fold_left : ('a -> key -> int -> 'a) -> 'a -> 'a btree -> 'a

    val fold_right : (key -> int -> 'a -> 'a) -> 'a btree -> 'a -> 'a
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

    let merge t1 t2 =
      let t2_values = elements t2 in
      let add_to_tree t k = add k t in
      List.fold_left add_to_tree t1 t2_values

    let rec remove x = function
      | Empty -> Empty
      | Node {left; value; count; right} ->
          match Ord.compare x value with
          | 0 -> if count = 1 then merge left right else Node {left; value; count = count - 1; right}
          | n when n < 0 -> Node {left = remove x left; value; count; right}
          | _ -> Node {left; value; count; right = remove x right}

    let rec map f = function
      | Empty -> Empty
      | Node {left; value; count; right} as t ->
        let l' = map f left in
        let v' = f value in
        let r' = map f right in
        if left == l' && value == v' && right == r' then t
        else Node {left = l'; value = v'; count; right = r'}

    let rec filter p = function
      | Empty -> Empty
      | Node {left; value; count; right} as t ->
        let l' = filter p left in
        let pv = p value in
        let r' = filter p right in
        if pv then
          if left == l' && right == r' then t
          else Node {left = l'; value; count; right = r'}
        else merge l' r'

    let rec fold_right f s accu =
      match s with
        Empty -> accu
      | Node {left; value; count; right} ->
          fold_right f right (f value count (fold_right f left accu))

    let rec fold_left f accu s =
    match s with
    | Empty -> accu
    | Node {left; value; count; right} ->
        fold_left f (f (fold_left f accu left) value count) right
end