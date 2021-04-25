module type Tree =
sig
  type 'a tree
  val insert : 'a -> 'a tree -> 'a tree
  val mem : 'a -> 'a tree -> bool
end

module BSTree : Tree =
struct
  type 'a tree =
    | Leaf
    | Node of 'a tree * 'a tree * 'a

  let rec mem ele = function
    | Leaf -> false
    | Node (l_chd, r_chd, v) ->
      if ele = v then true
      else if ele < v then mem ele l_chd
      else mem ele r_chd

  let rec insert ele = function
    | Leaf -> Node (Leaf, Leaf, ele)
    | Node (l_chd, r_chd, v) ->
      if ele < v then Node (insert ele l_chd, r_chd, v)
      else Node (l_chd, insert ele r_chd, v)
end

module RBTree : Tree =
struct
  type color = Red | Black
  type 'a tree =
    | Leaf
    | Node of color * 'a tree * 'a tree * 'a

  let rec mem ele = function
    | Leaf -> false
    | Node (_, l_chd, r_chd, v) ->
      if ele = v then true
      else if ele < v then mem ele l_chd
      else mem ele r_chd

  let balance = function
    | Black, Node (Red, Node (Red, a, b, x), c, y), d, z
    | Black, Node (Red, a, Node (Red, b, c, y), x), d, z
    | Black, a, Node (Red, Node (Red, b, c, y), d, z), x
    | Black, a, Node (Red, b, Node (Red, c, d, z), y), x ->
        Node (Red, Node (Black, a, b, x), Node (Black, c, d, z), y)
    | a, b, c, d -> Node (a, b, c, d)

  let insert ele tree =
    let rec insert = function
      | Leaf -> Node (Red, Leaf, Leaf, ele)
      | Node (color, l, r, v) as t ->
        if ele < v then balance (color, l, insert r, v)
        else if ele > v then balance (color, insert l, r, v)
        else t
    in
    match insert tree with
    | Leaf -> failwith "BAD!"
    | Node (_, l, r, v) -> Node (Black, l, r, v)
end
