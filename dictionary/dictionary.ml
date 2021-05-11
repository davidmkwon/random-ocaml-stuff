open Base

(* smol hashtable impl *)

type ('a, 'b) t =
  { mutable length: int
  ; buckets: ('a, 'b) Avltree.t array
  ; hash: 'a -> int
  ; compare: 'a -> 'a -> int
  }

let bucket_count = 17

let length t = t.length

let _find_bucket t k = (t.hash k) % bucket_count

let create ~hash ~compare =
  { length = 0
  ; buckets = Array.create ~len:bucket_count Avltree.empty
  ; hash = hash
  ; compare = compare
  }

let find t ~key =
  let bucket = Array.get t.buckets (_find_bucket t key) in
  Avltree.find bucket ~compare:t.compare key

let mem t ~key =
  let bucket = Array.get t.buckets (_find_bucket t key) in
  Avltree.mem bucket ~compare:t.compare key

let iter t ~f =
  Array.iter t.buckets ~f:(fun tree ->
    Avltree.iter tree ~f:(fun ~key ~data ->
      f key data;
    )
  )

let add t ~key ~data =
  let ind = _find_bucket t key in
  let bucket = Array.get t.buckets ind in
  let added = ref false in
  let n_tree = Avltree.add bucket
      ~replace:true
      ~compare:t.compare
      ~added:added
      ~key ~data in
  Array.set t.buckets ind n_tree;
  if !added then t.length <- t.length + 1

let remove t ~key =
  let ind = _find_bucket t key in
  let bucket = Array.get t.buckets ind in
  let removed = ref false in
  let n_tree = Avltree.remove bucket
      ~removed:removed
      ~compare:t.compare
      key in
  Array.set t.buckets ind n_tree;
  if !removed then t.length <- t.length - 1
