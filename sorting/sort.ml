open Base

(**
 * [selection_sort lst] returns the sorted lst using the selection sort algo.
 * runs in O(n^2) time.
 *  [remove ele lst] returns lst with the first occurence of ele removed
 *  [lst_min lst] is the minimum element of lst *)
let selection_sort ~lst =
  let lst_min lst =
    let rec rec_lst_min prev = function
      | [] -> prev
      | x::tl ->
        let min_ele = min x prev in
        rec_lst_min min_ele tl
    in
    rec_lst_min Int.max_value lst
  in
  let rec remove ele = function
    | [] -> []
    | hd::tl -> if ele = hd then tl else hd::(remove ele tl)
  in
  let rec selection_sort = function
    | [] -> []
    | lst ->
      let min = lst_min lst in
      let rem_lst = remove min lst in
      min::(selection_sort rem_lst)
  in
  selection_sort lst

(**
 * [insertion_sort lst] returns the sorted list using the insertion sort algo.
 * runs in O(n^2) time
 *)
let insertion_sort ~lst =
  (*let insert lst l =
    let (left, right) = Caml.List.partition (fun x -> x < l) lst in
    left @ l::right*)
  let rec insert l lst =
    match lst with
    | [] -> [l]
    | hd::tl -> if hd > l then l::hd::tl else hd::(insert l tl)
  in
  List.fold_left lst ~init:[] ~f:(fun accu x -> insert x accu)

(**
 * [merge_sort lst] returns the sorted list using the merge sort algo.
 * runs in O(nlogn) time.
 *)
let merge_sort ~lst =
  let rec merge lst1 lst2 =
    match lst1, lst2 with
    | _, [] -> lst1
    | [], _ -> lst2
    | hd1::tl1, hd2::tl2 ->
      if hd1 < hd2 then hd1 :: merge tl1 (hd2::tl2)
      else hd2 :: merge (hd1::tl1) tl2
  in
  let rec merge_sort start_ind end_ind lst =
    if end_ind - start_ind = 1 then
      [Option.value_exn (List.nth lst start_ind)] else
    let mid_ind = (start_ind + end_ind) / 2 in
    let lst1 = merge_sort start_ind mid_ind lst in
    let lst2 = merge_sort mid_ind end_ind lst in
    merge lst1 lst2
  in
  merge_sort 0 (List.length lst) lst

(**
 * [quick_sort lst] returns the sorted list using the quick sort algo.
 * pivots are randomly chosen, and runs in average case O(nlogn) time.
 *)
let rec quick_sort ~lst =
  match lst with
  | [] -> []
  | lst ->
    let pivot_ind = Random.int_incl 0 (List.length lst - 1) in
    let pivot = List.nth_exn lst pivot_ind in
    let left_list = List.filter lst ~f:(fun x -> x < pivot) in
    let right_list = List.filter lst ~f:(fun x -> x > pivot) in
    let left_sort = quick_sort ~lst:left_list in
    let right_sort = quick_sort ~lst:right_list in
    left_sort @ pivot::right_sort
