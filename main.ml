let hash x = x
let compare = Stdlib.compare

let dict = Dictionary.create ~hash:hash ~compare:compare

let () = Dictionary.add dict ~key:1 ~data:3
let () = Dictionary.add dict ~key:4 ~data:5
let () = Dictionary.add dict ~key:9 ~data:10
let () = Dictionary.add dict ~key:1 ~data:13

let () = Printf.printf "dictionary length %d\n" (Dictionary.length dict)
let () = Dictionary.iter dict ~f:(fun k d ->
    Printf.printf "key: %d, val: %d\n" k d)
