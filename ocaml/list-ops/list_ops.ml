let rec foldl ~init ~f l = match l with 
   | [] -> init 
   | h :: t -> foldl ~init:(f init h) ~f t 

let fold = foldl

let reverse l = fold ~init:[] ~f:(fun i v -> v :: i) l

let foldr ~init ~f l = foldl ~init ~f:(fun i v -> f v i) (reverse l)

let length l = fold ~init:0 ~f:(fun i _ -> i+1) l

let map ~f l = foldr ~init:[] ~f:(fun v i -> (f v)::i) l

let append l k = foldr ~init:(k) ~f:(fun v i -> v :: i) (l) 

let concat l = foldr ~init:[] ~f:(append) l
                                     
let filter ~f l = foldr ~init:[] ~f:(fun v i -> if f v then v :: i else i) l
