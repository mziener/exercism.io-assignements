open Core.Std

let count s c = String.fold ~init:0 
    ~f:(fun i v -> if c = v then i+1 else i) s

let nucleotide_counts s = String.fold ~init:Char.Map.empty
    ~f:(fun i v -> 
        match Map.find i v with
        | None -> Map.add i ~key:v ~data:1
        | Some x -> Map.add i ~key:v ~data:(x+1)) 
    s
