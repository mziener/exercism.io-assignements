open Core.Std

let word_count s = 
  String.lowercase s                   
  |> String.split_on_chars ~on:['!';'&';'@';'$';'%';
                                '^';';';',';':';' ';'\'']
  |> List.filter ~f:(fun c -> c <> "")
  |> List.fold ~init:(String.Map.empty) 
               ~f:(fun i v -> 
                   match Map.find i v with
                   | None -> Map.add i ~key:v ~data:1
                   | Some x -> Map.add i ~key:v ~data:(x+1))
  
