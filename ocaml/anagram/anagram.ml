open Core.Std

let anagrams s l = 
  let sort s = String.lowercase s
               |> String.to_list
               |> List.sort ~cmp:(Char.ascending)
               |> String.of_char_list in
  let (@=@) s1 s2 = (sort s1) = (sort s2) 
                    && String.lowercase s1 <> String.lowercase s2 in
  List.filter ~f:((@=@) s) l
             
