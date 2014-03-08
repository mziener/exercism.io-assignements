open Core.Std

let isSilence s = String.for_all ~f:(Char.is_whitespace) s

let isYelling s = 
  let letters = String.filter ~f:(Char.is_alpha) s in
  letters <> "" && String.for_all ~f:(Char.is_uppercase) letters 
                              
let isQuestion s = String.is_suffix s ~suffix:"?"

let response_for s = match s with
  | s when isSilence s
    -> "Fine. Be that way!"
  | s when isYelling s
    -> "Woah, chill out!"
  | s when isQuestion s -> "Sure."
  | _ -> "Whatever."



