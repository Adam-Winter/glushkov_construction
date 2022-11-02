type regex =
| Empty
| Letter of char
| Epsilon
| Union of regex * regex
| Concat of regex * regex
| Star of regex 

type rpn =
| Binary of string
| Unary of string
| Element of regex

let rec string_of_regex (exp: regex): string =
  match exp with
  | Empty -> "∅"
  | Epsilon -> "ε"
  | Letter(c) -> String.make 1 c 
  | Union(e1, e2) -> (string_of_regex e1) ^ (string_of_regex e2) ^ "|"
  | Concat(e1, e2) -> (string_of_regex e1) ^ (string_of_regex e2) ^ "@"
  | Star(e) -> (string_of_regex e) ^ "*"

let regex_of_string (s: string): regex =
  let n = String.length s in
  let rec aux (stack: rpn list) (i: int): regex =
    if List.length stack >= 2 then
      match stack with
      | Unary(e1)::Element(e2)::rest -> aux (Element(Star(e2))::rest) ()
      | Binary(e2)if (s.[i] = '|') || (s.[i] = '@') || (s.[i] = '*') then
    else 
      if i = n - 1 then
        match stack with
        | Element(e) -> e 
       