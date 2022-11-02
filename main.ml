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
    match stack with
    | [] ->
      if (s.[i] = '∅') then
        aux (Element(Empty)::[]) (i + 1)
      else if (s.[i] = 'ε') then
        aux (Element(Epsilon)::[]) (i + 1)
      else
        aux (Element(Letter(s.[i]))::[]) (i +1)
    | Element(e1)::Element(e2)::rest ->
      if (s.[i] = '|')  then
        aux Element(Union(e1, e2)::rest) (i + 1)
      else if (s.[i] = '@') then 
        aux Element(Concat(e1, e2)::rest) (i + 1)
      else if (s.[i] = '*') then
        aux Element(Star(e1)::Element(e2)::rest) (i + 1)
      else if s.[i] = '∅' then
        aux Element()
       