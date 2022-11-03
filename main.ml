type regex =
| Empty
| Letter of char
| Epsilon
| Union of regex * regex
| Concat of regex * regex
| Star of regex 

type rpn =
| Element of regex

let e: regex = Concat(Epsilon, Union(Star(Concat(Union(Letter('b'), Star(Letter('a'))), Letter('c'))), Empty))

let s: string = "ab@c|*d@e|*"

let rec print_regex (exp: regex): unit =
  match exp with
  | Empty -> print_string "{}"
  | Epsilon -> print_string "ε"
  | Letter(c) ->
    let s = String.make 1 c in 
    print_string s
  | Union(e1, e2) ->
    begin
      print_string "(";
      print_regex e1;
      print_string "|";
      print_regex e2;
      print_string ")"
    end
  | Concat(e1, e2) ->
    begin
      print_string "(";
      print_regex e1;
      print_string ".";
      print_regex e2;
      print_string ")"
    end
  | Star(e) ->
    begin
      print_string "(";
      print_string "(";
      print_regex e;
      print_string ")";
      print_string "*";
      print_string ")"
    end

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
      if s.[i] = '{' then
        aux (Element(Empty)::[]) (i + 2)
      else if (s.[i] = '#') then
        aux (Element(Epsilon)::[]) (i + 1)
      else
        aux (Element(Letter(s.[i]))::[]) (i + 1)
    | Element(e)::[] ->
      if i = n then e
      else 
        if (s.[i] = '{') then
          aux (Element(Empty)::[]) (i + 2)
        else if (s.[i] = '8') then
          aux (Element(Epsilon)::[]) (i + 1)
        else if (s.[i] = '*') then
          aux (Element(Star(e))::[]) (i + 1)
        else
          aux (Element(Letter(s.[i]))::Element(e)::[]) (i + 1)
    | Element(e1)::Element(e2)::rest ->
      if (s.[i] = '|')  then
        aux ((Element(Union(e1, e2)))::rest) (i + 1)
      else if (s.[i] = '@') then 
        aux (Element(Concat(e1, e2))::rest) (i + 1)
      else if (s.[i] = '*') then
        aux (Element(Star(e1))::Element(e2)::rest) (i + 1)
      else if s.[i] = '{' then
        aux (Element(Empty)::Element(e1)::Element(e2)::rest) (i + 2)
      else if s.[i] = '8' then
        aux (Element(Epsilon)::Element(e1)::Element(e2)::rest) (i + 1)
      else
        aux (Element(Letter(s.[i]))::Element(e1)::Element(e2)::rest) (i + 1)
  in
  aux [] 0