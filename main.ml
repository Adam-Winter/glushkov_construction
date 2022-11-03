type regex = 
  | Letter of char
  | Point
  | Option of regex
  | Union of regex * regex
  | Concat of regex * regex
  | Star of regex

type automate = {
  nb_etats : int;
  initial : bool array;
  terminaux : bool array;
  transitions : (char * int) list array
}
      
let e: regex = Star(Concat(Union(Letter('b'), Star(Letter('a'))), Letter('c')))

let s: string = "ab@c|*d@e|*"

let rec print_regex (exp: regex): unit =
  match exp with
  | Point ->
      print_string "."
  | Option(e) ->
      begin
        print_string "(";
        print_string "(";
        print_regex e;
        print_string ")";
        print_string "?";
        print_string ")"
      end
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
  | Point -> "."
  | Option(e) -> (string_of_regex e) ^ "?"
  | Letter(c) -> String.make 1 c 
  | Union(e1, e2) -> (string_of_regex e1) ^ (string_of_regex e2) ^ "|"
  | Concat(e1, e2) -> (string_of_regex e1) ^ (string_of_regex e2) ^ "@"
  | Star(e) -> (string_of_regex e) ^ "*"
    
let regex_of_string (s: string): regex =
  let len = String.length s in
  let rec aux (stack: regex list) (i: int) =
    match stack with 
    | [] -> 
        if i = len then
          failwith "regexp vide"
        else if s.[i] = '*' || s.[i] = '?' || s.[i] = '|' || s.[i] = '@' then
          failwith "format regexp invalide "
        else if s.[i] = '.' then
          aux [Point] (i+1)
        else 
          aux [Letter(s.[i])] (i+1)
    |elt::[] ->
        if i = len then
          elt
        else if  s.[i] = '|' || s.[i] = '@' then
          failwith "format regexp invalide "
        else if s.[i] = '*' then
          aux [Star(elt)] (i+1) 
        else if s.[i] = '?' then
          aux [Option(elt)] (i+1)
        else if s.[i] = '.' then
          aux [Point; elt] (i+1)
        else 
          aux [(Letter(s.[i]));elt] (i+1) 
    |e1::e2::rest -> 
        if i = len then
          failwith "format invalide"
        else if  s.[i] = '|' then
          aux ((Union(e2,e1))::rest) (i+1)
        else if s.[i] = '@' then
          aux (Concat(e2, e1)::rest) (i+1)
        else if s.[i] = '*' then
          aux (Star(e1)::e2::rest) (i+1) 
        else if s.[i] = '?' then
          aux (Option(e1)::e2::rest) (i+1)
        else if s.[i] = '.' then
          aux (Point::e1::e2::rest) (i+1) 
        else 
          aux (Letter(s.[i])::e1::e2::rest) (i+1) 
  in aux [] 0
let test = regex_of_string "ab?@c|"
