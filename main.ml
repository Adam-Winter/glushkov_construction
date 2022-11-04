type regex = 
  | Letter of char
  | Point
  | Option of regex
  | Union of regex * regex
  | Concat of regex * regex
  | Star of regex

type automate = {
  nb_etats: int;
  initiaux: bool array;
  terminaux: bool array;
  transitions: (char * int) list array; 
}

type state = int array option

type transition = char option

type triplet = state * transition * state

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
    | elt::[] ->
      if i = len then
        elt
      else if  s.[i] = '|' || s.[i] = '@' then
        failwith "format regexp invalide "
      else if s.[i] = '*' then
        aux [Star(elt)] (i + 1) 
      else if s.[i] = '?' then
        aux [Option(elt)] (i + 1)
      else if s.[i] = '.' then
        aux [Point; elt] (i + 1)
      else 
        aux [(Letter(s.[i])); elt] (i + 1) 
    | e1::e2::rest -> 
      if i = len then
        failwith "format invalide"
      else if  s.[i] = '|' then
        aux ((Union(e2,e1))::rest) (i + 1)
      else if s.[i] = '@' then
        aux (Concat(e2, e1)::rest) (i + 1)
      else if s.[i] = '*' then
        aux (Star(e1)::e2::rest) (i + 1) 
      else if s.[i] = '?' then
        aux (Option(e1)::e2::rest) (i + 1)
      else if s.[i] = '.' then
        aux (Point::e1::e2::rest) (i + 1) 
      else 
        aux (Letter(s.[i])::e1::e2::rest) (i + 1) 
  in aux [] 0

let recognized_alphabet (a: automate): char array =
  let n: int = a.nb_etats in
  let rec aux (charlist: char list) (current: (char*int) list) (i: int): char list =
    match current with
    | [] ->
      if i = n - 1 then charlist
      else aux charlist (a.transitions.(i + 1)) (i + 1)
    | (c, x)::[] ->
      aux (c::charlist) (a.transitions.(i + 1)) (i + 1)
    | (c, x)::rest ->
      aux (c::charlist) rest i
  in
  let charlist = aux [] (a.transitions.(0)) 0 in
  let n: int = List.length charlist in
  let array: char array = Array.make n 'a' in
  let rec array_of_list (l: char list) (index: int): unit =
    match l with
    | [] -> ()
    | c::rest ->
      begin 
        array.(index) <- c;
        array_of_list rest (index + 1)
      end
  in array_of_list charlist 0 ;
  array 

let intials_array (a: automate): int array =
  let n = a.nb_etats in
  let rec initials_list (index: int) (acc: int list): int list =
    if index = (n - 1) then
      if a.initiaux.(n - 1) then (n - 1)::acc
      else acc
    else
      if a.initiaux.(index) then initials_list (index + 1) (index::acc)
      else initials_list (index + 1) acc
  in
  let l = initials_list 0 [] in
  let nb_initials = List.length l in
  let array: int array = Array.make nb_initials 0 in
  let rec array_of_list (l: int list) (index: int): unit =
    match l with
    | [] -> ()
    | c::rest ->
      begin 
        array.(index) <- c;
        array_of_list rest (index + 1)
      end
  in array_of_list l 0 ;
  array

let next_state_from_current_for_letter (a: char) (q: state): state =
  let rec aux (rest: (char * int) list) =
    match rest with
    | [] -> (Some())

let deterministic_transitions (a: automate): int array array =
  let n: int = a.nb_etats in
  let alph: char array = recognized_alphabet a in 
  let alphlen: int = Array.length alph in
  let inits: int array = intials_array a in
  let seen = (None, None, Some(inits))::[] in 
  let todo = (None, None, Some(inits))::[] in
  let explore_all (s: triplet list) (t: triplet list): triplet list =
    match t with
    | [] -> s
    | elem::elems ->
      match elem with
      | (q, c, qp) ->
        let temp: triplet list ref = ref [] in
        for i = 0 to (alphlen - 1) do 
          let newstate = next_state_from_current_for_letter (alph.(i)) qp in 
          if not List.mem seen newstate then
            temp <- newstate::(!temp)
        done;
        explore_all (temp @ seen) elems
  in
  explore_all seen todo