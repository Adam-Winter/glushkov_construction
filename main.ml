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

type state = int list

type transition = char option

type triplet = state * transition * state

let e: regex = Star(Concat(Union(Letter('b'), Star(Letter('a'))), Letter('c')))

let s: string = "ab@c|*d@e|*"

let rec union (s1: 'a list) (s2: 'a list): 'a list =
  match s1 with
  | [] -> s2
  | x::xs ->
    if not (List.mem x s2) then union xs (x::s2)
    else union xs s2

let index (elem: 'a) (l: 'a list) =
  let rec aux (count: int) (rest: 'a list) =
    match l with
    | [] -> failwith "Error: element not found"
    | e::es ->
      if elem = e then count else aux (count + 1) es
  in aux 0 l

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

let recognized_alphabet (a: automate): char list =
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
  aux [] (a.transitions.(0)) 0

let intials_list (a: automate): int list =
  let n = a.nb_etats in
  let rec aux (index: int) (acc: int list): int list =
    if index = (n - 1) then
      if a.initiaux.(n - 1) then (n - 1)::acc
      else acc
    else
      if a.initiaux.(index) then aux (index + 1) (index::acc)
      else aux (index + 1) acc
  in aux 0 []

let terminals_list (a: automate): int list =
  let n = a.nb_etats in
  let rec aux (index: int) (acc: int list): int list =
    if index = (n - 1) then
      if a.terminaux.(n - 1) then (n - 1)::acc
      else acc
    else
      if a.terminaux.(index) then aux (index + 1) (index::acc)
      else aux (index + 1) acc
  in aux 0 []

let rec next_state_from_int (auto: automate) (a: char) (q: int): int list = 
  let rec aux (rest: (char * int) list) (destinations: int list): int list =
    match rest with
    | [] -> destinations
    | (c, i)::elems ->
      if a = c then aux elems (i::destinations)
      else aux elems destinations
  in aux (auto.transitions.(q)) []

let rec next_state_from_state (auto: automate) (a: char) (q: state): state =
  match q with
  | [] -> []
  | i::is ->
    union (next_state_from_int auto a i) (next_state_from_state auto a is)

let explore_state (a: automate) (s: state) (alph: char list): triplet list =
  let rec aux (alphrest: char list) (acc: triplet list) =
    match alphrest with
    | [] -> acc
    | c::cs ->
      let sn: state = next_state_from_state a c s in 
      aux cs ((s, Some(c), sn)::acc)
  in aux alph []

let deterministic_transitions (a: automate): triplet list =
  let alph: char list = recognized_alphabet a in
  let inits: int list = intials_list a in
  let res: triplet list = [([], None, inits)] in
  let todo: triplet list = [([], None, inits)] in
  let rec explore_all (r: triplet list) (t: triplet list): triplet list =
    match t with
    | [] -> r
    | elem::elems ->
      match elem with
      | (lorig, c, ldest) ->
        let neighbors = explore_state a ldest alph in 
        explore_all (union neighbors r) (union neighbors elems)
  in explore_all res todo

let deterministic_states (a: automate): state list =
  let trans = deterministic_transitions a in
  let rec aux (rest: triplet list) (acc: state list) =
    match rest with
    | [] -> acc
    | (s1, c, s2)::rests ->
      if not (List.mem s2 acc) then aux rests (s2::acc)
      else aux rests acc
  in aux trans []

let transition_transform (tr: triplet list) (states: state list): (char * int) list array =
  let n = List.length states in
  let new_transitions: (char * int) list array = Array.make n [] in
  let rec aux (l: triplet list) =
    match l with
    | [] -> ()
    | (q, None, qp)::rest -> ()
    | (q, Some(s), qp)::rest ->
      let spot1 = index q states in
      let spot2 = index qp states in
      new_transitions.(spot1) <- ((s, spot2)::new_transitions.(spot1));
      aux rest
  in aux tr;
  new_transitions

let rec is_deterministic_terminal (s: state) (a: automate): bool =
  let terms = a.terminaux in 
  match s with
  | [] -> false
  | x::xs ->
    if terms.(x) then true
    else is_deterministic_terminal xs a

let deterministic_initials (a: automate) (states: state list): bool array =
  let n = List.length states in
  let inits = intials_list a in
  let new_init = Array.make n false in 
  let spotinit = index inits states in 
  new_init.(spotinit) <- true;
  new_init

let has_a_terminal (s: state) (a: automate): bool =
  let terms = a.terminaux in 
  let n = a.nb_etats in
  let b = ref false in
  for i = 0 to (n - 1) do 
    if (terms.(i) && List.mem i s) then b := true
  done;
  !b 

let deterministic_terminals (a: automate) (states: state list): bool array =
  let n = List.length states in
  let new_term = Array.make n false in
  let rec aux (rest: state list) =
    match rest with
    | [] -> ()
    | stt::stts ->
      begin
        if has_a_terminal stt a then 
          new_term.(index stt states) <- true;
        aux stts  
      end
  in aux states;
  new_term

let determinize (a: automate): automate =
  let transitions: triplet list = deterministic_transitions a in
  let states = deterministic_states a in
  let n = List.length states in
  let new_transitions = transition_transform transitions states in 
  let new_terms = deterministic_terminals a states in 
  let new_inits = deterministic_initials a states in
  let new_auto: automate = {nb_etats = n; initiaux = new_inits; terminaux = new_terms; transitions = new_transitions} in 
  new_auto