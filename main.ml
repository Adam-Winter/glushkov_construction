type regex = 
  | Letter of char
  | Point
  | Option of regex
  | Union of regex * regex
  | Concat of regex * regex
  | Star of regex
      
type regex2 = 
  | Letter of int 
  | Option of regex2
  | Union of regex2 * regex2
  | Concat of regex2 * regex2
  | Star of regex2

      

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
    
let gestion_point (i: int) (save: (int * char) list)=
  (* on considère le point comme l'union de toutes les lettres*) 
  let rec aux (e: regex2) (j: int) (save: (int * char) list)=
    if j = 128 then
      e, i + 128, save 
    else 
      aux (Union( Letter(i + j), e)) (j+1) (((i+j), (Char.chr j))::save)
  in 
  aux (Letter(i)) 1 ((i, (Char.chr 0))::save)
        
    
let regex_of_string_linearized (s: string): regex2 * ((int * char) list) =
  let len = String.length s in
  let rec aux (stack: regex2 list) (i: int) (save: (int * char) list) (j: int)  =
    match stack with 
    | [] -> 
        if i = len then
          failwith "regexp vide"
        else if s.[i] = '*' || s.[i] = '?' || s.[i] = '|' || s.[i] = '@' then
          failwith "format regexp invalide "
        else if s.[i] = '.' then
          let e, new_j, new_save = gestion_point i save in
          
          aux [e] (i+1) new_save new_j
        else 
          aux [Letter(i)] (i+1) ((i, s.[i])::save) (j+1)
    |elt::[] ->
        if i = len then
          elt, save
        else if  s.[i] = '|' || s.[i] = '@' then
          failwith "format regexp invalide "
        else if s.[i] = '*' then
          aux [Star(elt)] (i+1) save j
        else if s.[i] = '?' then
          aux [Option(elt)] (i+1) save j
        else if s.[i] = '.' then
          let e, new_j, new_save = gestion_point i save in
          aux [e; elt] (i+1) new_save new_j
        else 
          aux [(Letter(j));elt] (i+1) ((j, s.[i])::save) (j+1)
    |e1::e2::rest -> 
        if i = len then
          failwith "format invalide"
        else if  s.[i] = '|' then
          aux ((Union(e2,e1))::rest) (i+1) save j
        else if s.[i] = '@' then
          aux (Concat(e2, e1)::rest) (i+1) save j
        else if s.[i] = '*' then
          aux (Star(e1)::e2::rest) (i+1) save j
        else if s.[i] = '?' then
          aux (Option(e1)::e2::rest) (i+1) save j
        else if s.[i] = '.' then
          let e, new_j, new_save = gestion_point i save in
          aux (e::e1::e2::rest) (i+1) new_save new_j
        else 
          aux (Letter(j)::e1::e2::rest) (i+1) ((j, s.[i])::save) (j+1)
  in aux [] 0 [] 0
    
let rec get_P (e: regex2) : int list = 
  match e with
  | Letter(i) -> [i]
  | Union(e1, e2) -> (get_P e1)@(get_P e2) (* complexité pas folle *) 
  | Concat(Star(e1), e2) -> (get_P e1)@(get_P e2)
  | Star(e1) |Option(e1) |Concat(e1,_)-> get_P e1 
                                           
let rec get_S (e: regex2) : int list = 
  match e with
  | Letter(i) -> [i]
  | Union(e1, e2) -> (get_S e1)@(get_S e2) (* complexité pas folle *) 
  | Concat(e1, Star(e2)) -> (get_S e1)@(get_S e2)
  | Star(e1) |Option(e1) |Concat(_,e1)-> get_S e1
                                           
let produit (x: int) (l: int list): (int * int) list =
  let rec aux l acc =
    match l with
    | [] -> acc
    | y::ys -> aux ys ((x, y)::acc)
  in aux l []
                                           
let produit_cartesien (l1 : int list) (l2: int list): (int * int) list =
  let rec aux l1 l2 acc=
    match l1 with
    | [] -> acc
    | x::xs -> aux xs l2 ((produit x l2)@acc) 
  in aux l1 l2 []
                                           
let rec get_F (e: regex2): (int * int) list =
  match e with
  | Letter(i) -> []
  | Union(e1, e2) -> (get_F e1)@(get_F e2)
  | Concat(e1, e2) -> (get_F e1)@(get_F e2)@(produit_cartesien (get_S e1) (get_P e2))
  | Star(e1) -> (get_F e1)@(produit_cartesien (get_S e1) (get_P e1))
  | Option(e1) -> get_F e1
                    
let rec decoder (x: int) (codage: (int * char) list): char=
  match codage with
  | [] -> failwith "code invalide"
  | (i, c)::rest ->
      if i = x then
        c 
      else 
        decoder x rest
                    
let rec initialiser_automate_pref (a: automate) (pref: int list) codage =
  match pref with
  | [] -> ()
  | x::xs -> 
      a.transitions.(a.nb_etats -1) <- ((decoder x codage), x)::a.transitions.(a.nb_etats -1) ;
      initialiser_automate_pref a xs codage
        
let rec initialiser_automate_suff (a: automate) (suff: int list) =
  match suff with
  | [] -> ()
  | x::xs -> 
      a.terminaux.(x) <- true ;
      initialiser_automate_suff a xs 
        
let rec creation_arcs (a: automate)(fact: (int * int) list) (codage: (int * char) list) =
  match fact with
  | [] -> ()
  | (x1, x2)::xs ->
      begin
        a.transitions.(x1) <- ((decoder x2 codage), x2)::a.transitions.(x1);
        creation_arcs a xs codage
      end
                                               
let glushkov(e: regex2) (codage: (int * char) list): automate =
  let len = List.length codage +1 in
  let res = {nb_etats = len; initial = Array.make len false; terminaux =  Array.make len false; transitions = Array.make len [] } in
  let pref = get_P e in
  let suff = get_S e in
  let fact = get_F e in
  begin
    initialiser_automate_pref res pref codage;
    initialiser_automate_suff res suff;
    creation_arcs res fact codage;
    res.initial.(res.nb_etats -1) <- true
  end;
  res
