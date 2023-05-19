(* Rohan Kallur *)
(* I Pledge my honor that I have abided by the Stevens Honor System. *)

(* 0 = Pen Down 
   1 = Pen Up
   2 = Move North
   3 = Move East
   4 = Move South
   5 = Move West *)

let square = [0; 2; 2; 3; 3; 4; 4; 5; 5; 1];;
let letter_e = [0;2;2;3;3;5;5;4;3;5;4;3;3;5;5;1];;

(* MAP FUNCTION *)
let rec map f l =
  match l with
  |[] -> []
  |h::t -> f h :: map f t

(* FOLD FUNCTION *)
let rec fold_right f l a =
  match l with
  |[] -> []
  |(h::t) -> f h (fold_right f t a)

(* Mirror Function *)
let mirror_helper x =
  if x=0 then 0
  else if x=1 then 1
  else if x>3 then x-2
  else x+2 

let mirror_image : int list -> int list =
  fun l ->
    map (mirror_helper) l

(* ROTATE 90 LETTER FUNCTION *)
let rotate_letter_helper x =
  if x=0 then 0
  else if x=1 then 1
  else if x=5 then 2
  else x+1

let rotate_90_letter : int list -> int list =
  fun l ->
    map (rotate_letter_helper) l
(* ROTATE 90 WORD FUNCTION *)
let rotate_90_word : int list list -> int list list =
  fun l ->
    map (rotate_90_letter) l


(* REPEAT FUNCTION*)
let rec repeat : int -> 'a -> 'a list =
  fun n x ->
    match n with
    |0 -> []
    |m -> x :: repeat (n-1) x

(* PANTOGRAPH FUNCTION*)
let rec panto_repeat : int -> 'a -> 'a list =
  fun n x ->
    match n with
    |0 -> []
    |m -> 
      if x=0 then x :: panto_repeat 0 x
      else if x=1 then x :: panto_repeat 0 x
      else x :: panto_repeat (n-1) x

let rec append = 
  fun l ->
    match l with
    |[] -> []
    |h::t -> h @ append t

let pantograph : int -> int list -> int list = 
  fun n p ->
    append (map (panto_repeat n) p)

let rec pantograph_nm : int -> int list -> int list =
  fun n p ->
    match p with
    |[] -> []
    |h::t ->
      if h=0 then h :: pantograph_nm n t
      else if h=1 then [h]
      else repeat n h @ pantograph_nm n t

let rec pantograph_f =
  fun n p ->
    failwith"idk if this is optional or not"  
      
(* COVERAGE FUNCTION *)
let cover_helper c x =
  match c with
  |(k, v) -> 
    if (x=0) then (k, v)
    else if (x=1) then (k, v)
    else if (x=2) then (k, v+1)
    else if (x=3) then (k+1, v)
    else if (x=4) then (k, v-1)
    else (k-1, v)

let rec coverage : int*int -> int list -> (int*int) list =
  fun c l ->
    match l with
    |[] -> [c]
    |h::t ->
      c :: coverage (cover_helper c h) t
      
(* COMPRESS FUNCTION *)

let rec progression : int list -> int -> int list =
  fun l n ->
    match l with
    |[] -> []
    |h::t ->
      if h=n then progression t n
      else l

let rec counter : int list -> int -> int -> int =
  fun l n x ->
    match l with
    |[] -> 1
    |h::t ->
      if h=n then counter t n x+1
      else x

let rec compress : int list -> (int*int) list =
  fun l ->
    match l with
    |[] -> []
    |h::t ->
      (h, (counter t h 1)) :: compress (progression t h)

(* UNCOMPRESS FUNCTION *)
let rec uncompress : (int*int) list -> int list =
  fun l ->
    match l with
    |[] -> []
    |(k,v)::t ->
      (repeat v k) @ uncompress t

(* UNCOMPRESS MAP FUNCTION *)
let rec uncompress_m : (int*int) list -> int list =
  fun l ->
   append (List.map (fun (k, v) -> repeat v k) l)

let rec uncompress_f =
  fun l ->
    failwith"apologies my guy"

(* OPTIMIZE FUNCTION *)
let rec opto : 'a list -> 'a -> 'a list =
  fun l curr ->
    match l with
    |[] -> []
    |h::t ->
      if (curr=1 && h=0) then h :: opto t 0
      else if (curr=1 && h=1) then opto t 1
      else if (curr=1) then h :: opto t 1
      else if (curr=0 && h=0) then opto t 0
      else if (curr=0 && h=1) then h :: opto t 1
      else h :: opto t 0


let rec optimize : 'a list -> 'a list =
  fun l ->
    opto l 1