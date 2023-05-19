(* 

   Stub for HW2 
   Please
   1. Rename to gt.ml
   2. Place your name here:

    Name: Rohan Kallur
    Pledge: I pledge my honor that I have abided by the Stevens Honor System.
*)



type 'a gt = Node of 'a*('a gt) list

let mk_leaf (n:'a) : 'a gt =
  Node(n,[])
    
let t : int gt =
 Node (33,
       [Node (12,[]);
        Node (77, 
              [Node (37,
                     [Node (14, [])]);
               Node (48, []);
               Node (103, [])])
       ])

let rec max: int list -> int =
  fun l ->
    match l with
    |[] -> 0
    |h::[] -> h
    |h::t ->
      let a = max t in
      if h > a then h
      else a

let rec height: 'a gt -> int=
  fun t->
  match t with
  |Node(_, []) -> 1
  |Node(_, l) -> 1 + max (List.map height l)
      
let rec size:'a gt -> int=
  fun t->
  match t with
  |Node(_, []) -> 1
  |Node(_, l) -> List.fold_left (fun x y -> x + size y) 1 l


let rec path: 'a gt -> int list -> int list list=
  fun t curr->
  match t with
  |Node(_, []) -> [List.rev curr]
  |Node(_, l) -> List.flatten (List.mapi(fun x y -> path y (x::curr)) l)
  
let rec paths_to_leaves:'a gt -> int list list =
  fun t->
  path t []

let rec check_list:'a list -> bool =
  fun l->
  match l with
  |[] -> true
  |h::[] -> true
  |h::t ->
    if h!=(List.hd t) then false
    else check_list t

let rec is_leaf_perfect:'a gt -> bool =
  fun t->
  check_list (List.map (List.length) (paths_to_leaves t))

let rec preorder:'a gt -> 'a list =
  fun (Node(d,ch))->
    if ch = [] then [d]
    else d :: List.flatten (List.map preorder ch)

let rec mirror: 'a gt -> 'a gt=
  fun (Node(d,ch))->
  Node(d , (List.rev (List.map mirror ch)))
  

let rec map:('a -> 'b) -> 'a gt -> 'b gt =
  fun f (Node(d,ch))->
  Node(f d, (List.map (map f) ch))
  
let rec fold : ('a -> 'b list -> 'b) -> 'a gt -> 'b =
  fun f (Node(d,ch)) ->
    f d (List.map (fold f) ch)

let sum t =
  fold (fun i rs -> i + List.fold_left (fun i j -> i+j) 0 rs) t

let mem t e = 
  fold (fun i rs -> i=e || List.exists (fun i -> i) rs) t

let mirror': 'a gt -> 'a gt= 
  fun t->
  fold (fun i rs -> Node(i, (List.rev rs))) t

let degree_helper (Node(d, ch)) =
  List.length ch

let degree: 'a gt -> int= 
  fun t->
  match t with
  |Node(_, []) -> 0
  |Node(_, ch) -> 0 + max (List.map (degree_helper) ch)