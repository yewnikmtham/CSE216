(* Name: Michelle Tham *)

(*** Section 1: Recursion and Higher-order Function ***)
(* 1.1 *)
let rec pow x n = 
	if n = 0 then 1		(* base case *)
	else x * pow x (n-1)
;;

let rec float_pow x n =
	if n = 0 then 1.	(* base  case *)
	else x *. float_pow x (n-1)
;;

(* 1.2 *)
(* Check prev == curr in list. if same, delete curr *)
let rec compress = function
	| [] -> []			(* Empty list - nothing to delete *)
	| curr :: [] -> curr :: []		(* Single element *)
	| curr :: next :: rest ->	
		if curr = next then compress (next :: rest)		(* If the adjacents are equal, then skip do not count current *)
		else curr :: compress (next :: rest)			(* If not equal, continue traversing through list *)
;;
 

(* 1.3 *)
let remove_if lst pred = 
	let rec predicate pred = function
		| [] -> []
		| curr :: rest -> 
			if (pred curr) then (predicate pred lst)
			else curr :: (predicate pred rest)
	in
	predicate pred lst
;;


(* 1.4 *)			(* equivs (=) [1;1;3;4] -> [[1;1];[3];[4]] *)
let rec add_to_list f element list =
  match list with
    | [] -> [[element]]
    | h :: t -> if f element (List.hd h) = true then (h@[element]) :: t else h :: add_to_list f element t;;

let equivs f lst = if lst = [] then [[]] else
  let rec equivs_help aux list = match list with 
    | [] -> aux
    | h :: t -> equivs_help (add_to_list f h aux) t
    in equivs_help [] lst;;

(* 1.5 *)
let rec slice list i j = 
  let rec answer =
    match list with 
      | [] -> []
      | h :: t -> let sliced = (if j = 1 then []
         else slice t (i-1) (j-1)) in
            if i > 0 then sliced 
              else h :: sliced in
    answer sliceList
;;


(* 1.6 *)
let composition f g x =
	f (g x)	
;;

(* 1.7 *)

let equiv_on x y list = 
  List.map x list = List.map y list;;


(* 1.8 *)
let rec pairwisefilter cmp lst =
	match lst with
	| [] -> []		(* Empty list *)
	| [x] -> [x]	(* Base case - one element *)
	| firstItem :: secondItem :: rest -> 
		cmp firstItem secondItem :: pairwisefilter cmp rest
;;

(* 1.9 *)
let polynomial lst x = 
	let rec paircheck acc = function
		| [] -> acc
		| curr :: rest -> let a, b = curr in
			paircheck (acc + (a * (pow x b))) rest
	in paircheck 0 lst
;;

(* 1.10 *)
let rec powerset list = 
  match list with 
    | [] -> [[]] 
    | h::t -> (powerset t) @ List.map(fun x -> h::x) (powerset t);;

