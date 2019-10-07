(* Name: Michelle Tham *)
(* SBU ID: 111810145 *)

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

 (*
(* 1.4 *)			(* equivs (=) [1;1;3;4] -> [[1;1];[3];[4]] *)
let equivs func list =			(* (=) is the equivalence function, groups together *)
	equivs_helper func list [[]]
;;		(* Is it empty? Are they equal? If not then make a new list *)
								(* For each new element, have to iterate through past elements to check if equal *)
let rec equivs_helper func lst =
	match lst with (* Stores list in *)
	| 
	| equivs_helper_helper
let rec equivs_helper_helper
	|
*)


(* 1.5 *)
let slice lst i j =
	let rec get_sub n = function
		| [] -> []
		| curr :: rest ->
			if i > j then []		(* If the lowerbound is greater than upperbound *)
			else if n = 0 then []	(* Base case *)
			else curr :: get_sub (n-1) rest
	in
	let rec remove n = function
		| [] -> []
		| curr :: rest as listv ->
			if n = 0 then listv
			else remove (n-1) rest
	in
	get_sub (j-1) (remove i lst)
;;

(* 1.6 *)
let composition f g x =
	f (g x)	
;;

(* 1.7 *)
let rec equiv_on f g lst = 
	match lst with
	| [] -> true
	| curr :: rest ->
		if (f curr) = (g curr) then equiv_on f g rest
		else false
;;

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


