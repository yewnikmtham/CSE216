(* Name: Michelle Tham *)

(* 2.2 *)

type stack = 
	Node of int list
;;

let start func =
	func([])
;;	

let push n lst func =
	match lst with
	| lst -> func(n :: lst)
;;

let pop lst func = 
	match lst with 
	| [] -> func([]) (* Empty stack *)
	| h :: t -> func(t)
;;

let add lst func = 
	match lst with
	| [] -> func([])
	| [x] -> func(lst)
	| x :: (y :: z) -> func((x + y) :: z)
;;

let mult lst func =
	match lst with
	| [] -> func([])
	| x :: [] -> func(lst)
	| x :: (y :: z) -> func((x * y) :: z)
;;

let clone lst func = 
	match lst with
	| [] -> []
	| h :: t -> func(h :: lst)
;;

let rec kpop_helper x lst func =
	match lst with
	| [] -> func([])
	| a :: t -> if x<0 then t
			else kpop_helper (x - 1) t func
;;

let kpop x lst func =
	match lst with
	| [] -> []
	| h :: t -> func(kpop_helper (h) t func)
;;

let halt lst =
	match lst with
	lst -> lst
;;

