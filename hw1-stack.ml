(* Name: Michelle Tham *)
(* SBU ID: 111810145 *)

(* 2.2 *)

type stack = 
	Node of int list
;;

let start func =
	func, []
;;	

let push n lst func =
	match lst with
	| [] -> func [n]
	| h :: t -> func (n :: h :: t)
;;

let pop lst func = 
	match lst with 
	| [] -> func [] (* Empty stack *)
	| h :: t -> func t
;;

let add lst func = 
	match lst with
	| [] -> func []
	| x :: y :: rest -> func ((x + y) :: rest)
	| _ -> func lst
;;

let mult lst func =
	match lst with
	| [] -> func []
	| x :: y :: rest -> func ((x * y ) :: rest)
	| _ -> func lst
;;

let clone lst func = 
	match lst with
	| [] -> func []
	| h :: t -> (h :: h :: t)
;;

