(* Name: Michelle Tham *)
(* SBU ID: 111810145 *)

(*** Section 2: Data Types ***)

(* 2.1 *)
type bool_expr =
	| Lit of string
	| Not of bool_expr
	| And of bool_expr * bool_expr
	| Or of bool_expr * bool_expr
;;

let rec truth_helper a b boola boolb exp =
	match exp with
	(* Bases on which of boolean operation is being used *)
	Lit checks -> (match check with
		| x -> boola
		| y -> boolb
		| _ -> failwith "Invalid variable.")
	| Not exp -> not(truth_helper a b boola boolb exp)
	| And(e1, e2) -> (truth_helper a b boola boolb e1) && (truth_helper a b boola boolb e2)
	| Or(e1, e2) -> (truth_helper a b boola boolb e1) || (truth_helper a b boola boolb e2)
;;

let truth_table a b exp = (* Truth Table and calls helper to evaluate boolean exp *)
	[(true, true, truth_helper a b true true exp);
		(true, false, truth_helper a b true false exp);
		(false, true, truth_helper a b false true exp);
		(false, false, truth_helper a b false false false false)]
;;
