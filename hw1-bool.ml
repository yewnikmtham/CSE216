(* Name: Michelle Tham *)
(* SBU ID: 111810145 *)

(*** Section 2: Data Types ***)

(* 2.1 *)
type bool_expr =
	| Lit of string
	| Not of bool_expr
	| And of bool_expr * bool_expr
	| Or of bool_expr * bool_expr

let rec truth_helper a a_val b b_val = function
	(* Bases on which of boolean operation is being used *)
	| Lit x -> if x = a then a_val
				else if x = b then b_val
				else failwith "There is an invalid variable."
	| Not e -> not(truth_helper a a_val b b_val e)
	| And(e1, e2) -> (truth_helper a a_val b b_val e1) && (truth_helper a a_val b b_val e2)
	| Or(e1, e2) -> (truth_helper a a_val b b_val e1) || (truth_helper a a_val b b_val e2)

let truth_table a b exp = (* Truth Table and calls helper to evaluate boolean exp *)
	[(true, true, truth_helper a true b true);
		(true, false, truth_helper a true b false);
		(false, true, truth_helper a false b true);
		(false, false, truth_helper a false b false)]
;;