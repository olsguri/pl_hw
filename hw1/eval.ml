type formula = TRUE
		| FALSE
		| NOT of formula
		| ANDALSO of formula * formula
		| ORELSE of formula * formula
		| IMPLY of formula * formula
		| LESS of expr * expr
	and expr = NUM of int
		| PLUS of expr * expr
		| MINUS of expr * expr
let rec convertexpr : expr -> int = fun expr ->
	match expr with
		| NUM int -> int
		| PLUS (expr1, expr2)->convertexpr(expr1)+convertexpr(expr2)
		| MINUS (expr1, expr2)->convertexpr(expr1)-convertexpr(expr2)

let rec eval: formula->bool = fun formula ->
	match formula with 
		| TRUE ->true
		| FALSE ->false
		| NOT formula -> not(eval(formula))
		| ANDALSO (formula1, formula2)-> eval(formula1) && eval(formula2)
		| ORELSE (formula1, formula2)-> eval(formula1) || eval(formula2)
		| IMPLY (formula1, formula2)->not(eval(formula1)) || eval(formula2)
		| LESS (expr1, expr2)-> convertexpr(expr1) < convertexpr(expr2)

(*let _ = if eval(LESS (PLUS(NUM 3,NUM 4),MINUS(NUM 7, NUM 8))) then print_endline("true")
	else print_endline("false")		*)
