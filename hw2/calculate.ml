exception FreeVariable
type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp *exp

let rec calculate: exp -> float = fun exp ->
	let rec calhelper: exp*exp -> float = fun (exp,expnum) ->
	match exp with
	| X -> calculate expnum
	| INT int -> float int
	| REAL float -> float
	| ADD (exp1,exp2) -> calhelper (exp1,expnum) +. calhelper (exp2,expnum)
	| SUB (exp1,exp2) -> calhelper (exp1,expnum) -. calhelper (exp2,expnum)
	| MUL (exp1,exp2) -> calhelper (exp1,expnum) *. calhelper (exp2,expnum)
	| DIV (exp1,exp2) -> calhelper (exp1,expnum) /. calhelper (exp2,expnum)
	| SIGMA (exp1,exp2,exp3) -> calculate exp
	| INTEGRAL (exp1,exp2,exp3) -> calculate exp
	in
	match exp with
	| X ->raise(FreeVariable)
	| INT int->float int
	| REAL float -> float
	| ADD (exp1,exp2) ->calculate exp1 +. calculate exp2
	| SUB (exp1,exp2) ->calculate exp1 -. calculate exp2
	| MUL (exp1,exp2) ->calculate exp1 *. calculate exp2
	| DIV (exp1,exp2) ->calculate exp1 /. calculate exp2
	| SIGMA (exp1, exp2, exp3) ->
		if (int_of_float( calculate(exp2))<int_of_float(calculate(exp1))) then 0.0
		else 
			calhelper(exp3,INT(int_of_float(calculate(exp2))))+.calculate(SIGMA(exp1,SUB(exp2, INT 1), exp3))
	| INTEGRAL (exp1, exp2, exp3) -> 
		if (calculate exp1 > calculate exp2) then -1.0*.calculate(INTEGRAL(exp2, exp1, exp3))
		else
			if (calculate exp2 -. calculate exp1 <0.1) then 0.0
			else
				calhelper(exp3, exp1)*.0.1+.calculate(INTEGRAL(ADD(exp1, REAL 0.1),exp2,exp3))
	

(*let _ = print_float(calculate(SIGMA(SIGMA(INT 1, INT 1, X), SIGMA(INT 10, INT 10, X),SUB(MUL(X,X),INT 1))));print_newline()*)
