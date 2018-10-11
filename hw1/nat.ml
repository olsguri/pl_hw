type nat = ZERO | SUCC of nat

let rec printnat : nat->string = fun nat ->
	match nat with
		| ZERO -> "ZERO"
		| SUCC nat1 ->
			(match nat1 with
			| ZERO -> "SUCC ZERO"
			| SUCC nat2-> "SUCC("^printnat(nat1)^")"
			)
(*let _ = print_string(printnat(SUCC(SUCC(SUCC ZERO))));print_newline()*)
let rec natadd : nat * nat -> nat = fun (nat1, nat2)->
	match nat1 with
		| ZERO ->nat2
		| SUCC nat1->SUCC (natadd(nat1,nat2))

let rec natmul : nat * nat -> nat = fun (nat1, nat2)->
	match nat1 with
		| ZERO -> ZERO
		| SUCC nat ->
			(match nat with
			| SUCC nat1 -> natadd(nat2,natmul(nat,nat2))
			| ZERO -> nat2
			)
(*let _ = print_string(printnat(natadd(ZERO,SUCC(SUCC ZERO))));print_newline()
let _ = print_string(printnat(natmul(SUCC ZERO, ZERO)));print_newline()*)
