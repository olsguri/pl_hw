module type Queue = 
	sig
		type element
		type queue
		exception EMPTY_Q
		val emptyQ: queue
		val enQ: queue * element -> queue
		val deQ: queue -> element * queue
	end

module IntListQ = 
	struct
		type element = int list
		type queue = int list list * int list list
		exception EMPTY_Q
		let emptyQ:queue = ([],[])
		let enQ: queue * element -> queue =  fun (iqueue,elem) ->
			match iqueue with
			| ([],[]) -> (elem::[],[])
			| (lista,listb)->(elem::lista,listb )
		let deQ: queue-> element * queue = fun iqueue ->
			match iqueue with
			| ([],[]) -> raise(EMPTY_Q)
			| (lista, listb) ->
				(
					match listb with
					| [] -> (List.hd (List.rev lista),([],List.tl (List.rev (lista))))
					| _ ->(List.hd listb, (lista, List.tl listb))
				)
			 
	end

(*module ValidIntListQ = (IntListQ : Queue)
let rec printintlist: int list -> string = fun il ->
	match il with
	| [] -> "[]"
	| int::[] -> string_of_int(int)^"]"
	| int::l -> string_of_int(int)^"; "^printintlist(l)

let rec printintlistlist: int list list -> string = fun ill ->
	match ill with
	| [] -> "[]"
	| l::[] -> printintlist(l)^"]"
	| l::ll -> printintlist(l)^"; "^printintlistlist(ll)

let rec printll:int list list -> string = fun ill ->
	match ill with
	| [] -> "[]"
	| _ -> "["^printintlistlist(ill)

let rec printqueue: IntListQ.queue -> string = fun queue ->
	match queue with
	| (lista,listb) -> "("^printll(lista)^", ["^printll(listb)^")"

let _ = print_string(printqueue(snd(IntListQ.deQ (IntListQ.emptyQ))));print_newline()*)
