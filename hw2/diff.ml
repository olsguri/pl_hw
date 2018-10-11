exception InvalidArgument
type ae = CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list
let rec cutlisthelp: ae list * int * int-> ae list = fun (list, n, count) ->
	if count = n-1 then (List.nth list count)::[]
	else (List.nth list count)::cutlisthelp(list, n, count+1)

let rec cutlist: ae list * int -> ae list * ae list = fun (list, n) ->
	match list with
	| [] -> raise(Failure "empty list")
	| _ -> 
		if n >= List.length list || n <= 0 then raise(Failure "out of range")
		else (cutlisthelp(list, n, 0),List.rev(cutlisthelp(List.rev list, (List.length list)-n,0)))

let rec diff: ae * string -> ae = fun (ae,str) ->
	match ae with
	| CONST int -> CONST 0
	| VAR string ->
		if str = string then CONST 1
		else CONST 0
	| POWER (string,int)->
		if string = str then 
			if int != 0 then TIMES(CONST(int)::POWER(string,int -1)::[])
			else CONST 0
		else  CONST 0
	| TIMES list->
		(
			let rec difftimes: ae list * string * int * int -> ae list = fun (ilist, istr, length, count) ->
				if count = 0 then TIMES(diff(List.hd ilist, istr)::List.tl ilist)::difftimes(ilist, istr, length, count+1)
				else if count = length-1 then TIMES(List.rev (List.tl (List.rev ilist))@(diff(List.hd (List.rev ilist), istr)::[]))::[]
				else TIMES(fst(cutlist(ilist,count))@(diff(List.hd(snd(cutlist(ilist,count))),istr)::List.tl(snd(cutlist(ilist,count)))))::difftimes(ilist,istr,length,count+1)
				in
			match list with
			| [] -> raise(InvalidArgument)
			| aeh::[] -> diff(aeh,str)
			| CONST int::VAR string::[] ->
				if string =str then CONST int
				else CONST 0
			| VAR string :: CONST int::[] ->
				if string =str then CONST int
				else CONST 0
			| CONST int1::CONST int2::[] -> CONST 0
			| CONST(int1)::ae::[]->
				if int1=1 then diff(ae,str)
				else if int1=0 then CONST 0
				else TIMES(CONST int1::diff(ae,str)::[])
			| ae::CONST(int2)::[]->
				if int2=1 then diff(ae,str)
				else if int2=0 then CONST 0
				else TIMES(CONST int2::diff(ae,str)::[])
			| VAR str1::VAR str2::[] ->
				if str1=str && str2=str then TIMES[CONST 2;VAR str2]
				else if str1=str then VAR str2
				else if str2=str then VAR str1
				else CONST 0
			| VAR(string)::ae::[] ->
				if string=str then SUM(ae::TIMES(VAR string::diff(ae,str)::[])::[])
				else TIMES(VAR string::diff(ae,str)::[])
			| ae::VAR(string)::[] ->
				if string=str then SUM(ae::TIMES(VAR string::diff(ae,str)::[])::[])
				else TIMES(VAR string::diff(ae,str)::[])
			| ae1::ae2::[] -> SUM(TIMES(diff(ae1,str)::ae2::[])::TIMES(ae1::diff(ae2,str)::[])::[])	
			
			| _ -> SUM(difftimes(list, str, List.length list, 0))
		)
	| SUM list ->
		(
			let rec diffsum: ae list * string -> ae list =  fun (ilist, istr) ->
				match ilist with
				| [] -> []
				| ae::[] -> diff(ae,istr)::[]
				| ae::l -> diff(ae,istr)::diffsum(l,istr)
				in
			match list with
			| [] -> raise(InvalidArgument)
			| aeh::[] -> diff(aeh,str)
			| VAR str1::VAR str2::[] ->
				if str1=str&&str2=str then CONST 2
				else if str1=str then CONST 1
				else if str2=str then CONST 1
				else CONST 0
			| CONST(int1)::ae::[] -> diff(ae,str)
			| ae::CONST(int2)::[] -> diff(ae,str)
			| ae1::ae2::[] -> SUM(diff(ae1,str)::diff(ae2,str)::[])
			| _ -> SUM(diffsum(list,str))
		)

(*let rec printae:ae->string=fun ae ->
	let rec printaelist: ae list->string = fun list ->
		match list with
		| [] -> "[]"
		| ae::[] -> printae ae 
		| ae::list1 -> printae(ae)^"; "^printaelist(list1)
	in

	match ae with
	| CONST int -> "CONST "^string_of_int(int)
	| VAR string -> "VAR "^string
	| POWER (string,int) -> "POWER ("^string^", "^string_of_int(int)^")"
	| TIMES list ->
	(	match list with
		| [] -> "TIMES ([])"
		| _ -> "TIMES (["^printaelist(list)^"])"
	)
	| SUM list ->
 	(	match list with
		| [] -> "SUM ([])"
		| _ -> "SUM (["^printaelist(list)^"])"
	)

let ex = [VAR "a"; VAR "b"; VAR "c"; VAR "d"]
let _ = print_string(printae(TIMES(ex)));print_newline()
let _ = print_string(printae(TIMES(fst (cutlist(ex,1)))));print_newline()

let _ = print_string(printae(TIMES(snd (cutlist(ex,1)))));print_newline()
let _ = print_string(printae(diff(SUM [TIMES [CONST 5; TIMES [VAR "y"; TIMES[VAR "x"; VAR "x"]]]; TIMES[SUM[VAR "x";CONST 2];VAR "x"]],"x")));print_newline();print_newline()

let _ = print_string(printae(diff(TIMES[VAR "x"; VAR "x"; VAR "x"], "x")));print_newline()
let _ = print_string(printae(diff(SUM[VAR "x"; VAR "x"; TIMES[VAR "x";VAR "x";CONST 2];VAR "x"],"x")));print_newline()
let _ = print_string(printae(diff(SUM[SUM[VAR "x";CONST 5];SUM[VAR "x";CONST 2]], "x")));print_newline()*)
