let rec sigma : int*int*(int->int)->int = fun (a,b,f)-> 
	if a > b then 0
	else if a=b then f b
	else f(b) + sigma(a,b-1,f)

(*let _ = print_int(5+sigma(1,10,fun x->x));print_newline()*)
