let rec prodseq:(int*int->float)*int*int->float = fun (m,n,k)->
	if k<1 then 1.0
	else if k = 1 then m(n,k)
	else m(n,k)*.prodseq(m,n,k-1)
let func (x,y) = float_of_int (x+y)

let rec sumprod: (int*int->float)*int*int->float = fun (m,n,k) ->
	if n<1 then 0.0
	else if n = 1 then prodseq(m,n,k)
	else prodseq(m,n,k)+.sumprod(m,n-1,k)

(*let matrix (i,j) = ((float_of_int i *.10.) +. (float_of_int j ))
let _ = print_float(sumprod(matrix,4,0) );print_newline()*)
