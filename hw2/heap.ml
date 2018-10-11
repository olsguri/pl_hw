type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap
let rank h = match h with
		| EMPTY -> -1
		| NODE (r,_,_,_) -> r

let shake (x,lh,rh) = if (rank lh) >= (rank rh) then NODE(rank rh+1,x,lh,rh)
			else NODE(rank lh+1,x,rh,lh)


let rec merge: heap * heap -> heap = fun (heap1,heap2) ->
	match heap1 with
	| EMPTY -> heap2
	| NODE (rank1,value1,heapa,heapb) -> 
		(	match heap2 with
			| EMPTY -> heap1
			| NODE(rank2,value2,heapc,heapd) ->
				if value1>value2 then shake(value2,heapc,merge(heapd,heap1))
				else shake(value1,heapa,merge(heapb,heap2))
		)

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY))

let findMin h =match h with
	| EMPTY -> raise EmptyHeap
	| NODE(_,x,_,_) -> x

let deleteMin h = match h with
	| EMPTY -> raise EmptyHeap
	| NODE(_,x,lh,rh) -> merge(lh,rh)

(*let rec printheap heap = match heap with
	| EMPTY -> print_string("Empty heap");print_newline()
	| NODE (_,x,left,right) -> print_int(x);print_newline();print_string("left\n");printheap left;print_string("right\n");printheap right

let heap1 = NODE(1,4,NODE(1,19,NODE(0,27,NODE(0,43,EMPTY,EMPTY),EMPTY),NODE(0,20,EMPTY,EMPTY)),NODE(0,8,NODE(1,12,NODE(0,15,EMPTY,EMPTY),NODE(0,25,EMPTY,EMPTY)),EMPTY))

let heap2 = NODE(1,6,NODE(0,8,NODE(0,14,EMPTY,EMPTY),EMPTY),NODE(0,7,EMPTY,EMPTY))
let _ = printheap (merge(heap1,heap2))
*)
(*
let heap = EMPTY
let heap = insert(1,heap)
let heap = insert(5,heap)
let heap = insert(7,heap)
let heap = insert(10,heap)
let heap = insert(15,heap)
let heap1 = EMPTY
let heap1 = insert(22,heap1)
let heap1 = insert(75,heap1)

let _ = print_int(findMin heap);print_newline()
let heap = deleteMin heap
let _ = print_int(findMin heap1);print_newline()
let heap1 = deleteMin heap1

let heap2 = merge(heap,heap1)
let _ = print_int(findMin heap2);print_newline()

*)
