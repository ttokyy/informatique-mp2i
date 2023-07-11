let a:int = 3
let b:int = 5
let f (x:int) (y:int) : int = a*x+b*y

let rec facto (n:int) : int =
	if n =0 then 1
	else (facto (n-1)) * n
	
let facto_bis (n:int) : int =
	let rec aux (nn:int) (res:int) : int =
		if nn = 0 then res
		else aux (nn-1) (res*nn)
	in aux n 1
