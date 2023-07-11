let compte (x:int) : string = 
	match x with 
	| 1 -> "un" 
	| 2 -> "deux" 
	| _ -> "bcp"
	
let compte_couple (c:int*int) : string = 
	match c with 
	| 1,_  -> "un peu" 
	| 2,_ -> "deux peu" 
	| 4,3 -> "quatre trois"
	| _,3  -> "trop"  
	| _ -> "bcp"
