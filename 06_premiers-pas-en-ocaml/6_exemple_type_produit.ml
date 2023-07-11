type ma_struct = {a:int; b:int; c:float}

(*let truc1:ma_struct = {1; 2; 3.0}*)
let truc2:ma_struct = {a=1; b=2; c=3.0}
let truc3:ma_struct = {b=1; a=2; c=3.0}

let somme_entiers (s:ma_struct) : int =
	s.a + s.b
	
let qualificatif (s:ma_struct) : string =
	match s with 
	| {a=0; b=0; c=0.} -> "tout nul"
	| {a=0; b=0; _ } -> "entiers_nuls"
	| {c=0.; _ } -> "réel nul"
	| _ -> "pas nul"

let calcul (s:ma_struct) : float =
	match s with 
	| {c=0.; _ } -> 0.
	| {a=x; b=y; c=z} -> float_of_int (x+y) *. z


