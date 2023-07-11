type 'a pile =
	| PileVide
	| PileNonVide of ('a*('a pile))
	
let cree_pile_vide : 'a pile = PileVide

let est_pile_vide (p:'a pile) : bool =
 match p with
 | PileVide -> true
 | _ -> false

let sommet (p:'a pile) : 'a =
	(* hyp : not (est_pile_vide p) *)
	match p with
 | PileVide -> failwith "la pile vide n'a pas de sommet"
 | PileNonVide (elem,_) -> elem

let empiler  (elem:'a) (p:'a pile): 'a pile =
	PileNonVide (elem, p)
	
let p1:int pile = PileNonVide(1,PileVide)
let p2 = PileNonVide(2,p1)
let p3 = empiler 3 p2

(* ------------------------------------------------ *)

let rec somme_pile_int (p: int pile) : int = 
	match p with 
	| PileVide -> 0
	| Pile (elem, pp) -> elem + somme_pile_int pp

let somme_pile_int_rt (p: int pile) : int = 
	let rec aux (pi: int pile) (sum:int) : int = 
		match pi with 
		| PileVide -> sum
		| Pile (elem, pp) -> aux pp elem+sum 
	in aux p 0

let rec concatene_pile_str (p: string pile) : string = 
	match p with 
	| PileVide -> ""
	| Pile (elem, pp) -> elem ^ concatene_pile_str pp

let concatene_pile_str_rt (p: string pile) : string = 
	let rec aux (pi: string pile) (res:string) : string = 
		match pi with 
		| PileVide -> res
		| Pile (elem, pp) -> aux pp (res^elem) 
	in aux p ""	












