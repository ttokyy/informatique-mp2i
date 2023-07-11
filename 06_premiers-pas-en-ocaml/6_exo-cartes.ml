type couleur = Pique | Trefle | Coeur | Carreau

let est_rouge (clr:couleur) : bool =
	(clr=Coeur) || (clr=Carreau)

let est_noir (clr:couleur) : bool =
	match clr with
	| Pique -> true
	| Trefle -> true
	

type valeur = 
	| Brele  of int 
	| Tete of int*string

let point (vlr: valeur) :int=
	match vlr with
	| Brele a -> a
	| Tete (a, b) -> a

 


type carte = couleur*valeur

let point_carte (c:carte) :int =
	let (_,vlr)=c in
	point vlr
	
(*-----------------------------------*)

let nom_val (v:valeur) : string =
	match v with
	| Brele(x) -> string_of_int x
	| Tete (_,str) -> str

let nom_couleur (clr:couleur) : string =
	match clr with 
	| Carreau -> "carreau"
	| Coeur -> "coeur"
	| Pique -> "pique"
	| Trefle -> "trèfle"
	
let nom_carte (c:carte) : string =
	let (clr,vlr)=c in 
	nom_valeur vlr ^ " de "^ nom_couleur clr
