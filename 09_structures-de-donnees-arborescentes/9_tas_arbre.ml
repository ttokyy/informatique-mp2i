(*-----------------------------------*)
(* Définir les types                 *)
(*-----------------------------------*)
type 'a ab =
	| V
	| Nd of ('a * 'a ab * 'a ab) 
(* le arbres binaires comme d'habitude *)

type chemin = bool list
(* chemin dans un arbre binaire, *)
(* qui peut mener à vide plutôt qu'à un noeud a priori*)

type 'a elemp = 'a * int
(* un element muni de sa priorité dans le tas  *)

type 'a tas = ('a elemp) ab * chemin
(* on maintient l'arbre te le chemin du premier vide *)




(*-----------------------------------*)
(* Outils                            *)
(*-----------------------------------*)

(*suivant en récusrif terminal*)
let ch_suivant (chem:chemin) : chemin =
	let rec ajoute_false (n:int)(ch:chemin) : chemin =
		(* hyp : n>=0 *)
		(* ajoute n false en tête de ch*)
		if n = 0 then ch else ajoute_false (n-1)(false::ch)
	in 
	let rec aux (ch:chemin) (n:int) : chemin =
		(* calcule le chemin suivant List.rev ch @ n true*)
		match ch with
		|[] -> ajoute_false (n+1) []
		|true::sch-> aux sch (n+1)
		|false::sch-> List.rev(ajoute_false (n) (true::sch))
	in aux (List.rev chem) 0

let test_ch_suivant : unit =
	assert(ch_suivant []=[false]);
	assert(ch_suivant [true;true;true]=[false;false;false;false]);
	assert(ch_suivant [true;false;true;true]=[true;true;false;false])
	
(*précédent en récusrif NON terminal*)
let ch_precedent (chem : chemin) : chemin =
	let rec aux (ch:chemin) : chemin = 
  match ch with
  | [] -> failwith "[] n'a pas de chemin préc"
  | [false] -> []
  | true :: sch -> false :: sch
  | false :: sch -> true :: (aux sch)
  in List.rev (aux (List.rev chem))

let test_ch_precedent : unit =
	assert(ch_precedent [false]=[]);
	assert(ch_precedent [false;false;false;false]=[true;true;true]);
	assert(ch_precedent [true;true;false;false]=[true;false;true;true])
	
let (#<=) (u:'a elemp)(v:'a elemp):bool =
(* teste si u est de moindre priorité que v*)
	let (_,pu) = u in
	let (_,pv) = v in 
	(pu <= pv)

let (#<) (u:'a elemp)(v:'a elemp):bool =
(* teste si u est de strictement moindre priorité que v *)
	let (_,pu) = u in
	let (_,pv) = v in 
	pu < pv
	

let est_tas_valide (t:'a tas) : bool =
	(* teste si dans l'arbre fst t chaque noeud à part la racine *)
	(* est de priorité 	supérieure à celle de son père *)
	(*ATTENTION on ne teste pas ici la validité du chemin...*)
	let rec aux (aa:('a elemp) ab) : bool =
		match aa with 
		| V -> true
		| Nd(x,V,V) -> true
		| Nd(x,V,Nd(xd,gd,dd)) -> (x #<= xd) && aux (Nd(xd,gd,dd))
		| Nd(x,Nd(xg,gg,dg),V) -> (x #<= xg) && aux (Nd(xg,gg,dg))
		| Nd(x,Nd(xg,gg,dg),Nd(xd,gd,dd)) -> 
				(x #<= xg) && (x #<= xd) 
				&& aux (Nd(xg,gg,dg)) && aux (Nd(xd,gd,dd))
	in let (a,ch)= t in aux a
		

(*-----------------------------------*)
(* Opération creer_tas_vide          *)
(*-----------------------------------*)
let tas_vide = (V,[])

(*-----------------------------------*)
(* Opération est_tas_vide            *)
(*-----------------------------------*)
let est_tas_vide (t:'a tas) : bool =
	let (a,ch) = t in
	match a with
	| V -> true 
	| Nd(x,g,d) -> false
	

(*-----------------------------------*)
(* Opération minimum                 *)
(*-----------------------------------*)

let elemp_min (t:'a tas) : 'a elemp =
	let (a,ch) = t in
	match a with
	| V -> failwith "le tas vide n'a pas d'élément minimum" 
	| Nd(x,g,d) -> x
	
let min (t:'a tas) : 'a =
	let (e,p) = (elemp_min t) in e 
	
let prio_min (t:'a tas) : 'a =
	let (e,p) = (elemp_min t) in p


(*-----------------------------------*)
(* Opération insere                  *)
(*-----------------------------------*)

let echange_racine (a:'a ab) (b: bool)  : 'a ab =
	(*hyp : si b le fils gauche de a est non vide, sinon le fils droit de a est non vide*)
	(* échange la racine de a ave son fils gauche si b, *)
	(* ou avec son fils droit sinon *)
  match a, b with
  | Nd(x, Nd(xg, gg, dg), d), true  -> Nd(xg, Nd(x,gg,dg), d)
  | Nd(x, g, Nd(xd, gd, dd)), false -> Nd(xd, g, Nd(x, gd, dd))
  | _ -> failwith "échange avec un fils vide impossible"

let rec ajoute_feuille (a:'a elemp ab)(ch:bool list) (ep:'a elemp) =
	(* ajoute ep en feuille dans a à la place d'un vide de chemin ch*)
	match a,ch with
	| V,[] -> Nd(ep,V,V)
	| V,_ -> failwith "le chemin est trop long"
	| _,[] -> failwith "le chemin est trop court"
	| Nd(x,g,d),false::sch -> Nd(x,(ajoute_feuille g sch ep),d)
	| Nd(x,g,d),true::sch -> Nd(x,g,(ajoute_feuille d sch ep))

let insere (t:'a tas)(elemp:'a elemp) : 'a tas =
	let rec aux (a:'a elemp ab)(ch:bool list) (ep:'a elemp) : 'a elemp ab =
		(* ajoute ep dans a sur le chemin ch à la place qu'impose sa prio, *)
		(* et repousse vers le bas les nds le long de ch de prio > à ep *)
		match a,ch with
		| V,[] -> Nd(ep,V,V)
		| V,_ -> failwith "le chemin est trop long"
		| _,[] -> failwith "le chemin est trop court"
		| Nd(x,g,d),false::sch -> 
			if ep #< x 
			then Nd(ep,(aux g sch x),d)
			else Nd(x,(aux g sch ep),d)
		| Nd(x,g,d),true::sch -> 
			if ep #< x 
			then Nd(ep,g,(aux d sch x))
			else Nd(x,g,(aux d sch ep))
	in let (arbre,chem) = t 
	in ((aux arbre chem elemp), (ch_suivant chem))

let test_insere : unit =
	let t0 =tas_vide in
	let t1 = insere t0 (1,1) in
	let t2 = insere t1 (2,2) in
	let t3 = insere t2 (0,0) in
	let t4 = insere t3 (10,10) in
	assert (est_tas_valide t1 && (min t1 = 1));
	assert (est_tas_valide t2 && (min t2 = 1));
	assert (est_tas_valide t3 && (min t3 = 0));
	assert (est_tas_valide t4 && (min t4 = 0))



(*-----------------------------------*)
(* Opération supprime_min            *)
(*-----------------------------------*)
let pousse_racine (t:'a tas) : 'a tas =
	(*hyp : t  est parfait et il est vide 
	ou constitué de 2 sous-arbres tournois
	calcule l'arbre obtenu en échangeant la racine
	avec son fils droit ou son fils gauche
	de manière à obtenir un arbre tournoi *)
	let rec aux (aa: ('a elemp) ab) : ('a elemp) ab =
		match aa with
		| Nd(x,V,Nd(xd,gd,dd)) -> 
			failwith "ceci n'est pas parfait"
			
		| Nd(x,Nd(xg,gg,dg),V) -> 
			if x #< xg then aa
			else Nd(xg,(aux (Nd(x,gg,dg)) ),V)
		
		| Nd(x,Nd(xg,gg,dg),Nd(xd,gd,dd)) ->
			if xg #<= xd
			then if xg #< x
				then Nd(xg,(aux (Nd(x,gg,dg))), Nd(xd,gd,dd))
				else aa
			else (*ie xd #< xg*)
				if xd #< x
				then Nd(xd, Nd(xg,gg,dg), (aux (Nd(x,gd,dd))))
				else aa
				
		| _ -> aa
	in let (a,ch) = t 
	in ((aux a),ch) 
	
let test_pousse : unit =
	let a1 = Nd((5,5),
				Nd((2,2),
					Nd((4,4),Nd((12,12),V,V),Nd((4,4),V,V)),
					Nd((3,3),Nd((8,8),V,V),Nd((8,8),V,V))),
				Nd((7,7),
					Nd((8,8),V,V),
					Nd((9,9),V,V))) in
	let t1= (a1,[true;false;false]) in
	assert (est_tas_valide (pousse_racine t1))



(* à completer 
let supprime_min (t:'a tas): 'a tas =
	(*hyp :  est_valide t && not(est_tas_vide t) *)
	(* calcule le tas obtenu en supprimant le min de t*)

*)
	

