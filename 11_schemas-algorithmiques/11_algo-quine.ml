let est_sat_quine (f:formule) :	env option =
	(* calcule Some(e) où e est un environnement satisfaisant f *)
	(* s'il en existe, et None sinon *)
	let rec aux (ff:formule) (e:env) : env option =  
		(* hyp : ff est une formule simplifiée *)
		(* hyp : et les variables dans e n'aparraissent pas dans ff *)
		(* calcule un prolongement de e ac des affectations des var. *)
		(* de ff qui la satisfont si c'est possible et None sinon *)
	 	match ff with 
		| Vrai -> Some e
		| Faux -> None
		| _ -> 
			match liste_var ff with
			| [] -> failwith "une formule simplifiée sans variables doit être Vrai ou Faux"
			| var::_ ->	
				let ffs1 = simpl_complet(substitue ff var Vrai) in
				let e_res = aux (ffs1) ((var,true)::e)
				match e_res with
				| None -> let ffs2 =  simpl_complet(substitue ff var Faux) in
									aux (ffs2) ((var,false)::e)
				| _ -> e_res
	in aux (simpl_complet f) []
	

