let tri_selec (t_init : 'a array) : int array =
  (* hypothèse : Array.length t_init >0 *)
  (* calcule une permutation qui trie les valeurs de t_init*)
  
  let n = Array.length t_init in
  let t = Array.make n t_init.(0) in 
  let sigma = Array.make n 0 in
  for i = 0 to (n-1) do 
    t.(i) <- t_init.(i) ;
    sigma.(i) <- i 
  done;
  (* à ce stade t est une copie de t_init et sigma est l'identité*)
  
  (* invariant : sigma est une permutation, 
     et pour i de k à n t.(sigma.(i))=t_init.(i)
     et t_init.(sigma.(i)) pour i de 1 à k-1 est croissante 
	 et si k>1, pour tout i de k à n, t.(k-1) <= t.(i) *)
  for k=0 to n-1 do 
    (* trouvons i0 l'argmin de t[k..n]*)
    let i0 = ref k in
    for j = k+1 to n-1 do
      if t.(j) < t.(!i0) then i0:=j else()
    done;
    (* on veut retenir ds sigma.(k) l'indice ds t_init de la valeur t.(i0), 
	   retenir  ds sigma.(k) = sigma.(i0)
	   de plus on veut séparer cette valeur des valeurs restant à trier dans t  
	   donc on échange  sigma.(i0) et sigma.(k) et  t.(i0) et t.(k)*)
    let temp = t.(k) in t.(k) <- t.(!i0) ; t.(!i0) <- temp;
    let temp = sigma.(k) in sigma.(k) <- sigma.(!i0); sigma.(!i0) <- temp;
  done;
  sigma 
  
let test_tri_selec :unit =
  assert(tri_selec [|1;2;3|] = [|0;1;2|]);
  assert(tri_selec [|4;2;3|] = [|1;2;0|]);
  assert(tri_selec [|'a';'d';'c';'e';'f'|] = [|0;2;1;3;4|])