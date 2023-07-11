#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <stdlib.h>

bool aux(int x, int* tab, int d, int f){
	//hyp : [d..f] est un intervalle valide d'indices pour tab
	//      et tab[d..f] est trié par ordre croissant
	//teste si x est un élément du tableau tab[d..f]
	if(d>f){
		return false;
	}
	else{
		int m = (d+f)/2;
		if (x== tab[m])
			return true;
		else if(x < tab[m])
			return aux(x,tab,d,m-1);
		else 
			return aux(x,tab,m+1,f);
	}
}

bool est_elem_rec(int x, int* tab, int lg){
	//hyp : tab est un tableau de lg entiers triés par ordre croissant
	return  aux(x,tab,0,lg-1);
}


int main (){
	int tab1[10]={ 2,23,25,42,43,65,65,67,93,102};
	int tab2[11]={20,23,25,32,53,55,65,76,93,102,104};
	
	assert(est_elem_rec(93,tab1,10));
	assert(est_elem_rec( 2,tab1,10));
	assert(!est_elem_rec(45,tab1,10));
	assert(!est_elem_rec(35,tab2,11));
	assert(est_elem_rec(20,tab2,11));
	assert(est_elem_rec(93,tab2,11));

	return 0;
}



