#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <stdlib.h>





bool est_elem(int x, int* tab, int lg){
	//hyp : tab est un tableau de lg entiers triés par ordre croissant
	//retourne si x est l'un des lg elements de tab
	int d = 0;
	int f = lg -1;
	int m = (d+f)/2;
	
	while(d <= f){
		if(tab[m] == x){
			return true;
		}
		else{
			if(tab[m] < x){
				d = m + 1;
			}
			else{
				f = m - 1;
			}
			m = (d+f)/2;
		}
	}
	return false;
}







int max (int a, int b){
	//retourne le maximum de {a,b}
	if (a>b)
		return a;
	else
		return b;
}


int nb_comp_1(int lg){
	if(lg == 0)
		return 0;
	else{
		int k = lg/2;
		if(lg%2 == 0){
			return max(nb_comp_1(k-1),nb_comp_1(k))+2;
		}
		else{
			return nb_comp_1(k)+2;
		}
	}
}








int main (){
	int tab1[10]={ 2,23,25,42,43,65,65,67,93,102};
	int tab2[11]={20,23,25,32,53,55,65,76,93,102,104};
	
	assert(est_elem(93,tab1,10));
	assert(est_elem( 2,tab1,10));
	assert(!est_elem(45,tab1,10));
	assert(!est_elem(35,tab2,11));
	assert(est_elem(20,tab2,11));
	assert(est_elem(93,tab2,11));
	
	int i=0;
	while(i<40){
		//printf("u_%d = %d = %d \n",i, nb_comp_1(i), nb_comp_2(i));
		printf("u_%d = %d \n",i, nb_comp_1(i));
		i = i + 1;
	}
	
	return 0;
}








int nb_comp_2(int lg){
	if(lg == 0)
		return 0;
	else{
		return nb_comp_2(lg/2)+2;
	}
}





/*
int main (int argc, char* argv[]){
	assert(argc==3);
	int n = atoi(argv[1]);
	int methode = atoi(argv[2]);
	
	if(methode==1){
		int i=0;
		while(i<n){
			nb_comp_1(i);
			i = i + 1;
		}
	}
	else{
		int i=0;
		while(i<n){
			nb_comp_2(i);
			i = i + 1;
		}
	}
	return 0;
}

*/
