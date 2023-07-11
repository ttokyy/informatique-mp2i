#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>


int fibo_mod_while(int n, int m){
	//hyp : n>= 0
	//retourne le reste modulo m du terme d'indice 0 de la suite de Fibonnacci
	if (n==0 || n==1)
		return n%m; // car F0=0 et F1=1
	int u = 1;
	int v = 1;
	int i = 1;
	//v=F_i et u=F_(i-1)
	while (i<n){
		int temp = v;
		v = (u + v)%m ;
		u = temp %m;
		i++;
	}
	return v;
}

int aux (int a, int b, int n, int m){
	// hyp : n>=0
	// calcule le reste modulo m du  terme u_n de la suite définie par
	// u_0=a, u_1=b et qq soit k\in N, u_(k+2)=u_(k+1)+u_k 
	if (n==0){
		return a%m;
	}
	/*
	else if (n==1){
		return b;
	}
	*/
	else{
		return aux (b%m,(a+b)%m,n-1,m);
	}
}

int fibo_mod_rec(int n, int m){
	//hyp : n>= 0
	//retourne reste modulo m du terme d'indice n de la suite de Fibonnacci 
	return aux(0,1,n,m);
}

int main(int argc, char* argv[]){
	int i;
	/*
	//affichage pour aperçu
	i = 0;
	while (i<15){
		printf("fibo_mod_w(%d,%d)=%d \t et fibo_r(%d,%d)=%d\n",i,2,fibo_mod_while(i,2),i,2,fibo_mod_rec(i,2));
		i++;
	}
	*/
	/*
	//jeu de test par comparaison
	i = 0;
	while (i<15){
		assert(fibo_mod_while(i,2)==fibo_mod_rec(i,2));
		i++;
	}
	*/
	
	if (argc != 4){
		printf("arg 1 : 'w' pour while ou 'r' pour récursif \n");
		printf("arg 2 : n \n");
		printf("arg 3 : m \n");
	}
	assert(argc == 4);
	
	int n = atoi(argv[2]);
	int m = atoi(argv[3]);
	
	if( strcmp(argv[1],"w")==0){
		printf("fibo_mod_w(%d,%d)=%d\n",n,m,fibo_mod_while(n,m));
	}
	else if( strcmp(argv[1],"r")==0){
		printf("fibo_mod_r(%d,%d)=%d\n",n,m,fibo_mod_rec(n,m));
	}
	else{
		printf("arg 1 : 'w' pour while ou 'r' pour récusrif \n");
	}

	return 0;
}
