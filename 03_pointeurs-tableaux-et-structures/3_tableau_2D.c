#include <stdio.h>



int main(int argc, char* argv[]){
	
	int tab[5][3]={{10,20,30},{11,21,31},{12,22,32},{13,23,33},{14,24,34},};
	int i = 0;
	
	/*
	//remplir ce tableau
	while(i <5){
		tab[i][0]=10+i;
		tab[i][1]=20+i;
		tab[i][2]=30+i;
		i++;
	}
	*/
	
	printf("tab est stocké à l'adresse tab=%lu \n",tab);
	printf("tab est stocké à l'adresse &tab[0]=%lu \n",&tab[0]);
	printf("tab est stocké à l'adresse &tab[0][0]=%lu \n",&tab[0][0]);
	printf("----------\n");
	//afficher ce tableau de pointeurs
	i=0;
	while(i <5){
		printf("&tab[%d]=%lu  et *(tab+%d)=%lu   et   tab[%d]=%lu\n",i, &(tab[i]),i,*(tab+i),i,tab[i]);
		i++;
	}
	printf("----------\n");
	
	//afficher ce tableau vu comme linéaire
	i=0;
	while(i < 15){
		printf("%d \t",*(*tab+i));
		i++;
	}
	printf("\n----------\n");
	
	//afficher ce tableau 2d vu comme un tableau de lignes (5 lignes de taille 2)
	i=0;
	while(i <5){
		printf("%d \t",*(*(tab+i)));
		printf("%d \t",*(*(tab+i)+1));
		printf("%d \t",*(*(tab+i)+2));
		printf("\n");
		i++;
	}
	printf("----------\n");
	
	//afficher ce tableau 2d vu comme un tableau de colonnes (5 colonnes de taille 2)
	i=0;
	while(i <5){
		printf("%d \t",**(tab+i));
		i++;
	}
	printf("\n");
	i=0;
	while(i <5){
		printf("%d \t",*(*(tab+i)+1));
		i++;
	}
	printf("\n");
	i=0;
	while(i <5){
		printf("%d \t",*(*(tab+i)+2));
		i++;
	}
	printf("\n----------\n");
	return 0;
}
