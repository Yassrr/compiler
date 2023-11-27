#include <stdlib.h>
#include <stdio.h>
#include <string.h>
typedef struct element_qdr{
    char *opt;
    char *opd1;
    char *opd2;
    char *res;
}quad;

typedef struct element_pile{
	char *donnee;
	struct element_pile *prc;
}pile;

char tempp[5];
quad EnsembleQuad[1000];
pile *pile1,*pile2,*pile3;
int qc;
void creerQuad(char *opt, char *opd1, char *opd2, char *res){
	EnsembleQuad[qc].opt=strdup(opt);
	EnsembleQuad[qc].opd1=strdup(opd1);
	EnsembleQuad[qc].opd2=strdup(opd2);
	EnsembleQuad[qc].res=strdup(res);
	qc++;
}


char* ConvertirEnChaine(int i){
	char s[15];
	sprintf(s,"%d",i);
	return strdup(s);
}

//quadruplet pour les expressions arithmitique
void creerQuadA(int type, char *cond1, char *cond2, char *res){
	char *TypeBR;
	switch(type){
		case 1 :{
			TypeBR=strdup("BNE");
		}
		break;
		case 2 :{
			TypeBR=strdup("BE");
		}
		break;
		case 3 :{
			TypeBR=strdup("BL");
		}
		break;
		case 4 :{
			TypeBR=strdup("BG");
		}
		break;
		case 5 :{
			TypeBR=strdup("BGE");
		}
		break;
		case 6 :{
			TypeBR=strdup("BLE");
		}
		break;
	}
	creerQuad(TypeBR,ConvertirEnChaine(qc+3),cond1,cond2);
	creerQuad("AFF","1","",res);
	creerQuad("BR",ConvertirEnChaine(qc+2),"","");
	creerQuad("AFF","0","",res);
}

//quadruplet pour les expressions logique
void creerQuadL(int type, char *cond1, char *cond2, char *res){
	switch(type){
		case 1 :{//not
			creerQuad("BNZ",ConvertirEnChaine(qc+3),cond1,"");
			creerQuad("AFF","1","",res);
			creerQuad("BR",ConvertirEnChaine(qc+2),"","");
			creerQuad("AFF","0","",res);
		}
		break;
		case 2 :{//or
			creerQuad("BNZ",ConvertirEnChaine(qc+4),cond1,"");
			creerQuad("BNZ",ConvertirEnChaine(qc+3),cond2,"");
			creerQuad("AFF","0","",res);
			creerQuad("BR",ConvertirEnChaine(qc+2),"","");
			creerQuad("AFF","1","",res);
		}
		break;
		case 3 :{//and
		   
			creerQuad("BZ",ConvertirEnChaine(qc+4),cond1,"");
			creerQuad("BZ",ConvertirEnChaine(qc+3),cond2,"");
			creerQuad("AFF","1","",res);
			creerQuad("BR",ConvertirEnChaine(qc+2),"","");
			creerQuad("AFF","0","",res);
		}
		break;
	}
}
//empiler une chaine
void empiler_Str(pile **p,char *donne){
	pile *new;

	new=(pile*)malloc(sizeof(pile));
	new->donnee=strdup(donne);

	new->prc=*p;
	*p=new;
}

//empiler un entier
void empiler_Int(pile **p,int donne){
	pile *new;
	char str[10];
	new=(pile*)malloc(sizeof(pile));
	sprintf(str,"%d",donne);
	new->donnee=strdup(str);
	
	new->prc=*p;
	*p=new;
}

char* RenvoyerSommetPile(pile*p){
	return p->donnee;
}

char* depiler(pile**p){
	char *res;
	pile *H;
	H=*p;
	res=strdup(H->donnee);
	*p=H->prc;
	free(H);
	return res;
}


void AfficherQuad(){
	int i;
	printf("____________________________________________________________________________________________\n");
	printf("\n||||||||||||||||||||||||||||||||||| EnsembleQuaduplets |||||||||||||||||||||||||||||||||||\n");
	printf("____________________________________________________________________________________________\n");
	for(i=0;i<qc;i++){
		printf("\n|\t %d - ( %s  ,  %s  ,  %s  ,  %s )",i,EnsembleQuad[i].opt,EnsembleQuad[i].opd1,EnsembleQuad[i].opd2,EnsembleQuad[i].res);
	}
	printf("\n____________________________________________________________________________________________\n");
}
