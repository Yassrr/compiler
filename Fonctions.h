
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
typedef struct
{
   int state;
   char name[1000];
   char code[20];
   char type[20];
   float val;
   int taille;
   bool table;
   int nbr_changement_val;
 } element;

typedef struct
{ 
   int state; 
   char name[20];
   char type[20];
} elt;

element tab[1000];
elt tabs[100],tabm[100];
extern char sav[20];
char cc[20];


void initialisation()
{
  int i;
  for (i=0;i<1000;i++)
  tab[i].state=0;
  
  

  for (i=0;i<100;i++)
    {tabs[i].state=0;
    tabm[i].state=0;}

}




void inserer (char entite[], char code[],char type[],float val,int i, int y)
{
  switch (y)
 { 
   case 0:/*insertion dans la table des IDF et CONST*/
     tab[i].state=1;
     strcpy(tab[i].name,entite);
     strcpy(tab[i].code,code);
	   strcpy(tab[i].type,type);
	   tab[i].val=val;
     tab[i].taille=0;
     tab[i].table=false;
     tab[i].nbr_changement_val=0;
	   break;

   case 1:/*insertion dans la table des mots clés*/
       tabm[i].state=1;
       strcpy(tabm[i].name,entite);
       strcpy(tabm[i].type,code);
       break; 
    
   case 2:/*insertion dans la table des séparateurs*/
      tabs[i].state=1;
      strcpy(tabs[i].name,entite);
      strcpy(tabs[i].type,code);
      break;
 }

}


void rechercher (char entite[], char code[],char type[],float val,int y)	
{

int j,i;

switch(y) 
  {
   case 0:
        for (i=0;((i<1000)&&(tab[i].state==1))&&(strcmp(entite,tab[i].name)!=0);i++); 
        if((i<1000)&&(strcmp(entite,tab[i].name)!=0))
        { 
	        
			inserer(entite,code,type,val,i,0); 
	      
         }
       
        break;

   case 1:/*verifier si la case dans la tables des mots clés est libre*/
       
       for (i=0;((i<100)&&(tabm[i].state==1))&&(strcmp(entite,tabm[i].name)!=0);i++); 
        if((i<100)&&(strcmp(entite,tabm[i].name)!=0))
		{
          inserer(entite,code,type,val,i,1);
		  
		}
        
        break; 
    
   case 2:/*verifier si la case dans la tables des séparateurs est libre*/
         for (i=0;((i<100)&&(tabs[i].state==1))&&(strcmp(entite,tabs[i].name)!=0);i++); 
        if(i<100)
		{
         inserer(entite,code,type,val,i,2);
		 
		}
        
        break;

    
  }

}




void afficher()
{int i;

printf("____________________________________________________________________________________________\n");
	printf("\n||||||||||||||||||||||||||||||||||| La table des IDF |||||||||||||||||||||||||||||||||||\n");
	printf("____________________________________________________________________________________________\n");
printf("______________________________________________________________________________________\n");
printf("|  Nom_Entite \t\t\t\t  | Code_Entite | Type_Entite  | Val_Entite   | \n");
printf("______________________________________________________________________________________\n");
  
for(i=0;i<1000;i++)
{	
	
    if(tab[i].state==1)
      { 
        printf("|%40s |%12s | %12s | %12f |  \n",tab[i].name,tab[i].code,tab[i].type,tab[i].val);
         
      }
}

 
printf("____________________________________________________________________________________________\n");
	printf("\n||||||||||||||||||||||||||||||||||| La table de mot cles |||||||||||||||||||||||||||||||||||\n");
	printf("____________________________________________________________________________________________\n");

printf("________________________________\n");
printf("\t| NomEntite |  CodeEntite \n");
printf("________________________________\n");
  
for(i=0;i<100;i++)
    if(tabm[i].state==1)
      { 
        printf("\t|%10s|%12s | \n",tabm[i].name, tabm[i].type);
               
      }

printf("____________________________________________________________________________________________\n");
	printf("\n||||||||||||||||||||||||||||||||||| La table de separateurs |||||||||||||||||||||||||||||||\n");
	printf("____________________________________________________________________________________________\n");

printf("_____________________________________\n");
printf("\t| NomEntite |  CodeEntite | \n");
printf("_____________________________________\n");
  
for(i=0;i<100;i++)
    if(tabs[i].state==1)
      { 
        printf("\t|%10s |%12s | \n",tabs[i].name,tabs[i].type );
        
      }

}
void Routine_DivisionParZero(char* f,int nb_ligne,int col)
{
  printf("File \"%s\" , line %d , character %d : semantic error (division par 0) \n",f,nb_ligne,col);                                  
}
int Recherche_position(char entite[],int valeur)
{
		int i=0,j=0;
		switch(valeur)
		{ 
		case 0:
		{while(i<1000)
		{
		if (strcmp(entite,tab[i].name)==0){ return i;	}
		i++;
		}
		return -1;
		}

		case 1:
		{while(j<100)
		{
		
		if (strcmp(entite,tabm[j].name)==0) return j;	
		j++;
		}
		return -1;
		}
		}		
}
void insererTYPE(char entite[], char type[])
	{
       int pos;
	   pos=Recherche_position(entite,0);
	   if(pos!=-1)  { strcpy(tab[pos].type,type); }
	}
int doubleDeclaration(char entite[])
 { 
	int pos;
	pos=  Recherche_position(entite,0);
	if(strcmp(tab[pos].type," ")==0) return 0;
	   else return -1;
}
 void Routine_Declaration(char*f,char entite[],int nb_ligne,int col,char sauvType[])
{
  
	if(doubleDeclaration(entite)==0) 
  { 
    insererTYPE(entite,sauvType);
  }
  else 
  printf("File \"%s\" , line %d , character %d : semantic error ( Double declaration de: %s ) \n",f,nb_ligne,col,entite);
}
void Routine_Declaration1(char*f,char entite[],int nb_ligne,int col)
{
	if(doubleDeclaration(entite)==0)   
	    	printf("File \"%s\" , line %d , character %d : semantic error ( variable non declaree: %s ) \n",f,nb_ligne,col,entite);
}
void Initialiser_TailleTab(char entite[],int valeur)
{
	int pos;
	pos=Recherche_position(entite,0); 
	tab[pos].taille=valeur;  
  tab[pos].table=true;
}
int  IDF_NaPasDeValeur(char entite[]) 
{
	int pos;
	pos=Recherche_position(entite,0);
	   if((strcmp(tab[pos].code,"IDF_CST")==0)||(strcmp(tab[pos].code,"IDF_aValeur")==0))
	   return 0;
	   else 
	   return 1;
}
char* Routine_RecupererValeur(char entite[])
{
	int pos;
	pos=Recherche_position(entite,0); 
  sprintf(cc,"%f",tab[pos].val);
	return cc;
} 
void Routine_IDF_CST(char entite[])
{   int pos;
	pos=Recherche_position(entite,0);
	if(pos!=-1)  {strcpy(tab[pos].code,"IDF_CST");}
}
void incrementer_nbrChangement(char entite[])
{
  int pos;
	pos=Recherche_position(entite,0);
  if((strcmp(tab[pos].code,"IDF_CST")==0)&&(tab[pos].nbr_changement_val<=1))
  tab[pos].nbr_changement_val++;
}
void Routine_ValeurIDF(char entite[], double v)
{
  int pos;
	pos=Recherche_position(entite,0);
	tab[pos].val=v;
  incrementer_nbrChangement(entite);
}
void Routine_ValeurIDF2(char entite[], char entite2[])
{
  int pos,pos2;
	pos=Recherche_position(entite,0);
  pos2=Recherche_position(entite2,0);
	tab[pos].val=tab[pos2].val;
 
}
void Depasser_taille_tableau(char* f,char entite[],int valeur,int nb_ligne,int col)
{
	int pos;
	pos=Recherche_position(entite,0);
	if((valeur-tab[pos].taille) >=0)
	 printf("File \"%s\" , line %d , character %d : semantic error ( Depassement de taille de tableau ) \n",f,nb_ligne,col,entite);
}
void Depasser_taille_tableau2(char* f,char entite[],char entite2[],int nb_ligne,int col)
{
	int pos,pos2;
	pos=Recherche_position(entite,0);
  pos2=Recherche_position(entite2,0);
	if((tab[pos2].val-tab[pos].taille) >=0)
	 printf("File \"%s\" , line %d , character %d : semantic error ( Depassement de taille de tableau ) \n",f,nb_ligne,col,entite);
}
int Non_Tableau(char* f,char entite[],int nb_ligne,int col)
{  int pos;
   pos=Recherche_position(entite,0);
   if(tab[pos].table==false)
   {
     return 0;
   }
   else
    return 1;
}
int Routine_RetournType(char entite[])
{
	int pos;
	pos=Recherche_position(entite,0);
	if(strcmp(tab[pos].type,"INTEGER")==0)
	 return 1;
	else
	  if(strcmp(tab[pos].type,"REAL")==0)
	      return 0;
		  else
		     return -1;
}
int Routine_ReturnTypeBol(char entite[])
{
    int pos;
	  pos=Recherche_position(entite,0);
    if(strcmp(tab[pos].type,"BOL")==0)
	      return 1;
	  else
        return 0;
}
int NonChangemnt_Val_DeCst(char *f,char entite[],int nb_ligne,int col)
{
     incrementer_nbrChangement(entite);
	   int pos;
     pos=Recherche_position(entite,0);
	   if((strcmp(tab[pos].code,"IDF_CST")==0)&&(tab[pos].nbr_changement_val>1))
	   {printf("File \"%s\" , line %d , character %d : semantic error ( Changement interdit de val de la constante %s) \n",f,nb_ligne,col,entite);
	   return 0;}
	   else 
	   return 1;
}
void Routine_division(char* f,int nb_ligne,int col)
{
  printf("File \"%s\" , line %d , character %d : semantic error ( Division par 0 ) \n",f,nb_ligne,col);                                   
}
int Routine_Affecter_Int_To_Real2(char entite[],char entite1[])
{
	int pos,pos2;
    pos=Recherche_position(entite,0);
	  pos2=Recherche_position(entite1,0);
	if((strcmp(tab[pos].type,"REAL")==0)&&(strcmp(tab[pos2].type,"INTEGER")==0))
	   return 1;
	else
	   return 0;
}
int Routine_compatibilite(char entite[],char entite1[])
{
	int pos1,pos2;
	pos1=Recherche_position(entite,0);
	pos2=Recherche_position(entite1,0);
	
	if(strcmp(tab[pos1].type,tab[pos2].type)==0)
	 return 1;
	else
	 return 0;
}
int Routine_compa(char entite[],char type[])
{   int pos;
    pos=Recherche_position(entite,0);
	if(strcmp(tab[pos].type,type)==0)
	   return 1;
	else
	   return 0;
}
void initialiserTab(int position[100])
{ int f;
  for(f=0;f<100;f++)
  position[f]=-1;
}
void AjoutType(int position[100],char ty[])
{
  int f,pos;
  for(f=0;f<100;f++)
  { 
    if(position[f]!=-1)
    {pos=position[f];
    strcpy(tab[pos].type,ty);}
  }
  initialiserTab(position);
}
int Routine_Affecter_Int_To_Real(char entite[])
{
	int pos;
    pos=Recherche_position(entite,0); 
	if((strcmp(tab[pos].type,"INTEGER")==0)||(strcmp(tab[pos].type,"REAL")==0))
	   return 1;
	else
	   return 0;
}
char Routine_signe(char s[])
{
  char sig;int u=0;
  unsigned int i,k=0;
  for(i=0;s[i]!='\0';i++)
  {  
    if(s[i]=='@' || s[i]=='%' || s[i]=='&' || s[i]=='#' ||s[i]=='$' )
	{u++; sig=s[i]; }
  }
  if(u==1)
  return sig;
  else
   return '0';
}
void MessageErreurSigne_Formatage(char *f, int nb_ligne,int col)
{
  printf("File \"%s\" , line %d , character %d : semantic error ( Incompatibilite de type signe de formatage/idf ) \n",f,nb_ligne,col);
}
void Make_SIGN_TABLE (char* s,char* T,int *j){
	
	int i;
	for(i=0;s[i]!='\0';i++)
	{  
    if(s[i]=='@' || s[i]=='%' || s[i]=='&' || s[i]=='#' ||s[i]=='$')
		{
		T[*j] = s[i];
	  *j +=1; 
		}
	}
 }
