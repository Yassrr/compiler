%{
 #include "quadruplet.h"
 #include <stdio.h>
 #include <string.h>
 #include <stdlib.h>
 int nb_ligne=1,col=1;
 char* f;	
 char sauvType[20];
 int position[100]; 
 int indice=0;
 int tailleTab2 = 0;
 char tab2[100];
 char tabIDF[100][50];
 int ind = 0;
 char *end; char kk[10];
 float fl;
 int cptTemp=1;
 char temp[20];
 char tabtemp[10];
%}
%union {
#include<stdbool.h>
int   entier;
char* string;
float reel;
bool  bol;
struct
    {
     int type;
     int div_zero;
     char* ctx;
   }tt;
struct
    {
     char* pctx;
     char* gctx;
   }tabl;
}
%type <tabl>TERME_AFFECTANT  BLOC1_FOR
%left <string>chaine_car
%type <tt>EXP_AR EXP_ARI EXP_ARITHMITIQUE SUB_COND1 TERME_COND1 EXPRESSION_LOGIQUE TERME CONDITION DEBUT_FOR
%token plus chevron_ouv chevron_ferm point_excl mc_docpropgram sep_pvg mc_sub mc_var mc_body
%token mc_as sep_barre mc_array mc_int mc_chr mc_str mc_flt mc_bol mc_cons sep_vergule
%token  deux_points crochet_ouv crochet_ferm par_ouv par_ferm mc_input  mc_output dollars pourcentage
%token diaz et_commer arobase mc_if mc_then mc_else mc_do mc_while mc_for mc_until mc_aff <string>idf  egal
%token <entier>ent_n  <entier>ent_p  <reel>reel_p <reel>reel_n  <bol>booleen  <string>caracter  
%left op_or
%left op_and 
%left op_sup op_inf op_supe op_infe op_ega op_dif 
%left op_plus op_moins 
%left op_etoile op_div 
%left op_not  
%start PROG

%% 
PROG: ENTETE DECLARATION BODY PIED { printf("\n   PROGRAMME SYNTAXIQUEMENT CORRECT\n\n"); creerQuad("","","","");}
     | ENTETE BODY PIED {  printf("\n   PROGRAMME SYNTAXIQUEMENT CORRECT\n\n"); creerQuad("","","","");}
;



ENTETE:chevron_ouv point_excl mc_docpropgram idf chevron_ferm 
;


//******************************************************************************************************************************************
DECLARATION: chevron_ouv mc_sub mc_var chevron_ferm ENSEMBLE_DEC chevron_ouv op_div mc_sub mc_var chevron_ferm
           | chevron_ouv mc_sub mc_var chevron_ferm  chevron_ouv op_div mc_sub mc_var chevron_ferm
;
ENSEMBLE_DEC: TYPE_DECLARAIONS
   | ENSEMBLE_DEC TYPE_DECLARAIONS
;
TYPE_DECLARAIONS: DECLARATION_VAR 
                | DECLARATION_TAB 
                | DECLARATION_CONS 
;
DECLARATION_VAR:  chevron_ouv LISTE_IDF mc_as mc_int op_div chevron_ferm sep_pvg 
                  { AjoutType(position,"INTEGER"); indice=0;}
                | chevron_ouv LISTE_IDF mc_as mc_chr op_div chevron_ferm sep_pvg
                   { AjoutType(position,"CHAR"); indice=0;} 
                | chevron_ouv LISTE_IDF mc_as mc_str op_div chevron_ferm sep_pvg 
                    { AjoutType(position,"STRING"); indice=0;}
                | chevron_ouv LISTE_IDF mc_as  mc_flt op_div chevron_ferm sep_pvg 
                    { AjoutType(position,"REAL"); indice=0;}
                | chevron_ouv LISTE_IDF mc_as  mc_bol op_div chevron_ferm sep_pvg
                    { AjoutType(position,"BOL"); indice=0;} 
;
TYPE: mc_int {strcpy(sauvType,"INTEGER");}
    | mc_chr {strcpy(sauvType,"CHAR");}
    | mc_str {strcpy(sauvType,"STRING");}
    | mc_flt {strcpy(sauvType,"REAL");}
    | mc_bol {strcpy(sauvType,"BOL");}
;
LISTE_IDF: idf sep_barre LISTE_IDF
           { position[indice]=Recherche_position($1,0); indice++; 
             Routine_Declaration(f,$1,nb_ligne,col,sauvType);}
         | idf 
           { position[indice]=Recherche_position($1,0); indice++;
             Routine_Declaration(f,$1,nb_ligne,col,sauvType);
           }
;
DECLARATION_TAB: chevron_ouv mc_array mc_as TYPE chevron_ferm TABLEAUX chevron_ouv op_div mc_array chevron_ferm 
;
TABLEAUX: LISTE_TAB 
        | TABLEAUX LISTE_TAB
;
LISTE_TAB: chevron_ouv idf deux_points ent_p op_div chevron_ferm 
           {
            Routine_Declaration(f,$2,nb_ligne,col,sauvType);
            if($4==0) printf("File \"%s\" , line %d , character %d :semantic error ( La taille de tableau ne doit pas etre null ) \n",f,nb_ligne,col);
            else 
            {
               Initialiser_TailleTab($2,$4);
               int k=$4;
               itoa(k,kk,10);
               creerQuad("BOUNDS",kk,"","");
               creerQuad("ADEC",$2,"","");
            }
           }
        |  chevron_ouv idf deux_points par_ouv ent_n par_ferm op_div chevron_ferm 
           {printf("File \"%s\" , line %d , character %d :semantic error ( La taille de tableau doit etre un entier positif ) \n",f,nb_ligne,col);}
        |  chevron_ouv idf deux_points reel_p  op_div chevron_ferm
           {printf("File \"%s\" , line %d , character %d :semantic error ( La taille de tableau doit etre un entier ) \n",f,nb_ligne,col);}
        |  chevron_ouv idf deux_points par_ouv reel_n par_ferm op_div chevron_ferm
           {printf("File \"%s\" , line %d , character %d :semantic error ( La taille de tableau doit etre un entier positif ) \n",f,nb_ligne,col);}
        |  chevron_ouv idf deux_points idf op_div chevron_ferm
           {  Routine_Declaration(f,$2,nb_ligne,col,sauvType);
              Routine_Declaration1(f,$4,nb_ligne,col,sauvType); // dimension idf non  déclaré
              if(IDF_NaPasDeValeur($4)==1)
		          printf("File \"%s\" , line %d , character %d :semantic error ( Declaration impossible de tableau car %s est non initialise ) \n",f,nb_ligne,col,$4);
              else
               { 
               fl= strtof((char*)Routine_RecupererValeur($4),&end);
               if(fl<=0)
               printf("File \"%s\" , line %d , character %d :semantic error ( La taille de tableau doit etre un entier positif ) \n",f,nb_ligne,col);
               else 
               {
               Initialiser_TailleTab($2,(int)fl);
               creerQuad("BOUNDS",$4,"","");
               creerQuad("ADEC",$2,"","");
               }
               }
           }
;

DECLARATION_CONS: chevron_ouv mc_sub mc_cons chevron_ferm decVAR chevron_ouv op_div mc_sub mc_cons chevron_ferm 
                | chevron_ouv mc_sub mc_cons chevron_ferm LISTE_CONS chevron_ouv op_div mc_sub mc_cons chevron_ferm  
;
decVAR: decVAR DECLARATION_VARCONST
      | DECLARATION_VARCONST
;
DECLARATION_VARCONST: chevron_ouv LISTE_IDFconst mc_as mc_int op_div chevron_ferm sep_pvg 
                      { AjoutType(position,"INTEGER"); indice=0;}
                    | chevron_ouv LISTE_IDFconst mc_as mc_chr op_div chevron_ferm sep_pvg
                       { AjoutType(position,"CHAR"); indice=0;}
                    |  chevron_ouv LISTE_IDFconst mc_as mc_str op_div chevron_ferm sep_pvg
                      { AjoutType(position,"STRING"); indice=0;}
                    |  chevron_ouv LISTE_IDFconst mc_as mc_flt op_div chevron_ferm sep_pvg
                      { AjoutType(position,"REAL"); indice=0;}
                    | chevron_ouv LISTE_IDFconst mc_as mc_bol op_div chevron_ferm sep_pvg
                      { AjoutType(position,"BOL"); indice=0;} 
;
LISTE_IDFconst: idf sep_barre LISTE_IDFconst
               { position[indice]=Recherche_position($1,0); indice++;
                 Routine_IDF_CST($1); Routine_Declaration(f,$1,nb_ligne,col,sauvType);}
               | idf 
               { position[indice]=Recherche_position($1,0); indice++;
                 Routine_IDF_CST($1); Routine_Declaration(f,$1,nb_ligne,col,sauvType);}
;
LISTE_CONS:  DEC_CONST 
          |  LISTE_CONS DEC_CONST 
;
DEC_CONST:chevron_ouv idf egal caracter op_div chevron_ferm sep_pvg 
          { 
            Routine_Declaration(f,$2,nb_ligne,col,"CHAR"); 
	          Routine_IDF_CST($2);
            incrementer_nbrChangement($2); 
          }
          |chevron_ouv idf egal par_ouv ent_n par_ferm op_div chevron_ferm sep_pvg 
          { 
            Routine_Declaration(f,$2,nb_ligne,col,"INTEGER");
	         Routine_IDF_CST($2);
	         float y= (float)$5;
	         Routine_ValeurIDF($2,y);
          }
          |chevron_ouv idf egal ent_p op_div chevron_ferm sep_pvg
          {
            Routine_Declaration(f,$2,nb_ligne,col,"INTEGER");
            Routine_IDF_CST($2);
            float y= (float)$4;
	         Routine_ValeurIDF($2,y);
          }
          |chevron_ouv idf egal reel_p op_div chevron_ferm sep_pvg
          {
            Routine_Declaration(f,$2,nb_ligne,col,"REAL");
            Routine_IDF_CST($2);
            Routine_ValeurIDF($2,$4);
          }
          |chevron_ouv idf egal par_ouv reel_n par_ferm op_div chevron_ferm sep_pvg
          {
            Routine_Declaration(f,$2,nb_ligne,col,"REAL");
            Routine_IDF_CST($2);
            Routine_ValeurIDF($2,$5);
          }
          |chevron_ouv idf egal chaine_car op_div chevron_ferm sep_pvg
          { 
            Routine_Declaration(f,$2,nb_ligne,col,"STRING");
            Routine_IDF_CST($2);
            incrementer_nbrChangement($2);
          }
          |chevron_ouv idf egal booleen op_div chevron_ferm sep_pvg
          { 
            Routine_Declaration(f,$2,nb_ligne,col,"BOL");
            Routine_IDF_CST($2);
            incrementer_nbrChangement($2);
          }
;
//*****************************************************************************************************************************



BODY: chevron_ouv mc_body chevron_ferm LISTE_INSTRUCTION chevron_ouv op_div mc_body  chevron_ferm
    | chevron_ouv mc_body chevron_ferm chevron_ouv op_div mc_body  chevron_ferm  
;
LISTE_INSTRUCTION: INSTRUCTION 
                 | LISTE_INSTRUCTION INSTRUCTION
;
INSTRUCTION: AFFECTATION
            | EENTREE
            | SORTIE
            | COND_IF
            | DO_WHILE
            | BOUCLE_FOR 
              
;




//***************************************************************************************************************************************************************************
AFFECTATION:  chevron_ouv mc_aff deux_points TERME_AFFECTANT sep_vergule  caracter op_div chevron_ferm
              { NonChangemnt_Val_DeCst(f,$4.pctx,nb_ligne,col); 
                if(Routine_compa($4.pctx,"CHAR")==0)  printf("File \"%s\" , line %d , character %d : semantic error ( Incompatibilite de type ) \n",f,nb_ligne,col);
                creerQuad("AFF",$6,"",$4.gctx);
              }
            | chevron_ouv mc_aff deux_points TERME_AFFECTANT sep_vergule  chaine_car op_div chevron_ferm
              { NonChangemnt_Val_DeCst(f,$4.pctx,nb_ligne,col); 
                if(Routine_compa($4.pctx,"STRING")==0)  printf("File \"%s\" , line %d , character %d : semantic error ( Incompatibilite de type ) \n",f,nb_ligne,col);
                creerQuad("AFF",$6,"",$4.gctx);
              }
            | chevron_ouv mc_aff deux_points TERME_AFFECTANT sep_vergule  TERME_AFFECTANT op_div chevron_ferm
              { NonChangemnt_Val_DeCst(f,$4.pctx,nb_ligne,col); 
                if(Routine_Affecter_Int_To_Real2($4.pctx,$6.pctx)==0)
		             {
                  if(Routine_compatibilite($4.pctx,$6.pctx)==0)
                    { printf("File \"%s\" , line %d , character %d : semantic error ( Incompatibilite de type ) \n",f,nb_ligne,col);}
                  else  Routine_ValeurIDF2($4.pctx,$6.pctx);
                  }
                  else  Routine_ValeurIDF2($4.pctx,$6.pctx);
                  creerQuad("AFF",$6.gctx,"",$4.gctx);
              }
            | chevron_ouv mc_aff deux_points TERME_AFFECTANT sep_vergule  ent_p op_div chevron_ferm
              { NonChangemnt_Val_DeCst(f,$4.pctx,nb_ligne,col); 
                  if(Routine_Affecter_Int_To_Real($4.pctx)==0) printf("File \"%s\" , line %d , character %d : semantic error ( Incompatibilite de type ) \n",f,nb_ligne,col);
                  else
                    {  
                       float y= (float)$6;
		                   Routine_ValeurIDF($4.pctx,y);
                    }
                int k=$6; itoa(k,kk,10); 
                creerQuad("AFF",strdup(kk),"",$4.gctx);
              } 
            | chevron_ouv mc_aff deux_points TERME_AFFECTANT sep_vergule  reel_p op_div chevron_ferm
              { NonChangemnt_Val_DeCst(f,$4.pctx,nb_ligne,col); 
                if(Routine_compa($4.pctx,"REAL")==0)  printf("File \"%s\" , line %d , character %d : semantic error ( Incompatibilite de type ) \n",f,nb_ligne,col);
                else Routine_ValeurIDF($4.pctx,$6);
                sprintf(kk,"%f",$6); creerQuad("AFF",strdup(kk),"",$4.gctx);
              }
            | chevron_ouv mc_aff deux_points TERME_AFFECTANT sep_vergule  par_ouv reel_n par_ferm op_div chevron_ferm
              { NonChangemnt_Val_DeCst(f,$4,nb_ligne,col); 
                if(Routine_compa($4.pctx,"REAL")==0)  printf("File \"%s\" , line %d , character %d : semantic error ( Incompatibilite de type ) \n",f,nb_ligne,col);
                else Routine_ValeurIDF($4.pctx,$7);
                sprintf(kk,"%f",$7); creerQuad("AFF",strdup(kk),"",$4.gctx);
              }
            | chevron_ouv mc_aff deux_points TERME_AFFECTANT sep_vergule  par_ouv ent_n par_ferm op_div chevron_ferm
              { NonChangemnt_Val_DeCst(f,$4.pctx,nb_ligne,col); 
                if(Routine_Affecter_Int_To_Real($4.pctx)==0) printf("File \"%s\" , line %d , character %d : semantic error ( Incompatibilite de type ) \n",f,nb_ligne,col);
                  else
                    {  
                       float y= (float)$7;
		                   Routine_ValeurIDF($4.pctx,y);
                    }
                int k=$7; itoa(k,kk,10); 
                creerQuad("AFF",strdup(kk),"",$4.gctx);
              }
            | chevron_ouv mc_aff deux_points TERME_AFFECTANT sep_vergule  EXP_ARITHMITIQUE op_div chevron_ferm
              { NonChangemnt_Val_DeCst(f,$4.pctx,nb_ligne,col); 
                if(($6.type==1)&&(Routine_RetournType($4.pctx)==-1))   printf("File \"%s\" , line %d , character %d : semantic error ( Incompatibilite de type ) \n",f,nb_ligne,col);
                if((Routine_RetournType($4.pctx)!=0)&&($6.type==2))    printf("File \"%s\" , line %d , character %d : semantic error ( Incompatibilite de type ) \n",f,nb_ligne,col);
                creerQuad("AFF",$6.ctx,"",$4.gctx);
              }
            | chevron_ouv mc_aff deux_points TERME_AFFECTANT sep_vergule  CONDITION op_div chevron_ferm
              { NonChangemnt_Val_DeCst(f,$4.pctx,nb_ligne,col); 
                if(Routine_ReturnTypeBol($4.pctx)!=1)
                printf("File \"%s\" , line %d , character %d : semantic error ( Incompatibilite de type ) \n",f,nb_ligne,col);
                creerQuad("AFF",$6.ctx,"",$4.gctx);
              }
            | chevron_ouv mc_aff deux_points TERME_AFFECTANT sep_vergule  booleen op_div chevron_ferm
            { NonChangemnt_Val_DeCst(f,$4.pctx,nb_ligne,col); 
                if(Routine_compa($4.pctx,"BOL")==0)  printf("File \"%s\" , line %d , character %d : semantic error ( Incompatibilite de type ) \n",f,nb_ligne,col);
               
               if($6) creerQuad("AFF","TRUE","",$4.gctx);
                  else  creerQuad("AFF","FALSE","",$4.gctx);
            }
;
TERME_AFFECTANT: idf
                { $$.pctx=$1;$$.gctx=$1;
                  Routine_Declaration1(f,$1,nb_ligne,col,sauvType);
                  if(Non_Tableau(f,$1, nb_ligne, col)==1)
                  printf("File \"%s\" , line %d , character %d :semantic error ( il faut preciser un indice de tableau %s ) \n",f,nb_ligne,col,$1);
                }
               | idf crochet_ouv idf crochet_ferm
               {  $$.pctx=$1;
                  strcpy(tabtemp,"");
                  strcat(strcat(strcat(strcat(tabtemp,$1),"["),$3),"]");
                  $$.gctx=tabtemp;
                  Routine_Declaration1(f,$1,nb_ligne,col,sauvType);
                  Routine_Declaration1(f,$3,nb_ligne,col,sauvType);
                  if(Non_Tableau(f,$1, nb_ligne, col)!=0)
                     {
                     if(Routine_RetournType($3)==1)
                         Depasser_taille_tableau2(f,$1,$3,nb_ligne,col);
                     else
                         printf("File \"%s\" , line %d , character %d :semantic error ( l'indice de tableau doit etre de type entier ) \n",f,nb_ligne,col);
                     }
                  else
                  printf("File \"%s\" , line %d , character %d : semantic error ( %s n'est pas un tableau) \n",f,nb_ligne,col,$1);
               }
               | idf crochet_ouv ent_p crochet_ferm
               {  $$.pctx=$1;
                  int k=$3; itoa(k,kk,10); strcpy(tabtemp,"");
                  strcat(strcat(strcat(strcat(tabtemp,$1),"["),strdup(kk)),"]");
                  $$.gctx=tabtemp;
                  Routine_Declaration1(f,$1,nb_ligne,col,sauvType);
                  if(Non_Tableau(f,$1, nb_ligne, col)!=0)
                   Depasser_taille_tableau(f,$1,$3,nb_ligne,col);
                  else
                   printf("File \"%s\" , line %d , character %d : semantic error ( %s n'est pas un tableau) \n",f,nb_ligne,col,$1);
               }
               | idf crochet_ouv par_ouv ent_n par_ferm crochet_ferm
               {  $$.pctx="";$$.gctx="";
                  printf("File \"%s\" , line %d , character %d :semantic error ( La taille de tableau doit etre un entier positif ) \n",f,nb_ligne,col);}
               | idf crochet_ouv reel_p crochet_ferm
               {  $$.pctx="";$$.gctx="";
                  printf("File \"%s\" , line %d , character %d :semantic error ( La taille de tableau doit etre un entier ) \n",f,nb_ligne,col);}
               |idf crochet_ouv par_ouv reel_n par_ferm crochet_ferm
               {  $$.pctx="";$$.gctx="";
                  printf("File \"%s\" , line %d , character %d :semantic error ( La taille de tableau doit etre un entier positif ) \n",f,nb_ligne,col);}
;  
//***********************************************************************************************************************************************************************



//***********************************************************************************************************************************************************************
EENTREE: chevron_ouv mc_input deux_points idf chaine_car op_div chevron_ferm
        {
           Routine_Declaration1(f,$4,nb_ligne,col,sauvType);
           if(Non_Tableau(f,$4, nb_ligne, col)==1)
            printf("File \"%s\" , line %d , character %d :semantic error ( il faut preciser un indice de tableau %s ) \n",f,nb_ligne,col,$4);
            if(NonChangemnt_Val_DeCst(f,$4,nb_ligne,col)==1)
            { char* tab =$5;
              if(Routine_signe(tab)=='0')
              printf("File \"%s\" , line %d , character %d : semantic error ( un seul signe de formatage pour INPUT) \n",f,nb_ligne,col);
              else 
              {
              if(Routine_signe(tab)=='#'){if(Routine_compa($4,"STRING")==0) MessageErreurSigne_Formatage(f,nb_ligne,col);}
              if(Routine_signe(tab)=='$'){if(Routine_compa($4,"INTEGER")==0) MessageErreurSigne_Formatage(f,nb_ligne,col);}
              if(Routine_signe(tab)=='%'){if(Routine_compa($4,"REAL")==0)  MessageErreurSigne_Formatage(f,nb_ligne,col);}
              if(Routine_signe(tab)=='&'){if(Routine_compa($4,"CHAR")==0)  MessageErreurSigne_Formatage(f,nb_ligne,col);}
              if(Routine_signe(tab)=='@'){if(Routine_compa($4,"BOL")==0)  MessageErreurSigne_Formatage(f,nb_ligne,col);}
              }
            }
        }
;
//****************************************************************************************************************************************************



//****************************************************************************************************************************************************
SORTIE:chevron_ouv mc_output deux_points EXP_SORTIE2 op_div chevron_ferm
;
EXP_SORTIE2: EXP_SORTIE2 op_plus EXP_SORTIE2
            |chaine_car op_plus LISTE_IDF_SORTIE 
              { int j=0;
                Make_SIGN_TABLE($1,tab2,&tailleTab2);
                if (tailleTab2 != ind){
                printf("File \"%s\" , line %d , character %d : semantic error ( nbr de signes de formatage != nbr idf ) \n",f,nb_ligne,col);
                  }else{
                  for( j = 0;j<ind;j++){
                  if(tab2[j]=='#'){if(Routine_compa(tabIDF[j],"STRING")==0) MessageErreurSigne_Formatage(f,nb_ligne,col);}
                  if(tab2[j]=='$'){if(Routine_compa(tabIDF[j],"INTEGER")==0) MessageErreurSigne_Formatage(f,nb_ligne,col);}
                  if(tab2[j]=='%'){if(Routine_compa(tabIDF[j],"REAL")==0) MessageErreurSigne_Formatage(f,nb_ligne,col);}
                  if(tab2[j]=='&'){if(Routine_compa(tabIDF[j],"CHAR")==0) MessageErreurSigne_Formatage(f,nb_ligne,col);}
                  if(tab2[j]=='@'){if(Routine_compa(tabIDF[j],"BOL")==0) MessageErreurSigne_Formatage(f,nb_ligne,col);}
                  }
                }
                tailleTab2 = 0;
                ind = 0;}

              | chaine_car
              {
                Make_SIGN_TABLE($1,tab2,&tailleTab2);
                int j=0;
                if (tailleTab2 != ind)
                {
                  printf("File \"%s\" , line %d , character %d : semantic error ( nbr de signes de formatage != nbr idf ) \n",f,nb_ligne,col);
                  }else{
                  for( j = 0;j<ind;j++){
                  if(tab2[j]=='#'){if(Routine_compa(tabIDF[j],"STRING")==0) MessageErreurSigne_Formatage(f,nb_ligne,col);}
                  if(tab2[j]=='$'){if(Routine_compa(tabIDF[j],"INTEGER")==0) MessageErreurSigne_Formatage(f,nb_ligne,col);}
                  if(tab2[j]=='%'){if(Routine_compa(tabIDF[j],"REAL")==0) MessageErreurSigne_Formatage(f,nb_ligne,col);}
                  if(tab2[j]=='&'){if(Routine_compa(tabIDF[j],"CHAR")==0) MessageErreurSigne_Formatage(f,nb_ligne,col);}
                  if(tab2[j]=='@'){if(Routine_compa(tabIDF[j],"BOL")==0) MessageErreurSigne_Formatage(f,nb_ligne,col);}
                  }
                }
                tailleTab2=0;
                ind=0;
              }
;
LISTE_IDF_SORTIE: LISTE_IDF_SORTIE sep_barre  idf 
                {
                   Routine_Declaration1(f,$3,nb_ligne,col,sauvType);
                   if(Non_Tableau(f,$3, nb_ligne, col)==1)
                   printf("File \"%s\" , line %d , character %d :semantic error ( il faut preciser un indice de tableau %s ) \n",f,nb_ligne,col,$3);
                   strcpy(tabIDF[ind],$3);  ind++;
                }
                | idf
                {
                   Routine_Declaration1(f,$1,nb_ligne,col,sauvType);
                   if(Non_Tableau(f,$1, nb_ligne, col)==1)
                   printf("File \"%s\" , line %d , character %d :semantic error ( il faut preciser un indice de tableau %s ) \n",f,nb_ligne,col,$1);
                   strcpy(tabIDF[ind],$1);  ind++;
                }
;
//***********************************************************************************************************************************************************




//***********************************************************************************************************************************************************
COND_IF: DEBUT_IF BLOC_THEN BLOC_ELSE FIN_IF
        {EnsembleQuad[atoi(depiler(&pile1))].opd1=ConvertirEnChaine(qc);}
       | DEBUT_IF BLOC_THEN  FIN_IF
        {EnsembleQuad[atoi(depiler(&pile1))].opd1=ConvertirEnChaine(qc);}
;
DEBUT_IF: chevron_ouv mc_if deux_points CONDITION chevron_ferm 
        { empiler_Int(&pile1,qc);
          creerQuad("BZ","",EnsembleQuad[qc-1].res,"");}
;
CONDITION:EXPRESSION_LOGIQUE
         {$$.ctx=strdup($1.ctx);}
         | SUB_COND1
         {$$.ctx=strdup($1.ctx);}
; 
BLOC_THEN: chevron_ouv mc_then chevron_ferm  LISTE_INSTRUCTION chevron_ouv op_div mc_then chevron_ferm
;
BLOC_ELSE: chevron_ouv mc_else
          { EnsembleQuad[atoi(depiler(&pile1))].opd1=ConvertirEnChaine(qc+1);
            empiler_Int(&pile1,qc);
            creerQuad("BR","","","");
          }  chevron_ferm LISTE_INSTRUCTION  chevron_ouv op_div mc_else chevron_ferm
;
FIN_IF: chevron_ouv op_div mc_if chevron_ferm
;
//***********************************************************************************************************************************************************




//***********************************************************************************************************************************************************
EXPRESSION_LOGIQUE:  op_and par_ouv TERME sep_vergule TERME par_ferm 
                    {  
                       sprintf(temp,"T%d",cptTemp);
                       creerQuadL(3,$3.ctx,$5.ctx,temp);
                       $$.ctx=temp; 
                       cptTemp++;
                    }
                   | op_or par_ouv  TERME sep_vergule TERME  par_ferm 
                    {  sprintf(temp,"T%d",cptTemp);
                       creerQuadL(2,$3.ctx,$5.ctx,temp);
                       $$.ctx=temp;
                       cptTemp++;
                    }
                   | op_not par_ouv  TERME par_ferm  
                    {  sprintf(temp,"T%d",cptTemp);
                       creerQuadL(1,$3.ctx,"",temp);
                       $$.ctx=temp;
                       cptTemp++;}
;
/*OPERANDE2: TERME sep_vergule OPERANDE2
           {$$.ctx=$3.ctx;}
          | TERME
           {$$.ctx=$1.ctx;}
;*/
TERME:  SUB_COND1
       {$$.ctx=strdup($1.ctx);}
      | booleen
       { if($1) $$.ctx="TRUE";
           else  $$.ctx="FALSE";}
      | EXPRESSION_LOGIQUE 
       {$$.ctx=strdup($1.ctx);}
      | TERME_AFFECTANT
       { $$.ctx=$1.gctx;
        if(Routine_ReturnTypeBol($1)==0)
        printf("File \"%s\" , line %d , character %d : semantic error (%s n'est pas un booleen) \n",f,nb_ligne,col,$1);}
;

TERME_COND1:EXP_AR 
            {$$.ctx=$1.ctx;}
           |EXP_ARITHMITIQUE
            {$$.ctx=$1.ctx;}
;
SUB_COND1: op_sup par_ouv TERME_COND1  sep_vergule TERME_COND1  par_ferm
           {  
              sprintf(temp,"T%d",cptTemp);
              creerQuadA(6,$3.ctx,$5.ctx,temp);
              $$.ctx=temp;
              cptTemp++;}
          |op_supe par_ouv TERME_COND1   sep_vergule TERME_COND1  par_ferm
          {
              sprintf(temp,"T%d",cptTemp);
              creerQuadA(3,$3.ctx,$5.ctx,temp);
              $$.ctx=temp;
              cptTemp++;}
          |op_inf par_ouv TERME_COND1  sep_vergule TERME_COND1  par_ferm
           {
              sprintf(temp,"T%d",cptTemp);
              creerQuadA(5,$3.ctx,$5.ctx,temp);
              $$.ctx=temp;
              cptTemp++;}
          |op_infe par_ouv TERME_COND1  sep_vergule TERME_COND1 par_ferm
           {
              sprintf(temp,"T%d",cptTemp);
              creerQuadA(4,$3.ctx,$5.ctx,temp);
              $$.ctx=temp;
              cptTemp++;}
          |op_ega par_ouv TERME_COND1 sep_vergule TERME_COND1  par_ferm
           {
              sprintf(temp,"T%d",cptTemp);
              creerQuadA(1,$3.ctx,$5.ctx,temp);
              $$.ctx=temp;
              cptTemp++;}
          |op_dif par_ouv TERME_COND1  sep_vergule TERME_COND1  par_ferm
           {
              sprintf(temp,"T%d",cptTemp);
              creerQuadA(2,$3.ctx,$5.ctx,temp);
              $$.ctx=temp;
              cptTemp++;}
          
;
//***********************************************************************************************************************************





//***********************************************************************************************************************************
DO_WHILE: DEBUT_DO LISTE_INSTRUCTION BLOC_WHILE FIN_DO
          {  
            creerQuad("BNZ",depiler(&pile3),EnsembleQuad[qc-1].res,"");
          }
;
DEBUT_DO: chevron_ouv mc_do chevron_ferm
          {
           empiler_Int(&pile3,qc);
          }
;
BLOC_WHILE: chevron_ouv mc_while deux_points CONDITION op_div chevron_ferm
          
;
FIN_DO: chevron_ouv op_div mc_do chevron_ferm
;
//********************************************************************************************************************




//********************************************************************************************************************
BOUCLE_FOR: DEBUT_FOR {
               creerQuad("BR",RenvoyerSommetPile(pile1),"","");
               strcpy(tempp,depiler(&pile2));
               EnsembleQuad[atoi(depiler(&pile2))].opd1=ConvertirEnChaine(qc);
               empiler_Str(&pile2,tempp);} 
               LISTE_INSTRUCTION FIN_FOR
               { 
                 sprintf(temp,"T%d",cptTemp);
                 creerQuad("+",$1.ctx,"1",temp);
                 creerQuad("AFF",temp,"",$1.ctx);
                 cptTemp++;
                 creerQuad("BR",depiler(&pile2),"","");
                 EnsembleQuad[atoi(depiler(&pile2))].opd1=ConvertirEnChaine(qc);}
; 
DEBUT_FOR: BLOC1_FOR  TERME_AFFECTANT mc_until  TERME_AFFECTANT  chevron_ferm
            {   $$.ctx=$1.gctx;
                empiler_Int(&pile2,qc);
                creerQuad("BG","",$2.gctx,$4.gctx);
                empiler_Int(&pile2,qc);
                creerQuad("BR","","","");
                empiler_Int(&pile2,qc);
                NonChangemnt_Val_DeCst(f,$1.pctx,nb_ligne,col); 
                if(Routine_Affecter_Int_To_Real2($1.pctx,$2.pctx)==0)
		             {
                  if(Routine_compatibilite($1.pctx,$2.pctx)==0)
                    { printf("File \"%s\" , line %d , character %d : semantic error ( Incompatibilite de type ) \n",f,nb_ligne,col);}
                  else  Routine_ValeurIDF2($1.pctx,$2.pctx);
                  }
                  else  Routine_ValeurIDF2($1.pctx,$2.pctx);
            }
         | BLOC1_FOR ent_p mc_until  TERME_AFFECTANT chevron_ferm
           {  $$.ctx=$1.gctx;
              empiler_Int(&pile2,qc);
              creerQuad("BG","",$1.gctx,$4.gctx);
              empiler_Int(&pile2,qc);
              creerQuad("BR","","","");
              empiler_Int(&pile2,qc);
             NonChangemnt_Val_DeCst(f,$1.pctx,nb_ligne,col); 
              if(Routine_Affecter_Int_To_Real($1.pctx)==0) printf("File \"%s\" , line %d , character %d : semantic error ( Incompatibilite de type ) \n",f,nb_ligne,col);
                  else
                    {  
                       float y= (float)$2;
		                   Routine_ValeurIDF($1.pctx,y);
                    }       
            }
         | BLOC1_FOR ent_p mc_until  ent_p chevron_ferm
           {  $$.ctx=$1.gctx;
              empiler_Int(&pile2,qc);
              int k=$4; itoa(k,kk,10);
              creerQuad("BG","",$1.gctx,strdup(kk));
              empiler_Int(&pile2,qc);
              creerQuad("BR","","","");
              empiler_Int(&pile2,qc);
             NonChangemnt_Val_DeCst(f,$1.pctx,nb_ligne,col); 
              if(Routine_Affecter_Int_To_Real($1.pctx)==0) printf("File \"%s\" , line %d , character %d : semantic error ( Incompatibilite de type ) \n",f,nb_ligne,col);
                  else
                    {  
                       float y= (float)$2;
		                   Routine_ValeurIDF($1.pctx,y);
                    }       
            }
         | BLOC1_FOR par_ouv ent_n par_ferm mc_until TERME_AFFECTANT chevron_ferm
           {  $$.ctx=$1.gctx;
              empiler_Int(&pile2,qc);
              creerQuad("BG","",$1.gctx,$6.gctx);
              empiler_Int(&pile2,qc);
              creerQuad("BR","","","");
              empiler_Int(&pile2,qc);
             NonChangemnt_Val_DeCst(f,$1.pctx,nb_ligne,col); 
              if(Routine_Affecter_Int_To_Real($1.pctx)==0) printf("File \"%s\" , line %d , character %d : semantic error ( Incompatibilite de type ) \n",f,nb_ligne,col);
                  else
                    {  
                       float y= (float)$3;
		                   Routine_ValeurIDF($1.pctx,y);
                    }       
            }
         | BLOC1_FOR par_ouv ent_n par_ferm mc_until  par_ouv ent_n par_ferm chevron_ferm
           {  $$.ctx=$1.gctx;
              empiler_Int(&pile2,qc);
              int k=$7; itoa(k,kk,10);
              creerQuad("BG","",$1.gctx,strdup(kk));
              empiler_Int(&pile2,qc);
              creerQuad("BR","","","");
              empiler_Int(&pile2,qc);
             NonChangemnt_Val_DeCst(f,$1.pctx,nb_ligne,col); 
              if(Routine_Affecter_Int_To_Real($1.pctx)==0) printf("File \"%s\" , line %d , character %d : semantic error ( Incompatibilite de type ) \n",f,nb_ligne,col);
                  else
                    {  
                       float y= (float)$3;
		                   Routine_ValeurIDF($1.pctx,y);
                    }       
            }
         | BLOC1_FOR reel_p mc_until TERME_AFFECTANT chevron_ferm
           {  $$.ctx=$1.gctx;
              empiler_Int(&pile2,qc);
              creerQuad("BG","",$1.gctx,$4.gctx);
              empiler_Int(&pile2,qc);
              creerQuad("BR","","","");
              empiler_Int(&pile2,qc);
             NonChangemnt_Val_DeCst(f,$1.pctx,nb_ligne,col); 
                if(Routine_compa($1.pctx,"REAL")==0)  printf("File \"%s\" , line %d , character %d : semantic error ( Incompatibilite de type ) \n",f,nb_ligne,col);
                else Routine_ValeurIDF($1.pctx,$2);
            }
         | BLOC1_FOR reel_p mc_until reel_p chevron_ferm
           {  $$.ctx=$1.gctx;
              empiler_Int(&pile2,qc);
              sprintf(kk,"%f",$4);
              creerQuad("BG","",$1.gctx,strdup(kk));
              empiler_Int(&pile2,qc);
              creerQuad("BR","","","");
              empiler_Int(&pile2,qc);
             NonChangemnt_Val_DeCst(f,$1.pctx,nb_ligne,col); 
                if(Routine_compa($1.pctx,"REAL")==0)  printf("File \"%s\" , line %d , character %d : semantic error ( Incompatibilite de type ) \n",f,nb_ligne,col);
                else Routine_ValeurIDF($1.pctx,$2);
            }
         | BLOC1_FOR par_ouv reel_n par_ferm mc_until TERME_AFFECTANT chevron_ferm
           {  $$.ctx=$1.gctx;
              empiler_Int(&pile2,qc);
              creerQuad("BG","",$1.gctx,$6.gctx);
              empiler_Int(&pile2,qc);
              creerQuad("BR","","","");
              empiler_Int(&pile2,qc);
             NonChangemnt_Val_DeCst(f,$1.pctx,nb_ligne,col); 
                if(Routine_compa($1.pctx,"REAL")==0)  printf("File \"%s\" , line %d , character %d : semantic error ( Incompatibilite de type ) \n",f,nb_ligne,col);
                else Routine_ValeurIDF($1.pctx,$3);
            }
         | BLOC1_FOR par_ouv reel_n par_ferm mc_until par_ouv reel_n par_ferm chevron_ferm
           {  $$.ctx=$1.gctx;
              empiler_Int(&pile2,qc);
              sprintf(kk,"%f",$7);
              creerQuad("BG","",$1.gctx,strdup(kk));
              empiler_Int(&pile2,qc);
              creerQuad("BR","","","");
              empiler_Int(&pile2,qc);
              NonChangemnt_Val_DeCst(f,$1.pctx,nb_ligne,col); 
                if(Routine_compa($1.pctx,"REAL")==0)  printf("File \"%s\" , line %d , character %d : semantic error ( Incompatibilite de type ) \n",f,nb_ligne,col);
                else Routine_ValeurIDF($1.pctx,$3);
            }
         | BLOC1_FOR caracter mc_until TERME_AFFECTANT chevron_ferm
           {  $$.ctx=$1.gctx;
              empiler_Int(&pile2,qc);
              creerQuad("BG","",$1.gctx,$4.gctx);
              empiler_Int(&pile2,qc);
              creerQuad("BR","","","");
              empiler_Int(&pile2,qc);
             NonChangemnt_Val_DeCst(f,$1.pctx,nb_ligne,col); 
                if(Routine_compa($1.pctx,"CHAR")==0)  printf("File \"%s\" , line %d , character %d : semantic error ( Incompatibilite de type ) \n",f,nb_ligne,col);
              }
         | BLOC1_FOR caracter mc_until caracter chevron_ferm
           {  $$.ctx=$1.gctx;
              empiler_Int(&pile2,qc);
              creerQuad("BG","",$1.gctx,$4);
              empiler_Int(&pile2,qc);
              creerQuad("BR","","","");
              empiler_Int(&pile2,qc);
             NonChangemnt_Val_DeCst(f,$1.pctx,nb_ligne,col); 
                if(Routine_compa($1.pctx,"CHAR")==0)  printf("File \"%s\" , line %d , character %d : semantic error ( Incompatibilite de type ) \n",f,nb_ligne,col);
              }
;
BLOC1_FOR:chevron_ouv mc_for TERME_AFFECTANT egal 
         {$$.gctx=$3.gctx; $$.pctx=$3.pctx;  empiler_Int(&pile1,qc);}
;
FIN_FOR: chevron_ouv op_div mc_for chevron_ferm
        {depiler(&pile1);}
;
//*******************************************************************************************************************************




//********************************************************************************************************************************
EXP_ARITHMITIQUE:EXP_ARITHMITIQUE op_plus EXP_ARITHMITIQUE
                 {if(($1.type==1)&&($3.type==1)) $$.type=1;
                              else $$.type=2;
                    $1.ctx=strdup($$.ctx);
                    sprintf($$.ctx,"T%d",cptTemp);
                    creerQuad("+",$1.ctx,$3.ctx,$$.ctx);
                    cptTemp++; }
                 |EXP_ARITHMITIQUE op_moins EXP_ARITHMITIQUE
                 {if(($1.type==1)&&($3.type==1)) $$.type=1;
                              else $$.type=2;
                    $1.ctx=strdup($$.ctx);
                    sprintf($$.ctx,"T%d",cptTemp);
                    creerQuad("-",$1.ctx,$3.ctx,$$.ctx);
                    cptTemp++; }
                 |EXP_ARITHMITIQUE op_div EXP_ARITHMITIQUE
                 {if($3.div_zero==9) Routine_division(f,nb_ligne, col);
                  $$.type=2;
                    $1.ctx=strdup($$.ctx);
                    sprintf($$.ctx,"T%d",cptTemp);
                    creerQuad("/",$1.ctx,$3.ctx,$$.ctx);
                    cptTemp++; 
                  }
                 |EXP_ARITHMITIQUE op_etoile EXP_ARITHMITIQUE
                 {if(($1.type==1)&&($3.type==1)) $$.type=1;
                              else $$.type=2;
                    $1.ctx=strdup($$.ctx);
                    sprintf($$.ctx,"T%d",cptTemp);
                    creerQuad("*",$1.ctx,$3.ctx,$$.ctx);
                    cptTemp++; }
                 |EXP_ARI
                 {$$.div_zero=$1.div_zero;$$.type=$1.type;
                   $$.ctx=$1.ctx;}
                 | par_ouv EXP_ARITHMITIQUE par_ferm
                  {$$.div_zero=$2.div_zero;$$.type=$2.type;
                   $$.ctx=$2.ctx;}
;
EXP_ARI: par_ouv EXP_AR par_ferm 
        {$$.div_zero=$2.div_zero;$$.type=$2.type;
                   $$.ctx=$2.ctx;}
;
EXP_AR: EXP_AR op_plus EXP_AR
        {if(($1.div_zero==9)&&($3.div_zero==9)) $$.div_zero=9;
         else $$.div_zero=4;
         if(($1.type==1)&&($3.type==1)) $$.type=1;
                else $$.type=2;
        $1.ctx=strdup($$.ctx);
        sprintf($$.ctx,"T%d",cptTemp);
        creerQuad("+",$1.ctx,$3.ctx,$$.ctx);
        cptTemp++; 
      }
       |EXP_AR op_moins EXP_AR
       {if(($1.div_zero==9)&&($3.div_zero==9)) $$.div_zero=9;
         else $$.div_zero=4;
         if(($1.type==1)&&($3.type==1)) $$.type=1;
                else $$.type=2;
        $1.ctx=strdup($$.ctx);
        sprintf($$.ctx,"T%d",cptTemp); 
        creerQuad("-",$1.ctx,$3.ctx,$$.ctx);
        cptTemp++; 
        }
       |EXP_AR op_div EXP_AR
       {if($3.div_zero==9) Routine_division(f,nb_ligne, col);
        $$.type=2;
        $1.ctx=strdup($$.ctx);
        sprintf($$.ctx,"T%d",cptTemp);
        creerQuad("/",$1.ctx,$3.ctx,$$.ctx);
        cptTemp++; 
      }
       |EXP_AR op_etoile EXP_AR
       {if(($1.div_zero==9)||($3.div_zero==9)) $$.div_zero=9;
        else $$.div_zero=4;
        if(($1.type==1)&&($3.type==1)) $$.type=1;
                else $$.type=2; 
         
        $1.ctx=strdup($$.ctx);
        sprintf($$.ctx,"T%d",cptTemp);
        creerQuad("*",$1.ctx,$3.ctx,$$.ctx);
        cptTemp++; 
        }
       |TERME_AFFECTANT
        { $$.ctx=$1.gctx;
          if(IDF_NaPasDeValeur($1)!=1)
            {
              fl= strtof((char*)Routine_RecupererValeur($1),&end);
              if(fl==0)  $$.div_zero=9; else $$.div_zero=4;
            }
            if(Routine_RetournType($1)==1)
                        $$.type=1;
                      else 
                        if(Routine_RetournType($1)==0)
                          $$.type=2;
                      else 
                        printf("File \"%s\" , line %d , character %d : semantic error ( Incompatibilite de type ) \n",f,nb_ligne,col);
          }
       |ent_p
        { if($1==0) $$.div_zero=9; else $$.div_zero=4; $$.type=1; 
            int k=$1; itoa(k,kk,10); $$.ctx=strdup(kk);}
       |par_ouv ent_n par_ferm
        { if($2==0) $$.div_zero=9; else $$.div_zero=4; $$.type=1;
          int k=$2; itoa(k,kk,10); $$.ctx=strdup(kk);} 
       |reel_p
        { if($1==0) $$.div_zero=9; else $$.div_zero=4; $$.type=2; 
            sprintf(kk,"%f",$1); $$.ctx=strdup(kk);} 
       |par_ouv reel_n par_ferm
        { if($2==0) $$.div_zero=9; else $$.div_zero=4; $$.type=2;
            sprintf(kk,"%f",$2); $$.ctx=strdup(kk);} 
;
//**********************************************************************************************
  




PIED: chevron_ouv op_div mc_docpropgram chevron_ferm 
;





%%
main(int argc, char* argv[])
{    
qc=0; 
f=argv[0];
initialisation(); 
initialiserTab(position);
yyparse();
afficher();
AfficherQuad();

}
yywrap()
{}
yyerror(char* msg)
{
printf("File \"%s\" , line %d , character %d : %s \n",f,nb_ligne,col,msg);
}