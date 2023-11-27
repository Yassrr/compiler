%{
 #include <stdio.h>
 #include "PartieSyntax.tab.h"
 #include "Fonctions.h"
 extern nb_ligne;
 extern col;
 extern YYSTYPE yylval;
 extern f;
 int cpt1=0,cpt2=0,i=0,bol=0,bol2=0;
 char t; 
%}
chiffre [0-9]
lettre [a-zA-Z]
character  \'.\'
chaine  "\""([^"]+|("\n")*)"\""
IDF {lettre}({lettre}|{chiffre}|_)*
rl_p  ({chiffre}+\.{chiffre}+) 
rl_n  (-{chiffre}+\.{chiffre}+)
entier_p    {chiffre}+
entier_n    [-]{chiffre}+
bool (TRUE|FALSE)
%%
"<"  {rechercher(yytext,"SEP",0,0,2); col = col + strlen(yytext); return chevron_ouv;}
    ">"  {rechercher(yytext,"SEP",0,0,2); col = col + strlen(yytext); return chevron_ferm; }
    "!" {rechercher(yytext,"SEP",0,0,2); col = col + strlen(yytext); return point_excl; }
    "docprogram "  {rechercher(yytext,"Mot cle"," ",0,1); col = col + strlen(yytext); return mc_docpropgram; }
    ";" {rechercher(yytext,"SEP",0,0,2); col = col + strlen(yytext); return sep_pvg; }
    "SUB" { rechercher(yytext,"Mot cle"," ",0,1); col = col + strlen(yytext); return mc_sub; }
    "VARIABLE" { rechercher(yytext,"Mot cle"," ",0,1); col = col + strlen(yytext); return mc_var; }
    "body"  { rechercher(yytext,"Mot cle"," ",0,1); col = col + strlen(yytext); return mc_body; }
    "AS" { rechercher(yytext,"Mot cle"," ",0,1); col = col + strlen(yytext); return mc_as; }
    "|" {rechercher(yytext,"SEP",0,0,2); col = col + strlen(yytext); return sep_barre; }
    "ARRAY" { rechercher(yytext,"Mot cle"," ",0,1); col = col + strlen(yytext); return mc_array; }
    "INT" { rechercher(yytext,"Mot cle"," ",0,1); col = col + strlen(yytext); return mc_int; }
    "CHR" {rechercher(yytext,"Mot cle"," ",0,1); col = col + strlen(yytext); return mc_chr; }
    "STR" { rechercher(yytext,"Mot cle"," ",0,1); col = col + strlen(yytext); return mc_str; }
    "FLT" { rechercher(yytext,"Mot cle"," ",0,1); col = col + strlen(yytext); return mc_flt; }
    "BOL" { rechercher(yytext,"Mot cle"," ",0,1); col = col + strlen(yytext); return mc_bol; }
    "CONSTANTE" {rechercher(yytext,"Mot cle"," ",0,1); col = col + strlen(yytext); return mc_cons; }
    "=" {rechercher(yytext,"OPERATEUR",0,0,2); col = col + strlen(yytext); return egal; }
    "+" {rechercher(yytext,"OPERATEUR",0,0,2); col = col + strlen(yytext); return op_plus; }
    "-" {rechercher(yytext,"OPERATEUR",0,0,2); col = col + strlen(yytext); return op_moins; }
    "*" {rechercher(yytext,"OPERATEUR",0,0,2); col = col + strlen(yytext); return op_etoile; }
    "/" {rechercher(yytext,"OPERATEUR",0,0,2); col = col + strlen(yytext); return op_div; }
    "AND" {rechercher(yytext,"Mot cle"," ",0,1);  col = col + strlen(yytext); return op_and; }
    "OR"  {rechercher(yytext,"Mot cle"," ",0,1); col = col + strlen(yytext); return op_or; }
    "NOT" { rechercher(yytext,"Mot cle"," ",0,1); col = col + strlen(yytext); return op_not; }
    "," {col = col + strlen(yytext); return  sep_vergule; }
    "SUP" {rechercher(yytext,"Mot cle"," ",0,1);  col = col + strlen(yytext); return op_sup; }
    "INF" {rechercher(yytext,"Mot cle"," ",0,1);  col = col + strlen(yytext); return op_inf; }
    "SUPE" { rechercher(yytext,"Mot cle"," ",0,1);  col = col + strlen(yytext); return op_supe; }
    "INFE" { rechercher(yytext,"Mot cle"," ",0,1);  col = col + strlen(yytext); return op_infe; }
    "EGA"  {rechercher(yytext,"Mot cle"," ",0,1);  col = col + strlen(yytext); return op_ega; }
    "DIF"  { rechercher(yytext,"Mot cle"," ",0,1);  col = col + strlen(yytext); return op_dif; }
    ":"  {rechercher(yytext,"SEP",0,0,2); col = col + strlen(yytext); return deux_points; }
    "[" {rechercher(yytext,"SEP",0,0,2); col = col + strlen(yytext); return crochet_ouv; }
    "]" {rechercher(yytext,"SEP",0,0,2); col = col + strlen(yytext); return crochet_ferm; }
    "(" {rechercher(yytext,"SEP",0,0,2); col = col + strlen(yytext); return par_ouv; }
    ")" {rechercher(yytext,"SEP",0,0,2); col = col + strlen(yytext); return par_ferm; }
    "INPUT" {rechercher(yytext,"Mot cle"," ",0,1); col = col + strlen(yytext); return mc_input; }
    "OUTPUT" {rechercher(yytext,"Mot cle"," ",0,1); col = col + strlen(yytext); return mc_output; }
    "$" {rechercher(yytext,"SEP",0,0,2); col = col + strlen(yytext); return dollars; }
    "%" {rechercher(yytext,"SEP",0,0,2); col = col + strlen(yytext); return pourcentage; }
    "#" {rechercher(yytext,"SEP",0,0,2); col = col + strlen(yytext); return diaz; }
    "&" {rechercher(yytext,"SEP",0,0,2); col = col + strlen(yytext); return  et_commer; }
    "@" {rechercher(yytext,"SEP",0,0,2); col = col + strlen(yytext); return arobase; }
    "IF" { rechercher(yytext,"Mot cle"," ",0,1); col = col + strlen(yytext); return mc_if; }
    "THEN" { rechercher(yytext,"Mot cle"," ",0,1); col = col + strlen(yytext); return mc_then; }
    "ELSE" { rechercher(yytext,"Mot cle"," ",0,1); col = col + strlen(yytext); return mc_else; }
    "DO" { rechercher(yytext,"Mot cle"," ",0,1); col = col + strlen(yytext); return mc_do; }
    "WHILE" { rechercher(yytext,"Mot cle"," ",0,1); col = col + strlen(yytext); return mc_while; }
    "FOR" {rechercher(yytext,"Mot cle"," ",0,1); col = col + strlen(yytext); return mc_for; }
    "UNTIL" { rechercher(yytext,"Mot cle"," ",0,1); col = col + strlen(yytext); return mc_until; }
    "AFF" { rechercher(yytext,"Mot cle"," ",0,1); col = col + strlen(yytext); return mc_aff; }
    {bool} {if(strcmp(strdup(yytext),"TRUE")==0)  yylval.bol=1; else yylval.bol=0; rechercher(yytext,"cst","BOL",0,0); col = col + strlen(yytext); return booleen; }
    {IDF}  {yylval.string=strdup(yytext); rechercher(yytext,"IDF"," ",0,0); col = col + strlen(yytext); return idf; }
    [ \t] {col = col + strlen(yytext);}
    \n {col =1; nb_ligne++;}

    {character} {yylval.string=strdup(yytext); rechercher(yytext,"cst","CHAR",0,0); col=col+strlen(yytext); return caracter;}
    {chaine} {yylval.string=strdup(yytext);rechercher(yytext,"cst","STRING",0,0); col=col+strlen(yytext); return chaine_car;}	



    {rl_p} { 
    if(yyleng<=11)
    {  
      while((i<yyleng)&&(bol==0)) 
      { 
        t=yytext[i]; cpt1++; 
        if(t=='.')
        { 
        bol2=1;
        if((cpt1-1)>8)
        {bol=1; printf("File \"%s\" , line %d , character %d : lexical error (la partie entiere de reel depase 7 chiffres)  \n",f,nb_ligne,col);}}
        if(bol2==1) 
        {
        cpt2++;
        if((cpt2-1)>4)
        {bol=1; printf("File \"%s\" , line %d , character %d : lexical error (la partie apres vergule de reel depase 3 chiffres)  \n",f,nb_ligne,col);}
        }
        i++;
      }
      if(bol==0) {yylval.reel=atof(yytext); rechercher(yytext,"cst","REAL",atof(yytext),0); col=col+strlen(yytext); return reel_p;}
    }
    else { printf("File \"%s\" , line %d , character %d : lexical error (le reel est trop long)  \n",f,nb_ligne,col); }
		}
    


    {rl_n} { cpt1=0;cpt2=0;i=0;bol=0;bol2=0;
    if(yyleng<=12)
    {  
      while((i<yyleng)&&(bol==0)) 
      { 
        t=yytext[i]; cpt1++; 
        if(t=='.')
        { 
        bol2=1;
        if((cpt1-1)>8)
        {bol=1; printf("File \"%s\" , line %d , character %d : lexical error (la partie entiere de reel depase 7 chiffres)  \n",f,nb_ligne,col);}}
        if(bol2==1) 
        {
        cpt2++;
        if((cpt2-1)>4)
        {bol=1; printf("File \"%s\" , line %d , character %d : lexical error (la partie apres vergule de reel depase 3 chiffres)  \n",f,nb_ligne,col);}
        }
        i++;
      }
      if(bol==0) {rechercher(yytext,"cst","REAL",atof(yytext),0); yylval.reel=atof(yytext); col=col+strlen(yytext); return reel_n;}
    }
    else { printf("File \"%s\" , line %d , character %d : lexical error (le reel est trop long)  \n",f,nb_ligne,col); }
		}



    {entier_n} { 
    if(((atoi(yytext))<=0) && ((atoi(yytext))>=-32768)) {
      yylval.entier=atoi(yytext);  rechercher(yytext,"cst","INTEGER",atoi(yytext),0); col=col+strlen(yytext); return ent_n;}
      else {col=col+strlen(yytext);
     printf("File \"%s\" , line %d , character %d : lexical error(la valeur entiere a depasse l'intervalle defini)  \n",f,nb_ligne,col);} 
	   }		



    {entier_p} {
    if(((atoi(yytext))<= 32767) && ((atoi(yytext))>=0)) {
       yylval.entier=atoi(yytext); rechercher(yytext,"cst","INTEGER",atoi(yytext),0); col=col+strlen(yytext); return ent_p;}
      else {col=col+strlen(yytext); 
	  printf("File \"%s\" , line %d , character %d : lexical error(la valeur entiere a depasse l'intervalle defini)  \n",f,nb_ligne,col);}
	  }	



    .   printf("File \"%s\" , line %d , character %d : lexical error  \n",f,nb_ligne,col);
%%