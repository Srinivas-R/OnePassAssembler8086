%{
#include "functions.h"
#include<stdio.h>
#include<ctype.h>
extern FILE *yyin;
extern int locctr;
int locctr_before_instr;
int f=0,mod=0,s=0;
int lineno=1;
FILE *op, *instr;
SYMBOL *temp;
char *c,h1[14],hex[5],h[14];
int yylex();
int yyerror();
int err[20],front=0;
extern struct instrn t;
int yyerrstatus,errf=0;
SYMBOL *temp;
%}

%union {
  int num;
  char str[30];
}

%token LABEL STR ARITH LOG DTTF CTTF IO REG8 REG16 MEM IBYTE IWORD CL
%start S
%nonassoc CON
%nonassoc ','
%type <str>LABEL
%type <str>STR
%type <str>ARITH
%type <str>LOG
%type <str>DTTF
%type <str>CTTF
%type <str>IO
%type <num> REG8
%type <num>REG16
%type <num> MEM
%type <str>IBYTE
%type <str>IWORD
%type <num>CL
%token COLON E DB DW


%type <str> ARITHMETIC
%type <str> LOGICAL
%type <str>DATATRANS
%type <str> CTRLTRANS
%type <str> CONST
%type <str> IMMED
%type <str> REGMEM
%type <str> RM
%type <str> STEP
%type <str> NWORDS
%type <str> NBYTES
%type <num>REG
%type <num>MEMORY

%%
S: N S  
| {printf("\n Completed parsing"); }
;

N: VAR E	{tohex(locctr_before_instr,hex,4);printf("\t\t\t%4d\t\t%s\n",lineno++,hex); locctr_before_instr=locctr;}
| INSTR E	{tohex(locctr_before_instr,hex,4);printf("\t\t\t%4d\t\t%s\n",lineno++,hex); locctr_before_instr=locctr;}
| LABEL COLON INSTR E {
		tohex(locctr_before_instr,hex,4);printf("\t\t\t%4d\t\t%s\n",lineno++,hex);
		temp=search_symbol($1);
		if(temp == NULL)
		  insert_symbol($1,locctr_before_instr,0);
		else if(temp->addrs == -1)
			resolve_links(temp, locctr_before_instr);
		else
		 {insert_symbol($1,locctr_before_instr,temp->flag);}

		locctr_before_instr=locctr;
		}
| E 		{printf("\t\t%4d\n",lineno++);}
;

VAR: LABEL VAL  {
		temp=search_symbol($1);
		if(temp == NULL)
		{insert_symbol($1,locctr_before_instr,0);}
		else if(temp->addrs == -1)
		{resolve_links(temp, locctr_before_instr);}
		else {insert_symbol($1,locctr_before_instr,temp->flag);}
		}
;
NBYTES: IBYTE ',' NBYTES  {$1[2]='\0';strcat($1,$3);strcpy($$,$1); locctr+=1; }
	| IBYTE           {$1[2]='\0';strcpy($$,$1); locctr+=1; }
	;


NWORDS: IWORD ',' NWORDS  {$1[4]='\0';strcat($1,$3);strcpy($$,$1); locctr+=2;}
 	| IWORD         	{$1[4]='\0'; strcpy($$,$1);locctr+=2;}
	;


VAL:  DB NBYTES  {make_obj_code(locctr_before_instr, $2);}
   |  DW NWORDS	 {make_obj_code(locctr_before_instr, $2);}
   | DB STR     {make_obj_code(locctr_before_instr, $2); locctr += strlen($2)-2;}
   ;


INSTR: ARITHMETIC	{if(!errf) make_obj_code(locctr_before_instr, $1);
					   else printf("\tERROR\n");
		 			 mod=f=s=errf=0; }
| LOGICAL		{if(!errf) make_obj_code(locctr_before_instr, $1);
				 else printf("\tERROR\n");
		 		 mod=f=s=errf=0;}
| DATATRANS	{if(!errf) make_obj_code(locctr_before_instr, $1);
			 else printf("\tERROR\n");
		 	 mod=f=s=errf=0;}
| CTRLTRANS	{if(!errf) make_obj_code(locctr_before_instr, $1);
			 else printf("\tERROR\n");
		 	 mod=f=s=errf=0;}
;

STEP: '1' {strcpy($$,"02");}
	| CL  {strcpy($$,"00");}
   ;


ARITHMETIC: ARITH REGMEM     {get_code($1,h,0+(f==2));addhex(h,$2,$$);locctr+=2;}
	| ARITH IMMED		    {get_code($1,h,1);addhex(h,$2,$$);locctr+=2;}
	| ARITH CONST       {get_code($1,h,2);addhex(h,$2,$$);locctr+=2;}
	| ARITH RM			{get_code($1,h,0);addhex(h,$2,$$);locctr+=2;}
	|ARITH				{get_code($1,h,0);strcpy($$,h);locctr+=1;}
         ;   

LOGICAL:LOG REG ',' STEP  {getrm($2,0,3,h,1);addhex(h,$4,h);get_code($1,h1,0);addhex(h,h1,$$);locctr+=2;}
	  | LOG MEMORY ',' STEP	{getrm($2,0,mod,h,0);addhex(h,$4,h);get_code($1,h1,0);addhex(h,h1,$$);locctr+=2;}
	  | LOG REGMEM	{get_code($1,h,0+(f==2));addhex(h,$2,$$);locctr+=2;}
	  | LOG IMMED	{get_code($1,h,1);addhex(h,$2,$$);locctr+=2;}
        ;


DATATRANS: DTTF REGMEM   {get_code($1,h,0+(f==2));addhex(h,$2,$$);locctr+=2;}
	| DTTF IMMED		{get_code($1,h,1);addhex(h,$2,$$);locctr+=2;}
	| DTTF RM		{get_code($1,h,2);addhex(h,$2,$$);locctr+=2;}
        ;

CTRLTRANS: CTTF LABEL  {
			 get_code($1,h,0);
			 Symbol *temp=search_symbol($2);
			 if(temp==NULL)
		  	  {strcpy(h1,""); temp = insert_symbol($2,-1,0); addReference(temp); locctr+=3;}
			 else
			  { 
			  	if(abs(locctr_before_instr+4-temp->addrs) > 255) locctr+=3;
			  		else locctr+=2;
			  	
			  	tohex(locctr_before_instr-temp->addrs,h1,4); //find out the difference
			    if(h1[0] =='F' && h1[1]=='F')  //in case of forward jump, addrs= FFF... so the first 2F get added with opcode **
			   	{ h1[0]='0'; h1[1]='0'; }   //so make them 00 so that 0+*=*
			   }
			   addhex(h,h1,$$);  //concatenating the hex values here 
			}
        | CTTF CONST		{get_code($1,h,0);addhex(h,$2,$$);locctr+=1;}
        ;

REGMEM: REG ',' MEMORY  	{if(f%10==2)
			 {getrm($1+1,0,mod,$$,1);
			  if($3 != -1)
			  {
			  	tohex($3,h,4-f/5-s);
			  	strcat($$,h);
			  }
			  if((f/5==0)&&(($1<8&&s==0)||($1>=8&&s==2))) yyerror(2);}
			 else
			  {
			  getrm($1+1,((f%10==1)? 6:$3%8),mod,$$,1);
			   if(f/10==1 && $3 != -1) 
			    {tohex($3/8,h,4-s);strcat($$,h);}
			   }
			 mod=0;
 			}
| MEMORY ',' REG   {getrm($3,((f%10==1)? $1%8:6),mod,h,0);addhex(h,"02",$$); 
			 if(f/10==1 && $1 != -1) 
			  {tohex($1/8,h,4-s);strcat($$,h);}
			 mod=0;f=0;
			 }
| REG ',' REG {getrm($3,$1,3,h,0);strcpy($$,h);
			   if(f<=1&&(($1<8&&$3>=8)||($3<8&&$1>=8))) 
			   		yyerror(1);
			  }
| MEMORY ',' MEMORY {if(f!=2) yyerror(3);}
;

IMMED: RM ',' CONST {}
        ;

RM: REG	 %prec CON	{getrm($1,0,3,h,0);strcpy($$,h);}
| MEMORY	 %prec CON	{getrm($1,0,mod,h,0);
						 strcpy($$,h);
						 mod=0;}
;

MEMORY: CONST MEM	{$$=$2+(toint($1,strlen($1)-1)<<3);
						mod=(strlen($1)-1)/2;
						f=10;}
| MEM		{$$=$1;}
| '[' CONST ']'		{$$=toint($2,strlen($2)-1)<<3;
						  f=11;}
| LABEL			{
			Symbol *temp=search_symbol($1);
			 if(temp==NULL)
		  	  {temp = insert_symbol($1,-1,0); addReference(temp); $$=-1;locctr+=2;}
			else
			  {
			  	$$=locctr_before_instr-temp->addrs;
			  	if(abs(locctr_before_instr+4-temp->addrs) > 255) locctr+=2;
			  		else locctr+=1;
			  }

			 f=12;
			}
| ARITH	{yyerror(0);}
| LOG	{yyerror(0);}
| DTTF	{yyerror(0);}
| CTTF	{yyerror(0);}
;

CONST: IWORD	{char flipped[6]; flip_high_low_bytes($1,flipped);strcpy($$,flipped);s=0;locctr+=2;}
        | IBYTE     	{strcpy($$,$1);s=2;locctr+=1;}
        ;

REG: REG8	{$$=$1;}
| REG16		{$$=$1;}
;


%%

int yyerror(int errcode)
{
  if(!errf)
   err[front++]=errcode+(lineno<<3);
  errf=1;
  return 0;
}

void disperr()
{
  int i=0;
  if(front) printf("\n\n%d errors found!!",front);
  for(;i<front;i++)
   switch(err[i]%8)
   {
     case 0:printf("\nError %d:Invalid operand\n",err[i]/8);break;
     case 1:printf("\nError %d:Mismatch in size of operands\n",err[i]/8);break;
     case 2:printf("\nError %d:Label not defined\n",err[i]/8);break;
     case 3:printf("\nError %d:Atleast 1 operand must be a register\n",err[i]/8);break;
     default:printf("\nFatal Error %d:Unable to parse\n",err[i]/8);break;
   }
}

int main(int argc, char * argv[])
{
  int flag;
  initopcodes();
  locctr=0;
  locctr_before_instr = 0;
  yyin=fopen(argv[1],"r");

  printf("\n\nINSTRUCTION\t\tLineNo\t\tLOCCTR\n\n");

  yyparse();
  printf("\n\n");
  flag = print_symtab();
  if(flag == 0) yyerror(2);
  printf("\n");
  displaycodes();
  disperr();
  return 0;
}