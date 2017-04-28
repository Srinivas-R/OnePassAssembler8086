#include<stdio.h>
#include<string.h>
#include<stdlib.h>

int object_code_locctr[100];     
char object_codes[100][20];      //Max 100 instructions
int num_codes = 0;               //Number of object codes generated till now

typedef struct symbol SYMBOL;
typedef struct symbol Symbol;

struct symbol
{
  char label[30];
  int addrs;
  int flag;
  SYMBOL *next;

  //To handle forward references
  int numReferences;               //Number of times symbol has been referenced before declaration
  int ReferenceIndexes[100];       //Indexes of instructions (in instr table) which have used the symbol
  //int ReferencePositions[100];     //Position in the obj code where address needs to be placed
 }symtab[26];

void make_obj_code(int locctr, char obj_code[])
{
  //Add the given object code to the list of object codes generated
  object_code_locctr[num_codes] = locctr;
  strcpy(object_codes[num_codes],obj_code);
  num_codes++;
}

void addReference(Symbol *temp)
{
  //The current instruction (index num_codes) is referencing an undefined symbol NAME
  //Add it to the list of references in the NAME linked list
  temp->ReferenceIndexes[temp->numReferences] = num_codes;
  temp->numReferences++;
}

void tohex( unsigned int num,char hex[],int len)
{
  int i,j=len-1;
  while(j>=0)
  {
    i=num%16;
    num/=16;
    hex[j--]=((i<10)? ('0'+i):('A'+i-10));
  }
  hex[len]='\0';
}

void flip_high_low_bytes(char org[],char flipped[])
{
  flipped[2] = org[0];
  flipped[3] = org[1];
  flipped[0] = org[2];
  flipped[1] = org[3];
  flipped[4] = org[4];
}

void resolve_links(Symbol *temp, int addr)
{
  //The symbol NAME has finally been defined with addr, resolve all references 
  temp->addrs = addr;
  int i=0;
  char hex[5],flipped[5];
  tohex(addr,hex,4);
  flip_high_low_bytes(hex,flipped);
  for(i=0;i<temp->numReferences;i++)
  {
    strcat(object_codes[temp->ReferenceIndexes[i]],flipped);
  }
}



void convert(char str[],char tar[])
{
  int i=0;
  while(str[i]!='\0')
  { 
    tohex((int)str[i],tar+i*2,2);
    i++;
  }
}

void addhex(char hx1[],char hx2[],char sum[])
{ 
  int i=0,f1=1,f2=1;
  while(f1||f2)
  {
    if(hx1[i]=='\0') f1=0;
    if(hx2[i]=='\0') f2=0;
    sum[i]=f1*hx1[i]+f2*hx2[i]-(f1&&f2)*('0');
    if(sum[i]>'9'&&f1*hx1[i]<='9'&&f2*hx2[i]<='9') sum[i]+=7;
    i++;
  }
  //printf("\nSum = %s",sum);
  sum[i]='\0';
} 

int toint(char hex[],int size)
{
  int num=0,i;
  for(i=0;i<size;i++)
  { num+=((hex[i]>='0'&&hex[i]<='9')? (hex[i]-'0'):(hex[i]-'A'+10));
    if(i<size-1) num*=16;
  }
  return num;
}

void getrm(int reg,int rorm,int mod,char rm[], int d_bit)
{
  int v=0,f=reg/8;
  if(f) rorm-=8;
  if(reg>=8) reg-=8;
  v=(d_bit<<9)+(f<<8)+rorm+(reg<<3)+(mod<<6);
  tohex(v,rm,4);
}

struct instrn
{
  char opname[10],code[10];
  struct instrn *link;
}optab[150];

typedef struct instrn Instr;

void initopcodes()
{
  FILE *opcf;
  int c=0;
  char n[10]=" ",cur[10],ccode[10];
  Instr *temp,*cr; 
  opcf=fopen("optable.txt","r");  
  while(!feof(opcf))
  {
     fscanf(opcf," %s %s ",cur,ccode);
     optab[c].link=NULL;
     if(strcmp(n,cur)==0)
     {
	temp=((Instr *)malloc(sizeof(Instr)));
	cr=optab[c-1].link;
	if(cr!=NULL) {
         for(;cr->link!=NULL;cr=cr->link);
	 cr->link=temp;
        }
	else optab[c-1].link=temp;
	strcpy(temp->opname,cur);
	strcpy(temp->code,ccode);
     }
     else 
     { 
	strcpy(optab[c].opname,cur);
	strcpy(optab[c].code,ccode);
	strcpy(n,cur);
	c++;
     }
     //getc(opcf);     
  }
  fclose(opcf);
}

void get_code(char name[],char tar[],int d)
{
  int l=0,h=150,m=(l+h)/2,i;

  while(1)
  {
    int cmp=strcmp(name,optab[m].opname);
    if(l>h) break;
    if(cmp==0) break;
    else if(cmp>0) l=m+1;
    else h=m-1;
    m=(l+h)/2;
  }
  Instr *cur=optab[m].link;
  for(i=0;i<d-1;i++) cur=cur->link;
  if(d>0&&cur!=NULL) strcpy(tar,cur->code);
  else strcpy(tar,optab[m].code);
}

void displaycodes()
{
  int i=0;
  printf("\nOBJECT CODES : \n\n");
  for(;i<num_codes;i++) 
  {
    printf("%10s\n",object_codes[i]);
  }

}

SYMBOL * search_symbol(char NAME[])
{ 
 int i = tolower(NAME[0]) -'a';
 SYMBOL *ptr1 = symtab[i].next;
 while(ptr1!=NULL && strcmp(ptr1->label,NAME)!=0)
	ptr1= ptr1->next;
 return ptr1;
 }

Symbol* insert_symbol(char NAME[],int addrs,int flg)
{ 
 SYMBOL *ptr,*temp;
 ptr = (SYMBOL *)malloc(sizeof(SYMBOL));
 ptr->next = NULL;
 ptr->flag=0;
 int index = tolower(NAME[0]) -'a';
 /*check if the symbol already exists in the hash table or not 
  * if it is not present, just add the symbol
  * else increment the correspondin flag to represent the >1 point where the name is repeated */
 if((temp= search_symbol(NAME))==NULL)
 {         //symbol not found, ok
   strcpy(ptr->label,NAME);
   ptr->addrs = addrs;
   //ptr->flag=0;
   ptr->next = symtab[index].next;
   symtab[index].next = ptr;  
   
  if(flg==0)
    ptr->flag=0;
   else 
   ptr->flag=flg;
 }
 else
 {
   //we have a repeated symbol error,set the flag
    temp->flag=flg+1;
  }

  return ptr;
}
  
void write_symtab()
{
 char hex[4];  
 FILE *ptr;
  ptr =fopen("symtab.txt","w");
  if(ptr==NULL)
  	{
		printf("\n File cannot be created. Exiting....");
		exit(0);
    }
  SYMBOL *temp;
  int i=0;
  while(i<26)
  {
     temp = symtab[i].next;
	 	while(temp!=NULL)
		{   
			tohex(temp->addrs,hex,4);
			fprintf(ptr,"%s %s %d\n",temp->label,hex,temp->flag);
			temp =temp->next;
		}
	 i++;
 }
 fclose(ptr);
 printf("\nEXIT");

}
int print_symtab()
{
  int flag = 1;
  int i=0;
  char hexaddr[4];
  SYMBOL *node;
  printf("\nSymbol table for given program: \n");
  printf("\n     LabelName      ADDRESS");
  for(; i<26;i++)
  {
      node = symtab[i].next;
	  while(node)
	  {
      if(node->addrs == -1)
      { 
        printf("\n%12s\t<UNDEFINED>",node->label);
	      flag = 0;
      }
      else
      {
      tohex(node->addrs,hexaddr,4);
		  printf("\n%12s\t%10s",node->label,hexaddr);
		  }
      node =node->next;

	  }
   }
   return flag;
 }
int intfl(char c)
{
  switch(c)
  {
    case '1': return 1;
	case '0': return 0;
	case '2':return 2;
	case '3': return 3;
	case '4': return 4;
	case '5': return 5;
	case '6':return 6;
	case '7': return 7;
	case '8': return 8;
	case '9': return 9;
  }
 }
void read_symtab()
{ char buf[5],loc[30];
  int num;  FILE *ptr; char c[1];
  ptr= fopen("symtab.txt","r");
  if(ptr==NULL)
  {
   printf("\n Invalid file, exiting..");
   exit(0);
   }
   else
   {
    int i=0;int x;
	while(!feof(ptr))
  {
    fscanf(ptr,"%s",buf);
    fscanf(ptr,"%s" ,loc);
    fscanf(ptr," %d ",&x);
 	//printf("\nAddrs  =%s",loc);
	insert_symbol(buf,toint(loc,4),x);//intfl(c[0]));
    
   // printf("\n %d",intfl(c[0]));
	i++; }
   }
  }
