#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <gmp.h>
#include <time.h>
#include "scaffold4.h"

/* You are suppose to change the routine Product4 here to your own routine 
 * The mpz calls in the scaffolded Product4 below are the normal GMP function
 * calls and should be neglected. By casting the void pointers as normal unsigned
 * integers, you should be able to access the data values as normal 4 bytes words.
 */
/**************************************************************************************************
Function Name:- multiply
Input Parameters:- void *, void *, void *, unisgned int, unsigned int, unsigned int, unisgned int, 
		unsigned int *, unsigned int *
Return Type:- void
Description:- This function performs multiplication of the two numbers using 
		Traditional algorithm 1.8. This function multiplies each word of the two numbers and uses 
		bit extraction approach to calculate the result in required base. 
***************************************************************************************************/
void multiply(void *a, void *b, void *c, unsigned int sa,
    unsigned int bitsa, unsigned int sb, unsigned int bitsb, unsigned
        int *sc, unsigned int *bitsc){

    	unsigned int *cint_a = (unsigned int *) a;
    	unsigned int *cint_b = (unsigned int *) b;
    	unsigned int *cint_c = (unsigned int *) c;
	int i=0,j=0;
	unsigned long long carry=0,p=0;
    	*sc=sa+sb;
	
    	for(i=0;i<(sa+sb);i++){
		cint_c[i]=0;
    	}
	
	unsigned long long base=4294967296;
    	for(i=0;i<sa;i++){
		carry=0;
		for(j=0;j<sb;j++){
			p=(unsigned long long) cint_a[i] * cint_b[j] + cint_c[i+j] + carry;
			carry=(p>>32);
			cint_c[i+j]=p-(carry<<32);
		}
		cint_c[i+j]=carry;
    	}
    	if(carry==0 && *sc>0){
		(*sc)=(*sc)-1;	
	}
}

/**************************************************************************************************
Function Name:- append
Input Parameters:-  unsigned int *, unsigned int *,unsigned int *, unsigned int *
Return Type:- void
Description:- This function appends zero to the smaller of the two numbers to equal the word size of 
		the two numbers.
***************************************************************************************************/

void append(unsigned int *sa, unsigned int *sb, unsigned int *len_num1, unsigned int *len_num2){
	
	int i=0;
	*len_num1=*sa;
	*len_num2=*sb;
	if((*sa)==0){
		*len_num1=2;
	}
	if((*sb)==0){
		*len_num2=2;
	}
	if((*sa)<(*sb)){
		*len_num1=*sa;
		*len_num2=*sb;
		if((*sb)%2!=0){
			*len_num2+=1;	
		}
		for(i=(*sa);i<(*len_num2);i++){
			*len_num1+=1;
		}
	}
	else if((*sa)>(*sb)){
		*len_num1=*sa;
		*len_num2=*sb;
		if((*sa)%2!=0){
			*len_num1+=1;
		}
		for(i=(*sb);i<(*len_num1);i++){
			*len_num2+=1;
		}
	}
	else if((*sa)==(*sb) && ((*sa)%2!=0)){
		*len_num1+=1;
		*len_num2+=1;
	}	
}
/**************************************************************************************************
Function Name:- sum
Input Parameters:-  void *, void *, void *, unisgned *int, unsigned *int, unsigned *int 
Return Type:- void
Description:- This function performs addition of the words of two numbers. This function uses bit 
		extraction approach to calculate the result in required base.
***************************************************************************************************/

void sum(void *a, void *b, void *ssum, unsigned int *sa, unsigned int *sb, unsigned int *sc ){
	unsigned int *cint_a = (unsigned int *)a;
	unsigned int *cint_b = (unsigned int *)b;
	unsigned int val = (*sa>*sb)?*sa:*sb;
	unsigned long long p = 0,carry=0;
	unsigned int *sum = (unsigned int *)ssum;
	int iCounter=0,i=0;
	unsigned long long base=4294967296;
    	
	for(iCounter=0; iCounter<val; iCounter++){
		p = (unsigned long long)cint_a[iCounter] + cint_b[iCounter] + carry;
		carry=(p>>32);
		sum[iCounter]=p-(carry<<32);
	}
	*sc=iCounter;
	if(carry!=0){
		sum[iCounter]=carry;
		*sc=iCounter+1;
	}
}
/**************************************************************************************************
Function Name:- sub
Input Parameters:-  void *, void *, void *, unisgned int *, unsigned int *, unsigned int *
Return Type:- void
Description:- This function performs subtraction of the words of two numbers. 
***************************************************************************************************/

void sub(void *a, void *b, void *ssub, unsigned int *sa, unsigned int *sb, unsigned int *sc){
	
	 unsigned int *cint_a = (unsigned int *) a;
        unsigned int *cint_b = (unsigned int *) b;
        unsigned int *sub = (unsigned int *) ssub;
	 int i=0,j=0;
	 unsigned int val = (*sa>*sb)?*sa:*sb;
	 unsigned long long base=4294967296;
    	
	 for(i=0;i<val;i++){
	 	if(cint_a[i]>=cint_b[i]){
	 		sub[i] = cint_a[i]-cint_b[i];
	 	}
		else{
			if(cint_a[i+1]!=0){
				cint_a[i+1]=cint_a[i+1]-1;
				sub[i]=(base+cint_a[i])-cint_b[i];
			}
			else{
				sub[i]=base+cint_a[i]-cint_b[i];
				for(j=i+1;j<val;j++){
					if(cint_a[j]==0){
						cint_a[j]=cint_a[j]+base-1;
					}
					else{
						cint_a[j]=cint_a[j]-1;
						break;
					}
				}	
			}
		}
	}
	*sc = val; 
	if(*sc>1 && sub[*sc-1]==0){
		(*sc)=(*sc)-1;
	}
}
/**************************************************************************************************
Function Name:- division
Input Parameters:-  void *, void *, void *, void *,unisgned int *, unsigned int *, unsigned int *,  
		unsigned int *, int 
Return Type:- void
Description:- This function divides the number by the numberofwords to find remainder and quotient. 
		The quotient computed in the previous result is added to the number to compute the number.
		The division is performed on this number to convert it into the required base. The quotient 
		and remainder is computed using the function memcpy.
***************************************************************************************************/

void division(void *a, void *b, void *c, void *d, unsigned int *sa, unsigned int *sb, unsigned int *sc, unsigned int *sd, int numberofwords){
	unsigned int *cint_a = (unsigned int *) a;
       unsigned int *cint_b = (unsigned int *) b;
	unsigned int *cint_c = (unsigned int *) c;
	unsigned int *cint_d = (unsigned int *) d;
	int i=0,r=0,q=0,count=0;
	r=numberofwords;
	q=(*sa)- numberofwords;
	
	sum(cint_a, cint_d, cint_a, sa, sd, sa);
	for(i=*sa-1;i>=0;i--){
		if(cint_a[i]==0){
			count++;
		}
		else{
			break;
		}
    	}
	r=r<(*sa-count)?r:(*sa-count);
	if(q==0 && cint_a[r]!=0){
		q=1;
	}
	if(r>0){

		memcpy(cint_b,cint_a,r*4);
	}

	if(count<q){
		q=*sa-r-count;
		memcpy(cint_c,&cint_a[r],q*4);
	}
	else{
		q=0;
	}
	*sb=r;
	*sc=q;
}
/**************************************************************************************************
Function Name:- Product32
Input Parameters:-  void *, void *, void *, void *,unisgned int , unsigned int , unsigned int ,  
		unsigned int , unsigned int *, unsigned int * 
Return Type:- void
Description:- This function implements the algorithm 5.2. It calls recusively till the word size of any number 
		is less then 35. It calls the traditional 1.8 algorithm when the word size of any number is less 
		then 35.
***************************************************************************************************/


void Product32(void *a, void *b, void *c, unsigned int sa, unsigned int bitsa, unsigned int sb, unsigned int bitsb, unsigned int *sc, unsigned int *bitsc){
	unsigned int *cinta = (unsigned int *) a;
       unsigned int *cintb = (unsigned int *) b;
       unsigned int *cint_c = (unsigned int *) c;
	int i=0,j=0,carry=0,p=0,k=0;
	unsigned int lengthnum1=0,lengthnum2=0;
      	if(sa<35 || sb<35){
		multiply(cinta, cintb, cint_c, sa, bitsa, sb, bitsb, sc, bitsc);
		return;
	}
	append(&sa, &sb, &lengthnum1, &lengthnum2);
	unsigned int *cint_a = (unsigned int *)calloc(lengthnum1,sizeof(int));
	unsigned int *cint_b = (unsigned int *)calloc(lengthnum2,sizeof(int));
	
	if(sa!=0){
		memcpy(cint_a,cinta,sa*4);
	}
	memset(&cint_a[sa],0,(lengthnum1-sa)*4);
	if(sb!=0){
		memcpy(cint_b,cintb,sb*4);
	}
	memset(&cint_b[sb],0,(lengthnum2-sb)*4);
	

	sa=lengthnum1;
	sb=lengthnum2;
	int numberofwords=sa/2;
       unsigned int *cint_mida = (unsigned int *)calloc(sa,sizeof(int));
       memcpy(cint_mida,&cint_a[sa/2],sa/2*4);
	unsigned int *cint_midb = (unsigned int *)calloc(sb,sizeof(int));
	memcpy(cint_midb,&cint_b[sb/2],sb/2*4);
	int x = sa/2;
	int y = sb/2;
	int length_cint_w4=0, length_cint_w3=0, length_cint_w2=0, length_cint_w1=0, length_remw4=0, length_remw3=0, length_remw2=0, length_remw1=0, length_quotientw4=0;
	int length_quotientw3=0, length_quotientw2=0, length_quotientw1=0, length_temp=0, length_num1_suma=0, length_num2_sumb=0;
	
	*sc=(sa+sb);
	
	unsigned int* num1_suma = (unsigned int *)calloc((sa+sb),sizeof(int));
	unsigned int* num2_sumb = (unsigned int *)calloc((sa+sb),sizeof(int));
	unsigned int* cint_w1 =(unsigned int *)calloc((sa+sb),sizeof(int));
	unsigned int* cint_w2 = (unsigned int *)calloc((sa+sb),sizeof(int));
	unsigned int* cint_w3 = (unsigned int *)calloc((sa+sb),sizeof(int));
	unsigned int* cint_w4 = (unsigned int*)calloc((sa+sb),sizeof(int));	
	unsigned int* c_carry = (unsigned int *)calloc((sa+sb),sizeof(int));
	unsigned int* remw4= (unsigned int *)calloc((sa+sb),sizeof(int));
	unsigned int* remw3= (unsigned int *)calloc((sa+sb),sizeof(int));
	unsigned int* remw2= (unsigned int*)calloc((sa+sb),sizeof(int));
	unsigned int* remw1= (unsigned int *)calloc((sa+sb),sizeof(int));
	unsigned int* temp= (unsigned int *)calloc((sa+sb),sizeof(int));
	unsigned int* quotientw4= (unsigned int *)calloc((sa+sb),sizeof(int));
	unsigned int* quotientw3= (unsigned int*)calloc((sa+sb),sizeof(int));
	unsigned int* quotientw2= (unsigned int *)calloc((sa+sb),sizeof(int));
	unsigned int* quotientw1= (unsigned int *)calloc((sa+sb),sizeof(int));
	
	sum(cint_a, cint_mida,  num1_suma, &x, &x, &length_num1_suma);
	sum(cint_b, cint_midb, num2_sumb, &y, &y, &length_num2_sumb);
	Product32(num1_suma, num2_sumb,cint_w3,length_num1_suma, bitsa, length_num2_sumb, bitsb, &length_cint_w3, bitsc);
	Product32(cint_a, cint_b, cint_w4, x, bitsa, y, bitsb, &length_cint_w4, bitsc);
	Product32(cint_mida, cint_midb, cint_w2, x, bitsa, y, bitsb, &length_cint_w2, bitsc);
	sub(cint_w3, cint_w2, cint_w3, &length_cint_w3, &length_cint_w2, &length_cint_w3);
	sub(cint_w3, cint_w4, cint_w3, &length_cint_w3, &length_cint_w4, &length_cint_w3);
	division(cint_w4,remw4,quotientw4,temp,&length_cint_w4,&length_remw4,&length_quotientw4, &length_temp, numberofwords);
	division(cint_w3,remw3,quotientw3,quotientw4,&length_cint_w3,&length_remw3,&length_quotientw3, &length_quotientw4, numberofwords);
	division(cint_w2,remw2,quotientw2,quotientw3,&length_cint_w2,&length_remw2,&length_quotientw2, &length_quotientw3, numberofwords);
	
	quotientw1=quotientw2;
	
	*sc = length_remw4+length_remw3+length_remw2+length_quotientw2;
	if(length_remw4>0){
		memcpy(cint_c,remw4,length_remw4*4);
	}
	if(length_remw3>0){
		memcpy(&cint_c[length_remw4],remw3,length_remw3*4);
	}
	if(length_remw2>0){
		memcpy(&cint_c[length_remw4+length_remw3],remw2,length_remw2*4);
	}
	if(length_quotientw2>0){
		memcpy(&cint_c[length_remw4+length_remw3+length_remw2],quotientw1,length_quotientw2*4);
	}
	
	return;
}
