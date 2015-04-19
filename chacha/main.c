#include <stdio.h>
#include "chacha.c"
#include <stdlib.h>

int main(void){
  u32 input[16];
  u8 output[64];
  int i;
  int iterations;
  for(i=0;i<16;i++){
    u32 x;
    x=random();
    x<<=16;
    x|=random()&0xffff;
    if(!(i%4))printf("\n");
    printf(" %08x",x);
    input[i]=x;
  }
  // C is too fast.
  for(iterations=0;iterations<1000;++iterations){
    salsa20_wordtobyte(output,input);
  }
  for(i=0;i<64;i++){
    if(!(i%16))printf("\n");
    if(!(i%4))printf(" ");
    printf(" %02x",output[i]);
  }
  printf("\n");
  for(i=0;i<16;i++)
    printf("0x%08x,",input[i]);
  printf("\n");
  return 0;
}

