#include <stdio.h>
#include "chacha.c"
#include <stdlib.h>

int main(void){
  ECRYPT_ctx x;
  u8 output[64];
  int i;
  int iterations;
  u8 key[32];
  u8 iv[8]={3,1,4,1,5,9,2,6};
  int blockcount = 7;
  for(i=0;i<32;++i){
    key[i]=i+1;
  }
  ECRYPT_keysetup(&x,key,256,0); //ivbits is not used
  ECRYPT_ivsetup(&x,iv);
  for(i=0;i<blockcount;++i){
    x.input[12] = PLUSONE(x.input[12]);
  }
  for(i=0;i<16;i++){
    if(!(i%4))printf("\n");
    printf(" %08x",x.input[i]);
  }

  // C is too fast.
  for(iterations=0;iterations<1000;++iterations){
    salsa20_wordtobyte(output,x.input);
  }
  for(i=0;i<64;i++){
    if(!(i%16))printf("\n");
    if(!(i%4))printf(" ");
    printf(" %02x",output[i]);
  }
  printf("\n");
  for(i=0;i<16;i++)
    printf("0x%08x,",x.input[i]);
  printf("\n");
  return 0;
}

