#include "crypto_core_hsalsa20.h"
#include "crypto_stream_salsa20.h"
#include "crypto_stream.h"
#include <stdio.h>
static const unsigned char sigma[16] = "expand 32-byte k";

int main(void){

  const unsigned char n[192/8]={0};
  const unsigned char k[256/8]={0};
  int i,row;
  unsigned char subkey[32];
  crypto_core_hsalsa20(subkey,n,k,sigma);
  for(row=0;row<32;row+=4){
    for(i=row;i<row+4;++i){
      printf("%02x ",subkey[i]);
    }
    printf("\n");
  }

  return 0;
}
