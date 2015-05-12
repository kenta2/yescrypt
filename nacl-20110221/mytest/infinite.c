#include <stdio.h>

#include "crypto_core_hsalsa20.h"
#include "crypto_core_salsa20.h"

static const unsigned char sigma[16] = "expand 32-byte k";

void crypto_stream_salsa20_my(
  const unsigned char *n,
  const unsigned char *k
)
{
  unsigned char in[16];
  unsigned char block[64];
  int i;
  unsigned int u;

  for (i = 0;i < 8;++i) in[i] = n[i];
  for (i = 8;i < 16;++i) in[i] = 0;

  for(;;) {
    crypto_core_salsa20(block,in,k,sigma);

    u = 1;
    for (i = 8;i < 16;++i) {
      u += (unsigned int) in[i];
      in[i] = u;
      u >>= 8;
    }

    fwrite(block,1,64,stdout);
  }
}


void crypto_stream_my(
  const unsigned char *n,
  const unsigned char *k
)
{
  unsigned char subkey[32];
  crypto_core_hsalsa20(subkey,n,k,sigma);
  crypto_stream_salsa20_my(n + 16,subkey);
}

int main(void){
  unsigned char key[32];
  unsigned char nonce[24];

  int code;
  code=fread(key,1,32,stdin);
  if(code!=32){
    fprintf(stderr,"Not enough key.\n");
    return 1;
  }

  code=fread(nonce,1,24,stdin);
  if(code!=24){
    fprintf(stderr,"Not enough nonce.\n");
    return 1;
  }
  // does not check if too much data is supplied

  crypto_stream_my(nonce,key);
  return 0;
}

/*
Approximately 270000 years to consume all 2^64 blocks until the block
counter overflows.  (Or 270000 computers 1 year.)
*/
