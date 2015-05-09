#include <stdio.h>

#include "crypto_core_hsalsa20.h"
#include "crypto_core_salsa20.h"

static const unsigned char sigma[16] = "expand 32-byte k";

int crypto_stream_salsa20_my(
        unsigned char *c,unsigned long long clen,
  const unsigned char *n,
  const unsigned char *k
)
{
  unsigned char in[16];
  unsigned char block[64];
  int i;
  unsigned int u;

  if (!clen) return 0;

  for (i = 0;i < 8;++i) in[i] = n[i];
  for (i = 8;i < 16;++i) in[i] = 0;

  while (clen >= 64) {
    crypto_core_salsa20(c,in,k,sigma);

    u = 1;
    for (i = 8;i < 16;++i) {
      u += (unsigned int) in[i];
      in[i] = u;
      u >>= 8;
    }

    clen -= 64;
    c += 64;
  }

  if (clen) {
    crypto_core_salsa20(block,in,k,sigma);
    for (i = 0;i < clen;++i) c[i] = block[i];
  }
  return 0;
}


int crypto_stream_my(
        unsigned char *c,unsigned long long clen,
  const unsigned char *n,
  const unsigned char *k
)
{
  unsigned char subkey[32];
  crypto_core_hsalsa20(subkey,n,k,sigma);
  return crypto_stream_salsa20_my(c,clen,n + 16,subkey);
}

int main(void){
#define SZ 128
  unsigned char buf[SZ];
  unsigned char nonce[24]={99};
  unsigned char key[32]={1};
  int code;
  code=crypto_stream_my(buf,SZ,nonce,key);
  if(code!=0)return 1;
  for(code=0;code<SZ;++code){
    printf("%02x ",buf[code]);
  }
  printf("\n");
  return 0;
}
