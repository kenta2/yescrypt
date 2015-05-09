CFLAGS=-Wall -I../build/debianbuildhost/include/amd64
LDFLAGS=../build/debianbuildhost/lib/amd64/libnacl.a
x.hsalsa : hsalsa.c
	gcc -o $@ $(CFLAGS) $^
x.xsalsa : xsalsa.c
	gcc -o $@ $(CFLAGS) $^ $(LDFLAGS)

