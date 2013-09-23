1、
gcc -o complex_nif.so -fpic -shared complex.c complex_nif.c

2、
flybird@flybird ~/ErlangStudy/erlang_call_c $ erl
Eshell V5.9.1  (abort with ^G)
1> c(complex).
{ok,complex}
2> complex:bar(4).
8
