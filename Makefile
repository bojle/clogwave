all:
	ninja -C build
	clang -g -fpass-plugin=build/libCLogWave.so alt.c
	./a.out 

