all:
	ninja -C build
	clang -g -emit-llvm -S -O0 alt.c -o alt.ll
	opt -load-pass-plugin build/libCLogWave.so -passes="CLogWave" alt.ll -o alt.bin
	lli alt.bin	
direct:
	ninja -C build
	clang -g -fpass-plugin=build/libCLogWave.so alt.c
	./a.out 
