programs := builder bench

.PHONY: all
all: $(programs)

builder: Builder.hs
	ghc --make -O2 Builder.hs -o builder -fforce-recomp -i../src

bench: Benchmark.hs MemBench.hs CBenchmark.o
	ghc --make -O2 -fliberate-case-threshold=1000 -fasm Benchmark.hs CBenchmark.o -o bench -fforce-recomp -i../src
	./bench 100

CBenchmark.o: CBenchmark.c
	ghc -c -optc -O3 $< -o $@

HeapUse: HeapUse.hs
	ghc --make -O $^ -fasm -o $@ -i../src

heap: HeapUse
	./HeapUse +RTS -M10M -t/dev/stderr -RTS

.PHONY: clean
clean:
	rm -f *.o *.hi $(programs)
