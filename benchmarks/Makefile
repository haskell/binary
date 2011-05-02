ghc := ghc
ghc-flags :=
programs := builder bench

.PHONY: all
all: $(programs)

builder: Builder.hs
	$(ghc) $(ghc-flags) --make -O2 Builder.hs -o $@ -fforce-recomp -i../src

bench: Benchmark.hs MemBench.hs CBenchmark.o
	$(ghc) $(ghc-flags) --make -O2 -fliberate-case-threshold=1000 -fasm Benchmark.hs CBenchmark.o -o $@ -fforce-recomp -i../src

.PHONY: run-bench
run-bench: bench
	./bench 100

CBenchmark.o: CBenchmark.c
	$(ghc) -c -optc -O3 $< -o $@

.PHONY: clean
clean:
	rm -f *.o *.hi $(programs)
