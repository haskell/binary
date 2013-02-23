ghc := ghc
ghc-flags :=
programs := builder bench

SYSTEM_BINARY := binary-0.5.1.1

.PHONY: all
all: $(programs)

builder: Builder.hs
	$(ghc) $(ghc-flags) --make -O2 Builder.hs -o $@ -fforce-recomp -i../src

get: Get.hs
	$(ghc) $(ghc-flags) --make -O2 Get.hs -o $@ -fforce-recomp -i../src

system-get: Get.hs
	$(ghc) $(ghc-flags) --make -O2 Get.hs -o $@ -package $(SYSTEM_BINARY)

bench: Benchmark.hs MemBench.hs CBenchmark.o
	$(ghc) $(ghc-flags) --make -O2 -fliberate-case-threshold=1000 Benchmark.hs CBenchmark.o -o $@ -fforce-recomp -i../src

system-bench: Benchmark.hs MemBench.hs CBenchmark.o
	$(ghc) $(ghc-flags) --make -O2 -fliberate-case-threshold=1000 Benchmark.hs CBenchmark.o -o $@ -no-user-package-conf

.PHONY: run-bench
run-bench: bench
	./bench 100

CBenchmark.o: CBenchmark.c
	$(ghc) -c -optc -O3 $< -o $@

.PHONY: clean
clean:
	rm -f *.o *.hi $(programs)
