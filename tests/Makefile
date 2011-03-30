all: compiled

interpreted:
	runhaskell QC.hs 1000

compiled:
	ghc --make -fhpc -O QC.hs -o qc -no-recomp -threaded -rtsopts -i../src -XCPP -package test-framework -package test-framework-quickcheck2
	rm -f qc.tix
	./qc --maximum-generated-tests=1000 -j2 +RTS -N2

inplace-bench:: Benchmark.hs MemBench.hs CBenchmark.o
	ghc --make -O2 -fliberate-case-threshold=1000 -fasm -i../src Benchmark.hs CBenchmark.o -o inplace-bench -fforce-recomp
	./inplace-bench 100

bench:: Benchmark.hs MemBench.hs CBenchmark.o
	ghc --make -O2 -fliberate-case-threshold=1000 -fasm Benchmark.hs CBenchmark.o -o bench -fforce-recomp
	./bench 100

bench-nb::
	ghc --make -O2 -fliberate-case-threshold=1000 NewBenchmark.hs -fasm -o bench-nb
	./bench-nb 

CBenchmark.o: CBenchmark.c
	ghc -c -optc -O3 $< -o $@

hugs:
	runhugs -98 QC.hs  


HeapUse: HeapUse.hs
	ghc --make -O $^ -fasm -o $@

heap: HeapUse
	./HeapUse +RTS -M10M -t/dev/stderr -RTS

clean:
	rm -f *.o *.hi qc bench bench-nb *~

.PHONY: clean inplace-bench bench bench-nb
