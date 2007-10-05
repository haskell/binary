all: compiled
     
interpreted:
	runhaskell QC.hs 1000

compiled:
	ghc --make -O QC.hs -o qc -no-recomp
	time ./qc 1000

bench:: Benchmark.hs MemBench.hs CBenchmark.o
	ghc --make -O Benchmark.hs -fasm CBenchmark.o -o bench -no-recomp
	time ./bench 50

bench-nb::
	ghc --make -O NewBenchmark.hs -fasm -o bench-nb
	time ./bench-nb 

CBenchmark.o: CBenchmark.c
	gcc -O -c $< -o $@

hugs:
	runhugs -98 QC.hs  


HeapUse: HeapUse.hs
	ghc --make -O $^ -fasm -o $@

heap: HeapUse
	./HeapUse +RTS -M10M -t/dev/stderr -RTS

clean:
	rm -f *.o *.hi qc bench bench-nb *~

.PHONY: clean bench bench-nb
