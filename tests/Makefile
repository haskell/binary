all: interpreted compiled bench
     
interpreted:
	runhaskell QC.hs 1000

compiled:
	ghc --make -O QC.hs -o qc -no-recomp
	time ./qc 1000

bench::
	ghc --make -O Benchmark.hs -fasm -o bench -no-recomp
	time ./bench

bench-nb::
	ghc --make -O NewBenchmark.hs -fasm -o bench-nb
	time ./bench-nb

hugs:
	runhugs -98 QC.hs  

clean:
	rm -f *.o *.hi qc bench bench-nb *~
