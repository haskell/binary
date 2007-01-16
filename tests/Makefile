all: interpreted compiled bench
     
interpreted:
	runhaskell QC.hs 1000

compiled:
	ghc -prof-auto-all --make -O QC.hs -o qc -no-recomp
	time ./qc 1000

bench::
	ghc --make -O Benchmark.hs -o benchmark -no-recomp
	time ./benchmark

bench-nb::
	ghc -fbang-patterns --make -O NewBenchmark.hs -o bench-nb -no-recomp
	time ./bench-nb

hugs:
	runhugs -98 QC.hs  

clean:
	rm -f *.o *.hi qc bench bench-nb *~
