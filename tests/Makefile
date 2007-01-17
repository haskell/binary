all: interpreted compiled bench
     
interpreted:
	runhaskell QC.hs 1000

compiled:
	ghc -prof -auto-all --make -O QC.hs -o qc -no-recomp
	time ./qc 1000

bench::
	ghc --make -O2 Benchmark.hs -o bench -no-recomp
	time ./bench

bench-nb::
	ghc --make -O2 -fliberate-case-threshold=100 NewBenchmark.hs -o bench-nb -no-recomp
	time ./bench-nb

hugs:
	runhugs -98 QC.hs  

clean:
	rm -f *.o *.hi qc bench bench-nb *~
