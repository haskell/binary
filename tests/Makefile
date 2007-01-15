all: interpreted compiled
     
interpreted:
	runhaskell QC.hs 1000

compiled: bench
	ghc -prof -auto-all --make -O QC.hs -o qc -no-recomp
	time ./qc 1000

bench:
	ghc -fbang-patterns --make -O Benchmark.hs -o bench -no-recomp
	time ./bench

hugs:
	runhugs -98 QC.hs  

clean:
	rm -f *.o *.hi qc bench *~
