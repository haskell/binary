all: interpreted compiled
     
interpreted:
	runhaskell QC.hs 2000

compiled:
	ghc --make -O QC.hs -o qc -no-recomp
	time ./qc 1000

hugs:
	runhugs -98 QC.hs  

clean:
	rm -f *.o *.hi qc *~
