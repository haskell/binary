all: compiled

interpreted:
	runhaskell QC.hs 1000

compiled:
	ghc --make -fhpc -O QC.hs -o qc -threaded -package QuickCheck-1.2.0.1 -i../src
	./qc 500

hugs:
	runhugs -98 QC.hs  

clean:
	rm -f *.o *.hi qc *.tix *~

.PHONY: clean
