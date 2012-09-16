ghc := ghc
ghc-flags := 

all: compiled

interpreted:
	runhaskell QC.hs 1000

compiled:
	$(ghc) --make -fhpc -O QC.hs -o qc -fforce-recomp -threaded -rtsopts -i../src -XCPP -package test-framework -package test-framework-quickcheck2 $(ghc-flags)
	rm -f qc.tix
	./qc --maximum-generated-tests=1000 -j2 +RTS -N2

hugs:
	runhugs -98 QC.hs  

clean:
	rm -f *.o *.hi qc *.tix *~

.PHONY: clean
