all:
	runhaskell QC.hs
	ghc --make -O -frules-off -funbox-strict-fields QC.hs -o qc -no-recomp
#do without the rules as they break stuff atm
	time ./qc

clean:
	rm -f *.o *.hi qc *~
