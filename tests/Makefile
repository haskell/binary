all:
	runhaskell QC.hs
	ghc --make -O -funbox-strict-fields QC.hs -o qc -no-recomp
	time ./qc
