programs := builder

.PHONY: all
all: $(programs)

builder: Builder.hs
	ghc --make -O2 Builder.hs -o builder -fforce-recomp -i../src

.PHONY: clean
clean:
	rm -f *.o *.hi $(programs)
