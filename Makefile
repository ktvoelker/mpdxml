
.PHONY: all clean

all: mpdxml

mpdxml: Main.hs
	ghc --make -o mpdxml Main.hs

clean:
	rm *.hi *.o mpdxml

