CC = ghc
OÚT = -o bin/jpchan
CFLAGS = -O2


all: 		jpchan clean

jpchan:

	$(CC) $(CFLAGS) $(OUT) \
		src/ChanTypes.hs\
		src/JChanPics.hs


clean:
	rm -f src/*.hi
	rm -f src/*.o