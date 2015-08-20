.DEFAULT: all
.PHONY: all install clean

all:
	stack build

install:	
	stack install

clean:
	stack clean
