.PHONY: all clean test



CC      := gcc
CFLAGS  := $(shell export PKG_CONFIG_PATH=/usr/local/lib64/pkgconfig/; \
	pkg-config --cflags libical) -Wall -fPIC -pthread -pedantic -ggdb 
LFLAGS  := $(shell export PKG_CONFIG_PATH=/usr/local/lib64/pkgconfig/; \
	pkg-config --libs libical)
C_FILES := $(wildcard src/*.c)
O_FILES := $(addprefix obj/,$(notdir $(C_FILES:.c=.o)))

obj/%.o: src/%.c
	$(CC) $(CFLAGS) -c -o $@ $<

main : $(O_FILES)
	$(CC) -o $@ $^ $(LIBS)

all : main

# TODO figure out how to auto compile test files
# TODO Also figure out how to put shader files in own directory

clean:
	-rm obj/*.o
	-rm test/*.o
