.PHONY: all clean test tags

CC      := gcc
CFLAGS  := $(shell export PKG_CONFIG_PATH=/usr/local/lib64/pkgconfig/; \
	pkg-config --cflags libical) -Wall -fPIC -pthread -pedantic -ggdb
LFLAGS  := $(shell export PKG_CONFIG_PATH=/usr/local/lib64/pkgconfig/; \
	pkg-config --libs libical)
C_FILES := $(wildcard src/*.c)
O_FILES := $(addprefix obj/,$(notdir $(C_FILES:.c=.o)))

all : main

obj/ :
	-@mkdir obj

obj/%.o: src/%.c obj/
	$(CC) $(CFLAGS) -c -o $@ $<

main : $(O_FILES)
	$(CC) -o $@ $^ $(LIBS)

tags :
	ctags -R src
	etags -R src

clean:
	-rm obj/*.o
	-rmdir obj
	-rm tags
	-rm TAGS
