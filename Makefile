CC=clang-3.9
CFLAGS=-Wall -Weverything -Wno-unused-parameter -pedantic \
	-fsanitize=address \
	-fsanitize=undefined \
	-fno-omit-frame-pointer


tinyrsh: serverd.c
	$(CC) -o $@ $^ $(CFLAGS)

clean:
	rm -f tinyrsh
