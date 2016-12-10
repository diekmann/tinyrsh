CC=clang-3.9
CFLAGS=-Wall -Weverything -Wno-unused-parameter -pedantic \
	-fsanitize=address \
	-fsanitize=undefined \
	-fno-omit-frame-pointer

OBJ = serverd.o handler.o
HEADER = handler.h

tinyrsh: $(OBJ) $(HEADER)
	$(CC) -o $@ $(CFLAGS) $(OBJ)

%.o: %c $(HEADER)
	$(CC) -c -o $@ $(CFLAGS) $< 


.PHONE: clean
clean:
	rm -f *.o
	rm -f tinyrsh
