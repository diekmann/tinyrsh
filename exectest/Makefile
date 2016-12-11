CC=clang-3.9
CFLAGS=-Wall -Weverything -Wno-unused-parameter -pedantic \
	-fsanitize=address \
	-fsanitize=undefined \
	-fno-omit-frame-pointer

OBJ = exectest.o
HEADER = 

exectest: $(OBJ) $(HEADER)
	$(CC) -o $@ $(CFLAGS) $(OBJ)

%.o: %c $(HEADER)
	$(CC) -c -o $@ $(CFLAGS) $< 


.PHONE: clean
clean:
	rm -f *.o
	rm -f exectest
