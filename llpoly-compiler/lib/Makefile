CC = clang
CFLAGS = -Wall -Wextra -fPIC

all: libllpoly.so libllpoly.a

libllpoly.so: libllpoly.o
	$(CC) -shared -o $@ $<

libllpoly.a: libllpoly.o
	ar rcs $@ $<

libllpoly.o: libllpoly.c libllpoly.h
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f libllpoly.o libllpoly.so libllpoly.a
