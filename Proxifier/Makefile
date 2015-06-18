
all: connect.c
	gcc -Wall -fPIC -DPIC -c connect.c
	ld -shared -o connect.so connect.o -ldl
