all: Main.hs obj/ethernet.o obj/receiver.o obj/udp.o
	ghc -threaded obj/ethernet.o obj/receiver.o obj/udp.o --make Main.hs

obj/ethernet.o: Sniffer/ethernet.c Sniffer/ethernet.h
	gcc -fPIC -c Sniffer/ethernet.c -o obj/ethernet.o
obj/receiver.o: Sniffer/receiver.c Sniffer/receiver.h
	gcc -fPIC -c Sniffer/receiver.c -o obj/receiver.o

obj/udp.o:	Proxifier/udp.c
	gcc -fPIC -c Proxifier/udp.c -o obj/udp.o


clean:
	find -type f -name "*.dyn_hi" -delete
	find -type f -name "*.dyn_o" -delete
	find -type f -name "*.hi" -delete
	find -type f -name "*.o" -delete

tests:
	rm *.socket
	make
	./Main
<<<<<<< HEAD


profil: Main.hs obj/ethernet.o obj/receiver.o obj/udp.o
	ghc -osuf p_o -prof -auto-all -threaded obj/ethernet.o obj/receiver.o obj/udp.o --make Main.hs
=======
	
profil: Main.hs obj/ethernet.o obj/receiver.o obj/udp.o
	ghc -prof -auto-all -threaded obj/ethernet.o obj/receiver.o obj/udp.o --make Main.hs
>>>>>>> 13f03d22ef56f3ee730b07f089231fe0c956a5b4
