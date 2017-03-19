all: Main.hs
	ghc -threaded --make Main.hs


#all: Main.hs obj/ethernet.o obj/receiver.o obj/udp.o
#		ghc -threaded obj/ethernet.o obj/receiver.o obj/udp.o --make Main.hs

obj/ethernet.o: Sniffer/ethernet.c Sniffer/ethernet.h
		gcc -fPIC -c Sniffer/ethernet.c -o obj/ethernet.o
obj/receiver.o: Sniffer/receiver.c Sniffer/receiver.h
		gcc -fPIC -c Sniffer/receiver.c -o obj/receiver.o

obj/udp.o:udpProxifier/udp.c
		gcc -fPIC -c Proxifier/udp.c -o obj/udp.o


clean:
		find -type f -name "*.dyn_hi" -delete
		find -type f -name "*.dyn_o" -delete
		find -type f -name "*.hi" -delete
		find -type f -name "*.o" -delete

cleanProf:
		find -type f -name "*.p_o" -delete
		make clean

tests:
		make
		./Main
#		rm *.socket


profil: Main.hs   #obj/ethernet.o obj/receiver.o obj/udp.o
	ghc -osuf p_o -prof -auto-all -threaded --make Main.hs  #obj/ethernet.o obj/receiver.o obj/udp.o --make Main.hs
			
