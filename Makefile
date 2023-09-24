ASL=/usr/local/bin/asl
P2BIN=/usr/local/bin/p2bin
P2HEX=/usr/local/bin/p2hex
INC=/home/smbaker/projects/pi/h8/h8-8008/as/include
INCDEPS=lib/*.inc *.inc

SIMH=/home/smbaker/projects/pi/simh/BIN/scelbi
IGNORE='Bytes loaded\|git commit\|HALT instruction'

all: out/forth-16450.bin out/forth-scelbi.bin

out/forth-16450.bin: $(INCDEPS) forth.asm
	mkdir -p out
	$(ASL) -i $(INC) -cpu 8008 -L forth.asm -o out/forth-16450.p -D ser16450
	$(P2BIN) out/forth-16450.p out/forth-16450.bin

out/forth-scelbi.bin: $(INCDEPS) forth.asm
	mkdir -p out
	$(ASL) -i $(INC) -cpu 8008 -L forth.asm -o out/forth-scelbi.p -D serscel
	$(P2BIN) out/forth-scelbi.p out/forth-scelbi.bin
	cp out/forth-scelbi.bin ~/projects/pi/simh/

test: multitest

iftest:
	mkdir -p results
	cat tests/iftest.input | tr '\n' '\r' > /tmp/scelbi-input
	$(SIMH) tests/start-simh.input > results/result.tmp
	rm -f results/iftest.test
	cat results/result.tmp | grep -v $(IGNORE) > results/iftest.test
	diff -w tests/iftest.good results/iftest.test

multitest:
	mkdir -p results
	cat tests/multitest.input | tr '\n' '\r' > /tmp/scelbi-input 
	$(SIMH) tests/start-simh.input > results/result.tmp
	rm -f results/multitest.test
	cat results/result.tmp | grep -v $(IGNORE) > results/multitest.test
	diff -w tests/multitest.good results/multitest.test

clean:
	rm -rf *~
	rm -f results/*.result results/*.test
	rm -f *.p *.bin *.hex *.lst

