SIMH=/home/smbaker/projects/pi/simh/BIN/scelbi
IGNORE='Bytes loaded\|git commit\|HALT instruction'

test: multitest

iftest:
	cat tests/iftest.input | tr '\n' '\r' > /tmp/scelbi-input
	$(SIMH) tests/start-simh.input > results/result.tmp
	rm -f results/iftest.test
	cat results/result.tmp | grep -v $(IGNORE) > results/iftest.test
	diff -w tests/iftest.good results/iftest.test

multitest:
	cat tests/multitest.input | tr '\n' '\r' > /tmp/scelbi-input 
	$(SIMH) tests/start-simh.input > results/result.tmp
	rm -f results/multitest.test
	cat results/result.tmp | grep -v $(IGNORE) > results/multitest.test
	diff -w tests/multitest.good results/multitest.test

clean:
	rm -rf *~
	rm -f results/*.result results/*.test

