FLAGS:=-Wall -O3 -prof -auto-all -caf-all -fforce-recomp -rtsopts
OFLAGS:=-odir objs -outputdir objs

test: tests/test.hs
	ghc $(FLAGS) -package text $(OFLAGS) --make $< -o test

ctest: tests/testConcurrent.hs
	ghc $(FLAGS) $(OFLAGS) -threaded --make $< -o ctest

run_test: test
	-time ./test +RTS -p -K100M -RTS tests/irc.dump - > /dev/null

run_test_out: test
	-rm -f out.log; time ./test +RTS -p -K100M -RTS tests/irc.dump out.log

run_ctest: ctest
	-time ./ctest +RTS -p -K100M -N2 -RTS tests/irc.dump - > /dev/null

run_ctest_out: ctest
	rm -f cout.log; time ./ctest +RTS -p -K100M -N2 -RTS tests/irc.dump cout.log

clean:
	@rm -rf objs test ctest
