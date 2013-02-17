FLAGS:=-Wall -O3 -prof -auto-all -caf-all -fforce-recomp -rtsopts

test: tests/test.hs
	ghc $(FLAGS) -odir objs -outputdir objs --make $< -o test

ftest: tests/ftest.hs
	ghc $(FLAGS) -package text -odir objs -outputdir objs --make $< -o ftest

run_test: test
	-time ./test +RTS -p -K100M -RTS tests/irc.dump - > /dev/null

run_test_out: test
	-rm -f out.log; time ./test +RTS -p -K100M -RTS tests/irc.dump out.log

run_ftest: ftest
	-time ./ftest +RTS -p -K100M -RTS tests/irc.dump - > /dev/null

run_ftest_out: ftest
	-rm -f fout.log; time ./ftest +RTS -p -K100M -RTS tests/irc.dump fout.log

clean:
	@rm -rf objs test ftest
