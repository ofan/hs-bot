FLAGS:=-Wall -O3 -prof -auto-all -caf-all -fforce-recomp -rtsopts

test: tests/test.hs
	ghc $(FLAGS) -package text -odir objs -outputdir objs --make $< -o test

run_test: test
	-time ./test +RTS -p -K100M -RTS tests/irc.dump - > /dev/null

run_test_out: test
	-rm -f out.log; time ./test +RTS -p -K100M -RTS tests/irc.dump out.log

clean:
	@rm -rf objs test
