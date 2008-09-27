all:
	(cd src;$(MAKE))

clean:
	(cd src;$(MAKE) clean)

run-dev:
	erl -pa ./ebin ./deps/*/ebin -boot start_sasl -s reloader -s dissident

run:
	erl -pa ./ebin ./deps/*/ebin -boot start_sasl -s dissident
