ERL          ?= erl
EBIN_DIRS    := $(wildcard lib/*/ebin)
APP          := dissident

all: build_modules erl ebin/$(APP).app
	
build_modules:
	mkdir -p lib/mochiweb/ebin
	mkdir -p lib/webmachine/ebin
	(cd lib/mochiweb ;make) 
	(cd lib/webmachine ;make)

init_modules:
	git submodule init
	git submodule update

update_modules:
	git submodule update
	(cd lib/mochiweb ;git remote update) 
	(cd lib/webmachine ;git remote update)

erl:
	@$(ERL) -pa $(EBIN_DIRS) -noinput +B \
	  -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

clean: 
	@echo "removing:"
	@rm -fv ebin/*.beam ebin/*.app lib/*/ebin/*.app lib/*/ebin/*.beam

ebin/$(APP).app:
	@cp -v src/$(APP).app $@

